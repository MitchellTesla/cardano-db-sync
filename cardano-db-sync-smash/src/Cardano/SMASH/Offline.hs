{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Offline
  ( fetchInsertNewPoolMetadata
  , runOfflineFetchThread
  ) where

import           Cardano.Prelude hiding (from, groupBy, on, retry)

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Control.Monad.Trans.Except.Extra (handleExceptT, hoistEither, left)

import           Cardano.SMASH.DB (DataLayer (..), postgresqlDataLayer)
import           Cardano.SMASH.FetchQueue
import           Cardano.SMASH.Types (FetchError (..), pomTicker)

import           Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import           Data.Time (UTCTime)
-- import qualified Data.Time as Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Clock.POSIX as Time

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
-- import qualified Cardano.SMASH.DBSync.Db.Schema as DB
import           Cardano.Db
import qualified Cardano.Db as DB

import           Cardano.Sync.Util

import           Database.Esqueleto (Entity (..), InnerJoin (..), SqlExpr, Value (..), ValueList,
                   desc, from, groupBy, in_, just, max_, notExists, on, orderBy, select,
                   subList_select, where_, (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as Http

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

-- This is what we call from the actual block-syncing code.
fetchInsertNewPoolMetadata
    :: Trace IO Text -> DataLayer -> DB.PoolMetadataRefId -> PoolIdent -> Shelley.PoolMetadata
    -> IO ()
fetchInsertNewPoolMetadata tracer dataLayer refId poolIdent md  =
    mkPoolFetchRetry >>= fetchInsertNewPoolMetadataOld tracer dataLayer
  where
    mkPoolFetchRetry :: IO PoolFetchRetry
    mkPoolFetchRetry = do
      now <- Time.getPOSIXTime
      pure $ PoolFetchRetry
              { pfrReferenceId = refId
              , pfrPoolIdent = poolIdent
              , pfrPoolUrl = PoolUrl . Shelley.urlToText $ Shelley._poolMDUrl md
              , pfrPoolMDHash = PoolMetaHash $ Shelley._poolMDHash md
              , pfrRetry = newRetry now
              }

-- Please note that it is possible that a user submits the same pool hash
-- several times which will result in the fetch being done for each of the entry.
-- We do this in order to make sure that the metadata wasn't fixed in the meantime.
fetchInsertNewPoolMetadataOld :: Trace IO Text -> DataLayer -> PoolFetchRetry -> IO ()
fetchInsertNewPoolMetadataOld tracer dataLayer pfr = do
    logInfo tracer $ showRetryTimes (pfrRetry pfr)

    res <- runExceptT $ fetchInsert tracer dataLayer pfr

    -- In the case all went well, we do nothing, but if something went wrong
    -- we log that and add the error to the database.
    case res of
        Right () ->
            logInfo tracer "Pool metadata fetch successful!"

        Left err -> do
            let fetchError = renderFetchError err
            logWarning tracer $ "Pool metadata fetch failed: " <> fetchError

            let addFetchError = dlAddFetchError dataLayer

            -- Here we add the fetch error. The fetch time is always constant.
            _pmfeIdE <- addFetchError $
                          PoolOfflineFetchError
                            (posixSecondsToUTCTime . fetchTime $ pfrRetry pfr)
                            (panic $ "fetchInsertNewPoolMetadataOld: " <> textShow (pfrPoolIdent pfr))
                            -- poolHash
                            (pfrReferenceId pfr)
                            fetchError
                            (retryCount (pfrRetry pfr) + 1)
            pure ()


-- |We pass in the @PoolId@ so we can know from which pool the error occured.
fetchInsert :: Trace IO Text -> DataLayer -> PoolFetchRetry -> ExceptT FetchError IO ()
fetchInsert tracer dataLayer pfr = do
    -- This is a bit bad to do each time, but good enough for now.
    manager <- liftIO $ Http.newManager tlsManagerSettings

    let poolMetadataURL = pfrPoolUrl pfr

    liftIO . logInfo tracer $ "Request URL: " <> unPoolUrl poolMetadataURL

    -- This is a weird Error.
    request <- handleExceptT (\(err :: HttpException) -> FEUrlParseFail (pfrPoolIdent pfr) poolMetadataURL (textShow err))
                $ Http.parseRequest (Text.unpack $ unPoolUrl poolMetadataURL)

    (respBS, status) <- httpGet512BytesMax (pfrPoolIdent pfr) poolMetadataURL request manager

    when (Http.statusCode status /= 200) .
      left $ FEHttpResponse (pfrPoolIdent pfr) poolMetadataURL (Http.statusCode status)

    liftIO . logInfo tracer $ "Response: " <> show (Http.statusCode status)

    decodedMetadata <-
        case eitherDecode' (LBS.fromStrict respBS) of
            Left err -> left $ FEJsonDecodeFail (pfrPoolIdent pfr) poolMetadataURL (Text.pack err)
            Right res -> pure res

    -- Let's check the hash
    let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
        expectedHash = unPoolMetaHash (pfrPoolMDHash pfr)

    if PoolMetaHash metadataHash /= pfrPoolMDHash pfr
      then left $ FEHashMismatch (pfrPoolIdent pfr) poolMetadataURL (renderByteArray expectedHash) (renderByteArray metadataHash)
      else liftIO . logInfo tracer $ "Inserting pool data with hash: " <> renderByteArray expectedHash

    let addPoolMetadata = dlAddPoolMetadata dataLayer

    _ <- liftIO $
            addPoolMetadata
                (Just $ pfrReferenceId pfr)
                (pfrPoolIdent pfr)
                (pfrPoolMDHash pfr)
                (PoolMetadataRaw $ decodeUtf8 respBS)
                (pomTicker decodedMetadata)

    liftIO $ logInfo tracer (decodeUtf8 respBS)

-- This is run as a new thread with the fetchLoop looping forever.
runOfflineFetchThread :: SqlBackend -> Trace IO Text -> IO ()
runOfflineFetchThread backend trce = do
    liftIO $ logInfo trce "Runing Offline fetch thread"
    let dataLayer = postgresqlDataLayer backend trce
    fetchLoop trce backend dataLayer FetchLoopForever

---------------------------------------------------------------------------------------------------

data FetchLoopType
    = FetchLoopForever
    | FetchLoopOnce
    deriving (Eq, Show)

fetchLoop :: Trace IO Text -> SqlBackend -> DataLayer -> FetchLoopType -> IO ()
fetchLoop trce sqlBackend dataLayer fetchLoopType =
    case fetchLoopType of
      FetchLoopForever -> forever work
      FetchLoopOnce -> work
  where
    work :: IO ()
    work = do
      -- A interval pause so we don't do too much work on this thread.
      liftIO $ threadDelay 60_000_000 -- 60 seconds

      now <- liftIO Time.getPOSIXTime

      -- Fetch all pools that have not been run with success.
      -- This has to be stateful in order to count.
      pools <- liftIO $ runDbIohkLogging sqlBackend trce queryPoolFetchRetry

      -- Filter for figuring out if it's time for a retry.
      let isRunnable :: PoolFetchRetry -> Bool
          isRunnable pfr = retryTime (pfrRetry pfr) < now

      let runnablePools = filter isRunnable pools

      liftIO $ logInfo trce $
        mconcat
          [ " ***************************** ", "Pools with errors: total "
          , textShow (length pools), ", runnable ", textShow (length runnablePools), "."
          ]

      -- We actually run the fetch again.
      liftIO $ forM_ runnablePools $ \ pool ->
          fetchInsertNewPoolMetadataOld trce dataLayer pool
      pure ()

httpGet512BytesMax
    :: PoolIdent -> PoolUrl -> Http.Request -> Http.Manager
    -> ExceptT FetchError IO (ByteString, Http.Status)
httpGet512BytesMax pool url request manager = do
    res <- handleExceptT (convertHttpException pool url) httpGet
    hoistEither res
  where
    httpGet :: IO (Either FetchError (ByteString, Http.Status))
    httpGet =
      Http.withResponse request manager $ \responseBR -> do
        -- We read the first chunk that should contain all the bytes from the reponse.
        responseBSFirstChunk <- Http.brReadSome (Http.responseBody responseBR) 512
        -- If there are more bytes in the second chunk, we don't go any further since that
        -- violates the size constraint.
        responseBSSecondChunk <- Http.brReadSome (Http.responseBody responseBR) 1
        if LBS.null responseBSSecondChunk
          then pure $ Right (LBS.toStrict responseBSFirstChunk, Http.responseStatus responseBR)
          else pure $ Left (FEDataTooLong pool url)

convertHttpException :: PoolIdent -> PoolUrl -> HttpException -> FetchError
convertHttpException pool url he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout -> FETimeout pool url "Response"
        Http.ConnectionTimeout -> FETimeout pool url "Connection"
        Http.ConnectionFailure {} -> FEConnectionFailure pool url
        other -> FEHttpException pool url (show other)
    InvalidUrlException urlx err -> FEUrlParseFail pool (PoolUrl $ Text.pack urlx) (Text.pack err)

-- select * from pool_metadata_fetch_error pmfr
--   where pmfr.id in (select max(id) from pool_metadata_fetch_error group by pool_id, pool_hash)
--   and not exists (select * from pool_metadata where pmr_id = pmfr.pmr_id);

-- Get a list of the pools for which there is a PoolMetadataReference entry but there is
-- no PoolMetadata entry.
-- This is a bit questionable because it assumes that the autogenerated 'id' primary key
-- is a reliable proxy for time, ie higher 'id' was added later in time.
queryPoolFetchRetry :: MonadIO m => ReaderT SqlBackend m [PoolFetchRetry]
queryPoolFetchRetry = do
    pmfe <- select . from $ \(pmfe `InnerJoin` pmr `InnerJoin` ph) -> do
                on (ph ^. DB.PoolHashId ==. pmr ^. DB.PoolMetadataRefPoolId)
                on (pmfe ^. DB.PoolOfflineFetchErrorPmrId ==. pmr ^. DB.PoolMetadataRefId)
                where_ (just (pmfe ^. DB.PoolOfflineFetchErrorId) `in_` latestReferences)
                where_ (notExists . from $ \pod -> where_ (pod ^. DB.PoolOffilineDataPmrId ==. just (pmfe ^. DB.PoolOfflineFetchErrorPmrId)))
                orderBy [desc (pmfe ^. DB.PoolOfflineFetchErrorFetchTime)]
                pure
                    ( pmfe ^. DB.PoolOfflineFetchErrorFetchTime
                    , pmfe ^. DB.PoolOfflineFetchErrorPmrId
                    , ph ^. DB.PoolHashView
                    , pmr ^. DB.PoolMetadataRefUrl
                    , pmr ^. DB.PoolMetadataRefHash
                    , pmfe ^. DB.PoolOfflineFetchErrorRetryCount
                    )

    pure $ map (convert . unValue6) pmfe
  where
    latestReferences :: SqlExpr (ValueList (Maybe DB.PoolOfflineFetchErrorId))
    latestReferences =
      subList_select . from $ \(pmfe :: SqlExpr (Entity PoolOfflineFetchError)) -> do
        groupBy (pmfe ^. DB.PoolOfflineFetchErrorPoolId, pmfe ^. DB.PoolOfflineFetchErrorPoolId)
        pure $ max_ (pmfe ^. DB.PoolOfflineFetchErrorId)

    convert :: (UTCTime, PoolMetadataRefId, Text, Text, ByteString, Word) -> PoolFetchRetry
    convert (fetchTime', poolMetadataReferenceId, poolIdent, poolUrl, poolMetadataHash', existingRetryCount) =
        let retry = retryAgain (Time.utcTimeToPOSIXSeconds fetchTime') existingRetryCount
        in
            PoolFetchRetry
              { pfrReferenceId = poolMetadataReferenceId
              , pfrPoolIdent = PoolIdent poolIdent
              , pfrPoolUrl = PoolUrl poolUrl
              , pfrPoolMDHash = PoolMetaHash poolMetadataHash'
              , pfrRetry = retry
              }

    -- Util
    unValue6 :: (Value a, Value b, Value c, Value d, Value e, Value f) -> (a, b, c, d, e, f)
    unValue6 (a, b, c, d, e, f) = (unValue a, unValue b, unValue c, unValue d, unValue e, unValue f)

renderFetchError :: FetchError -> Text
renderFetchError fe =
  case fe of
    FEHashMismatch (PoolIdent pool) (PoolUrl url) xpt act ->
      mconcat
        [ "Hash mismatch from ", pool, " when fetching metadata from ", url
        , ". Expected ", xpt, " but got ", act, "."
        ]
    FEDataTooLong (PoolIdent pool) (PoolUrl url) ->
      mconcat
        [ "Offline pool data from ", pool, " when fetching metadata from ", url
        , " exceeded 512 bytes."
        ]
    FEUrlParseFail (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "URL parse error from ", pool, " when fetching metadata from ", url
        , "' resulted in : ", err
        ]
    FEJsonDecodeFail (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "JSON decode error from ", pool, " when fetching metadata from ", url
        , " resulted in : ", err
        ]
    FEHttpException (PoolIdent pool) (PoolUrl url) err ->
      mconcat
        [ "HTTP Exception from ", pool, " when fetching metadata from ", url
        , " resulted in : ", err
        ]
    FEHttpResponse (PoolIdent pool) (PoolUrl url) sc ->
      mconcat
        [ "HTTP Response from ", pool, " when fetching metadata from ", url
        , " resulted in : ", textShow sc
        ]
    FETimeout (PoolIdent pool) (PoolUrl url) ctx ->
      mconcat
        [ ctx, " timeout from ", pool, " when fetching metadata from "
        , url, "."
        ]
    FEConnectionFailure (PoolIdent pool) (PoolUrl url) ->
      mconcat
        [ "Connection failure from pool ", pool, " when fetching metadata from "
        , url, "'."
        ]
    FEIOException err -> "IO Exception: " <> err

