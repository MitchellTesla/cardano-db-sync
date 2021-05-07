{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.DBSync.Db.Query
  ( DBFail (..)
  , queryAllPools
  , queryPoolByPoolId
  , queryAllPoolMetadata
  , queryPoolMetadata
  , queryDelistedPool
  , queryAllDelistedPools
  , queryAllReservedTickers
  , queryReservedTicker
  , queryAdminUsers
  , queryPoolMetadataFetchError
  , queryPoolMetadataFetchErrorByTime
  , queryAllRetiredPools
  , queryRetiredPool
  ) where

import           Cardano.Prelude hiding (Meta, from, isJust, isNothing, maybeToEither)

import           Data.Time.Clock (UTCTime)

import           Database.Esqueleto (entityVal)
import           Database.Persist.Sql (SqlBackend, selectList)

import           Cardano.Db
import           Cardano.SMASH.Db.Error
import           Cardano.Sync.Util

-- |Return all pools.
queryAllPools :: ReaderT SqlBackend m [pool]
queryAllPools =
  panic "queryAllPools"
{-
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- |Return pool, that is not RETIRED!
queryPoolByPoolId :: PoolIdentifier -> ReaderT SqlBackend m (Either DBFail pool)
queryPoolByPoolId poolId = do
  panic $ "queryPoolByPoolId: " <> textShow poolId
{-
  res <- select . from $ \(pool :: SqlExpr (Entity Pool)) -> do
            where_ (pool ^. PoolPoolId ==. val poolId
                &&. pool ^. PoolPoolId `notIn` retiredPoolsPoolId)
            pure pool
  pure $ maybeToEither RecordDoesNotExist entityVal (listToMaybe res)
  where
    -- |Subselect that selects all the retired pool ids.
    retiredPoolsPoolId :: SqlExpr (ValueList PoolIdentifier)
    retiredPoolsPoolId =
        subList_select . from $ \(retiredPool :: SqlExpr (Entity RetiredPool)) ->
        return $ retiredPool ^. RetiredPoolPoolId
-}

-- |Return all retired pools.
queryAllPoolMetadata :: ReaderT SqlBackend m [PoolMetadata]
queryAllPoolMetadata =
  panic "queryAllPoolMetadata"
{-
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- | Get the 'Block' associated with the given hash.
-- We use the @PoolIdentifier@ to get the nice error message out.
queryPoolMetadata :: PoolIdentifier -> PoolMetaHash -> ReaderT SqlBackend m (Either DBFail PoolMetadata)
queryPoolMetadata poolId poolMetadataHash' = do
  panic $ "queryPoolMetadata: " <> textShow (poolId, poolMetadataHash')

{-
queryPoolMetadata poolId poolMetadataHash' = do
  res <- select . from $ \ poolMetadata -> do
            where_ (poolMetadata ^. PoolMetadataPoolId ==. val poolId
                &&. poolMetadata ^. PoolMetadataHash ==. val poolMetadataHash')
            pure poolMetadata
  pure $ maybeToEither (DbLookupPoolMetadataHash poolId poolMetadataHash') entityVal (listToMaybe res)
-}
-- |Return all retired pools.
queryAllRetiredPools :: ReaderT SqlBackend m [retiredPool]
queryAllRetiredPools =
  panic "queryAllRetiredPools"

{-
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- |Query retired pools.
queryRetiredPool :: PoolIdentifier -> ReaderT SqlBackend m (Either DBFail retiredPool)
queryRetiredPool poolId =
  panic $ "queryRetiredPool:" <> textShow  poolId
{-
queryRetiredPool poolId = do
  res <- select . from $ \retiredPools -> do
            where_ (retiredPools ^. RetiredPoolPoolId ==. val poolId)
            pure retiredPools
  pure $ maybeToEither RecordDoesNotExist entityVal (listToMaybe res)
-}

-- | Check if the hash is in the table.
queryDelistedPool :: PoolIdentifier -> ReaderT SqlBackend m Bool
queryDelistedPool poolId =
  panic $ "queryDelistedPool: " <> textShow poolId
{-
queryDelistedPool poolId = do
  res <- select . from $ \(pool :: SqlExpr (Entity DelistedPool)) -> do
            where_ (pool ^. DelistedPoolPoolId ==. val poolId)
            pure pool
  pure $ Data.Maybe.isJust (listToMaybe res)
-}

-- |Return all delisted pools.
queryAllDelistedPools :: MonadIO m => ReaderT SqlBackend m [DelistedPool]
queryAllDelistedPools = do
  res <- selectList [] []
  pure $ entityVal <$> res

-- |Return all reserved tickers.
queryAllReservedTickers :: MonadIO m => ReaderT SqlBackend m [ReservedTicker]
queryAllReservedTickers = do
  res <- selectList [] []
  pure $ entityVal <$> res

-- | Check if the ticker is in the table.
queryReservedTicker :: TickerName -> PoolMetaHash -> ReaderT SqlBackend m (Maybe ReservedTicker)
queryReservedTicker reservedTickerName' poolMetadataHash' =
  panic $ "queryReservedTicker: " <> textShow (reservedTickerName', poolMetadataHash')

{-
queryReservedTicker reservedTickerName' poolMetadataHash' = do
  res <- select . from $ \(reservedTicker :: SqlExpr (Entity ReservedTicker)) -> do
            where_ (reservedTicker ^. ReservedTickerName ==. val reservedTickerName'
                &&. reservedTicker ^. ReservedTickerPoolHash ==. val poolMetadataHash')

            limit 1
            pure reservedTicker
  pure $ fmap entityVal (listToMaybe res)
-}

-- | Query all admin users for authentication.
queryAdminUsers :: MonadIO m => ReaderT SqlBackend m [AdminUser]
queryAdminUsers = do
  res <- selectList [] []
  pure $ entityVal <$> res

-- | Query all the errors we have.
queryPoolMetadataFetchError :: Maybe PoolIdentifier -> ReaderT SqlBackend m [PoolMetadataFetchError]
queryPoolMetadataFetchError mPoolId =
  panic $ "queryPoolMetadataFetchError: " <> textShow mPoolId

{-
queryPoolMetadataFetchError Nothing = do
  res <- selectList [] []
  pure $ entityVal <$> res

queryPoolMetadataFetchError (Just poolId) = do
  res <- select . from $ \(poolMetadataFetchError :: SqlExpr (Entity PoolMetadataFetchError)) -> do
            where_ (poolMetadataFetchError ^. PoolMetadataFetchErrorPoolId ==. val poolId)
            pure poolMetadataFetchError
  pure $ fmap entityVal res
-}

-- We currently query the top 10 errors (chronologically) when we don't have the time parameter, but we would ideally
-- want to see the top 10 errors from _different_ pools (group by), using something like:
-- select pool_id, pool_hash, max(retry_count) from pool_metadata_fetch_error group by pool_id, pool_hash;
queryPoolMetadataFetchErrorByTime
    :: PoolIdentifier
    -> Maybe UTCTime
    -> ReaderT SqlBackend m [PoolMetadataFetchError]
queryPoolMetadataFetchErrorByTime poolId _ =
  panic $ "queryPoolMetadataFetchErrorByTime: " <> textShow poolId
{-
queryPoolMetadataFetchErrorByTime poolId Nothing = do
  res <- select . from $ \(poolMetadataFetchError :: SqlExpr (Entity PoolMetadataFetchError)) -> do
            where_ (poolMetadataFetchError ^. PoolMetadataFetchErrorPoolId ==. val poolId)
            orderBy [desc (poolMetadataFetchError ^. PoolMetadataFetchErrorFetchTime)]
            limit 10
            pure poolMetadataFetchError
  pure $ fmap entityVal res

queryPoolMetadataFetchErrorByTime poolId (Just fromTime) = do
  res <- select . from $ \(poolMetadataFetchError :: SqlExpr (Entity PoolMetadataFetchError)) -> do
            where_ (poolMetadataFetchError ^. PoolMetadataFetchErrorPoolId ==. val poolId
                &&. poolMetadataFetchError ^. PoolMetadataFetchErrorFetchTime >=. val fromTime)
            orderBy [desc (poolMetadataFetchError ^. PoolMetadataFetchErrorFetchTime)]
            pure poolMetadataFetchError
  pure $ fmap entityVal res
-}
