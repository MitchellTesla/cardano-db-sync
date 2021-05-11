{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingVia #-}

module Cardano.SMASH.Db.Types where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

import           Cardano.Db

import           Cardano.Api (AsType (..), Hash, deserialiseFromBech32, deserialiseFromRawBytesHex,
                   serialiseToRawBytes)

import           Cardano.Api.Shelley (StakePoolKey)
import           Cardano.Sync.Util
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

instance ToJSON PoolIdent where
    toJSON (PoolIdent poolId) =
        object
            [ "poolId" .= poolId
            ]

instance FromJSON PoolIdent where
    parseJSON =
        withObject "PoolId" $ \o -> do
            poolId <- o .: "poolId"
            case parsePoolIdent poolId of
                Left err -> fail $ toS err
                Right p -> pure p

-- Currently deserializing from safe types, unwrapping and wrapping it up again.
-- The underlying DB representation is HEX.
--
-- pool ids as key hashes and so use the "address hash" size, which is 28 bytes, and hence a hex encoding of that is 2*28 = 56
parsePoolIdent :: Text -> Either Text PoolIdent
parsePoolIdent pool =
    case pBech32OrHexStakePoolId pool of
        Nothing -> Left "Unable to parse pool id. Wrong format."
        Just p -> Right . PoolIdent . decodeUtf8 . Base16.encode $ serialiseToRawBytes p

  where
    -- bech32 pool <<< e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
    -- bech32 <<< pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
    pBech32OrHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
    pBech32OrHexStakePoolId str = pBech32StakePoolId str <|> pHexStakePoolId str

    -- e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
    pHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
    pHexStakePoolId =
      deserialiseFromRawBytesHex (AsHash AsStakePoolKey) . BS.pack . toS

    -- pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
    pBech32StakePoolId :: Text -> Maybe (Hash StakePoolKey)
    pBech32StakePoolId =
      either (const Nothing) Just . deserialiseFromBech32 (AsHash AsStakePoolKey)

instance ToJSON PoolMetaHash where
    toJSON (PoolMetaHash poolHash) =
        object
            [ "poolHash" .= decodeUtf8 (Base16.encode poolHash)
            ]

-- The validation of @PoolMetadataHash@ is a bit more involved and would require
-- an analysis with some bounds on the size.
instance FromJSON PoolMetaHash where
    parseJSON =
        withObject "PoolMetadataHash" $ \o -> do
           poolHash <- eitherToMonadFail . first Text.pack . Base16.decode . encodeUtf8  =<< o .: "poolHash"
           pure $ PoolMetaHash poolHash

instance ToJSON TickerName where
    toJSON (TickerName name) =
        object
            [ "name" .= name
            ]

instance FromJSON TickerName where
    parseJSON =
        withObject "TickerName" $ \o -> do
            name <- o .: "name"
            eitherToMonadFail $ validateTickerName name

-- |Util.
eitherToMonadFail :: MonadFail m => Either Text a -> m a
eitherToMonadFail eta =
    case eta of
        Left err -> fail $ toS err
        Right val-> pure val

-- |The validation for the ticker name we can reuse.
validateTickerName :: Text -> Either Text TickerName
validateTickerName name = do
    let tickerLen = length name
    if tickerLen >= 3 && tickerLen <= 5
        then Right $ TickerName name
        else Left $
              mconcat
                [ "\"ticker\" must have at least 3 and at most 5 "
                , "characters, but it has ", textShow tickerLen, " characters."
                ]

