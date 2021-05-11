{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Db.Schema.Types
  ( Address (..)
  , AddressHash (..)
  , PaymentAddrHash (..)
  , PoolIdent (..)
  , PoolMetaHash (..)
  , PoolMetaHex (..)
  , PoolMetadataRaw (..)
  , PoolUrl (..)
  , TickerName (..)
  , decodePoolMetaHash
  , encodePoolMetaHash
  ) where

import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)

import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           GHC.Generics (Generic)

import           Quiet (Quiet (..))


newtype Address -- Length of 28/56/94 bytes enforced by Postgres.
  = Address { unAddress :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet Address)

newtype AddressHash -- Length (28 bytes) enforced by Postgres
  = AddressHash { unAddressHash :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet AddressHash)

newtype PaymentAddrHash -- Length (32 bytes) enforced by Postgres
  = PaymentAddrHash { unPaymentAddrHash :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet PaymentAddrHash)


-- The pool's Bech32 encoded identifier (the hash of the stake pool operator's vkey).
newtype PoolIdent
  = PoolIdent { unPoolIdent :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolIdent)

-- | The raw binary hash of a stake pool's metadata.
newtype PoolMetaHash
  = PoolMetaHash { unPoolMetaHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHash)

-- Hex encoded version of a PoolMetaHash
newtype PoolMetaHex
  = PoolMetaHex { unPoolMetaHex :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHex)

decodePoolMetaHash :: PoolMetaHex -> Either String PoolMetaHash
decodePoolMetaHash (PoolMetaHex hex) = PoolMetaHash <$> Base16.decode (Text.encodeUtf8 hex)

encodePoolMetaHash :: PoolMetaHash -> PoolMetaHex
encodePoolMetaHash (PoolMetaHash bs) = PoolMetaHex $ Text.decodeUtf8 (Base16.encode bs)

-- | The stake pool metadata. It is JSON format. This type represents it in
-- its raw original form. The hash of this content is the 'PoolMetadataHash'.
newtype PoolMetadataRaw
  = PoolMetadataRaw { unPoolMetadata :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetadataRaw)

-- | The pool url wrapper so we have some additional safety.
newtype PoolUrl
  = PoolUrl { unPoolUrl :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolUrl)

-- | The ticker name wrapper so we have some additional safety.
newtype TickerName
  = TickerName { unTickerName :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet TickerName)

