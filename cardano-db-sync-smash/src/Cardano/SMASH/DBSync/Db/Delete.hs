{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SMASH.DBSync.Db.Delete
  ( deleteDelistedPool
  , deleteRetiredPool
  , deleteAdminUser
  ) where

import           Cardano.Prelude hiding (Meta)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.Db

import           Cardano.Sync.Util

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: PoolIdent -> ReaderT SqlBackend m Bool
deleteDelistedPool poolId =
  panic $ "deleteDelistedPool " <> textShow poolId

-- | Delete a retired pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteRetiredPool :: PoolIdent -> ReaderT SqlBackend m Bool
deleteRetiredPool poolId =
  panic $ "deleteRetiredPool " <> textShow poolId

deleteAdminUser :: AdminUser -> ReaderT SqlBackend m Bool
deleteAdminUser adminUser = do
  panic $ "deleteAdminUser " <> textShow adminUser
