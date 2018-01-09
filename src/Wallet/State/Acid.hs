{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Wallet.State.Acid
  (
    getDynamicSet
  , insertDynamic
  , lookupDynamic
  , insertConstant
  , lookupConstant
  , openPersistentState
  ) where

import           Universum hiding (empty)

import           Data.Acid (AcidState, Update, Query, update, query, makeAcidic,
                            openLocalState)
import           Data.IxSet.Typed ((@=), insert, getOne, empty)

import           Pos.Binary.Class (Bi, serialize', unsafeDeserialize')
import           Pos.Core.Common (HeaderHash)

import           Wallet.State.Types (Key, Tip, IxConstantSet, ConstantEntry (..),
                                     IxDynamicSet, DynamicEntry (..),
                                     PersistentState (..))

--------------------------------------------------------------------------------
-- ACID DB Operations
--------------------------------------------------------------------------------

-- | Inserts a new dynamic piece of data.
acidInsertDynamic :: Key -> Tip -> ByteString -> Update PersistentState ()
acidInsertDynamic key tip value = do
  PersistentState simple dynamic <- get
  put $ PersistentState simple (insert (DynamicEntry key tip value) dynamic)

acidLookupDynamic :: Key -> Query PersistentState (Maybe DynamicEntry)
acidLookupDynamic key = do
  PersistentState _ dynamic <- ask
  return $ getOne (dynamic @= key)

-- | Gets all dynamic data.
acidGetPersistentState :: Query PersistentState PersistentState -- IxDynamicSet
acidGetPersistentState = ask
  --PersistentState _ dynamic <- ask
  --return dynamic

-- | Inserts a new constant piece of data.
acidInsertConstant :: Key -> ByteString -> Update PersistentState ()
acidInsertConstant key value = do
  PersistentState simple dynamic <- get
  put $ PersistentState (insert (ConstantEntry key value) simple) dynamic

-- | Defines a lookup of a particular entry on the AcidState database.
acidLookupConstant :: Key -> Query PersistentState (Maybe ConstantEntry)
acidLookupConstant key = do
  PersistentState simple _ <- ask
  return $ getOne (simple @= key)

$(makeAcidic ''PersistentState
  [ 'acidGetPersistentState
  , 'acidInsertDynamic
  , 'acidLookupDynamic
  , 'acidInsertConstant
  , 'acidLookupConstant
  ])

--------------------------------------------------------------------------------
-- Data API
--------------------------------------------------------------------------------

-- | For performance reasons the last tip is stored in acid-state. This tip is
-- invalidated (by setting it to Nothing) anytime a new tracked entity is added.
-- This is necessary since previous blockchain state could affect the current
-- state.
-- lookupTip :: MonadIO m => AcidState PersistentState -> m (Maybe HeaderHash)
-- lookupTip acid = do
--   tip' <- liftIO $ query acid (AcidLookupTip)
--   case tip' of
--     Just tip ->
--       return $ Just $ unsafeDeserialize' tip
--     Nothing ->
--       return Nothing

openPersistentState :: MonadIO m => m (AcidState PersistentState)
openPersistentState = do
  let simple = empty :: IxConstantSet
  let dynamic = empty :: IxDynamicSet
  liftIO $ openLocalState (PersistentState simple dynamic)

insertConstant :: (Bi b, MonadIO m) => AcidState PersistentState -> Key -> b -> m ()
insertConstant acid key value' = do
  let value = serialize' value'
  _ <- liftIO $ update acid (AcidInsertConstant key value)
  return ()

lookupConstant :: MonadIO m => AcidState PersistentState -> Key -> m (Maybe ConstantEntry)
lookupConstant acid key =
  liftIO $ query acid (AcidLookupConstant key)

getDynamicSet :: MonadIO m => AcidState PersistentState -> m IxDynamicSet
getDynamicSet acid = do
  PersistentState _ dynamic <- liftIO $ query acid AcidGetPersistentState
  return dynamic

insertDynamic :: (Bi b, MonadIO m) => AcidState PersistentState -> Key -> b -> b -> m ()
insertDynamic acid key tip' value' = do
  let tip = serialize' tip'
  let value = serialize' value'
  _ <- liftIO $ update acid (AcidInsertDynamic key tip value)
  return ()

lookupDynamic :: MonadIO m => AcidState PersistentState -> Key -> m (Maybe DynamicEntry)
lookupDynamic acid key =
  liftIO $ query acid (AcidLookupDynamic key)


