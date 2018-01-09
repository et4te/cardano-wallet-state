{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Wallet.State.Types
  (
    PersistentState (..)
  , TransientState (..)
  , Key
  , Tip
  , ConstantEntry (..)
  , DynamicEntry (..)
  , IxConstantSet
  , IxDynamicSet
  ) where

import           Universum

import           Data.Data (Data)
import           Data.IxSet.Typed -- (IxSet, Indexable (..), Proxy)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Typeable (Typeable)

--------------------------------------------------------------------------------
-- Persistent Types
--------------------------------------------------------------------------------

type Key = ByteString

-- | Each entry is stored according to the tip that it is synchronised
-- with. The tip is a serialized @HeaderHash@.
type Tip = ByteString

-- For example a balance at tip 0 could be 0 whilst at tip 3 could be
-- 1 whilst at tip 10 could be 2 ADA.

-- | Data which is unrelated to the blockchain.
data ConstantEntry = ConstantEntry !Key !ByteString
  deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''ConstantEntry)

-- | Data which is synchronised to a tip within the blockchain.
data DynamicEntry = DynamicEntry !Key !Tip !ByteString
  deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''DynamicEntry)

--------------------------------------------------------------------------------
-- Typed IxSet
--------------------------------------------------------------------------------

type ConstantIxs = '[Key]

type IxConstantSet = IxSet ConstantIxs ConstantEntry

instance Indexable ConstantIxs ConstantEntry where
  indices = ixList (ixGen (Proxy :: Proxy Key))

-- These are our indices into the stored state, for demonstrative purposes we
-- simply use a label here.
type DynamicIxs = '[Key, Tip]

-- The main IxSet for a generic entry.
type IxDynamicSet = IxSet DynamicIxs DynamicEntry

-- Our entries should be indexed by label.
instance Indexable DynamicIxs DynamicEntry where
  indices = ixList (ixGen (Proxy :: Proxy Key))
                   (ixGen (Proxy :: Proxy Tip))

--------------------------------------------------------------------------------
-- We have two different types of state, only persistent state has a safe copy
-- derivation since we do not store transient state on disk.
--------------------------------------------------------------------------------

-- | State which is persistent (stored on disk)
data PersistentState = PersistentState
  { psConstantSet  :: IxConstantSet
  , psDynamicSet :: IxDynamicSet
  }

$(deriveSafeCopy 0 'base ''PersistentState)

-- | State which is transient (temporarily held in memory)
newtype TransientState = TransientState
  { getTransientState :: Integer }

