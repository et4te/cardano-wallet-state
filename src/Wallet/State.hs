module Wallet.State
  (
  -- *
    acquireTransientState

  , module Wallet.State.Types
  ) where

import           Universum

import           Wallet.State.Types (PersistentState (..), TransientState (..),
                                     ConstantEntry (..), DynamicEntry (..))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

acquireState :: MonadIO m => MVar a -> (a -> b) -> m b
acquireState mvar getter = do
  state' <- takeMVar mvar
  putMVar mvar state'
  pure $ getter state'

acquireTransientState :: MonadIO m => MVar TransientState -> m Integer
acquireTransientState mvar =
  acquireState mvar getTransientState
