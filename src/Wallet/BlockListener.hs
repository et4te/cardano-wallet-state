module Wallet.BlockListener
  (
    onApplyBlocks
  , onRollbackBlocks
  ) where

import           Universum
import           Pos.Util.Chrono (NewestFirst (..), OldestFirst (..))

onApplyBlocks blunds = do
  let blocks = map (\(b,_) -> b) $ getOldestFirst blunds

  putText $ show blocks

  pure mempty

onRollbackBlocks blunds = do
  let blocks = map (\(b,_) -> b) $ getNewestFirst blunds

  putText $ show blocks

  pure mempty
