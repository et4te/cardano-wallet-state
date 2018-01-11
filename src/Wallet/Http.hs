module Wallet.Http
  (
    runHttpServer
  ) where

import           Universum



-- | Start a http server ...
runHttpServer :: Monad m => t -> m ()
runHttpServer _sendActions = do
  return ()
