{-# LANGUAGE FlexibleContexts #-}

module Wallet.Node
  (
      runWalletNode
  ) where

import           Universum
import           Mockable (Production)
import           Pos.Communication (OutSpecs, WorkerSpec, worker)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (HasConfigurations, NodeParams (..))
import           Pos.Launcher (runRealBasedMode, runNode, bracketNodeResources)
import           Pos.Ssc.Types (SscParams)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Update (updateTriggerWorker)
import           Pos.Util (mconcatPair)
import           Wallet.Http (runHttpServer)
import           Wallet.Mode (MonadWalletMode, WalletContext(..), realModeToWallet)
import           Wallet.State.Acid (initAcidState, synchronise)

----------------------------------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------------------------------

-- In reality this will be a http server which has access to the state as it is updated by
-- the block listener callbacks. For testing I have simply added functions which generate
-- blocks from genesis secrets and checks that the tip of the chain has been advanced. The
-- actual state is expected to be updated in the onApplyBlocks callback of the BListener
-- instance derivation.
httpWorker
  :: (HasCompileInfo, MonadWalletMode m)
  => ([WorkerSpec m], OutSpecs)
httpWorker =
  first pure $ worker mempty $ runHttpServer

----------------------------------------------------------------------------------------------------
-- Node
----------------------------------------------------------------------------------------------------

runWalletNode
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SscParams
    -> NodeParams
    -> Production ()
runWalletNode sscParams nodeParams =
    bracketNodeResources nodeParams sscParams txpGlobalSettings
        initNodeDBs $ \nodeResources -> do
            -- The acid state database is initialised on node start.
            initAcidState $ \acid -> do
                -- First we synchronise with the current state of the blockchain.
                synchronise acid

                runRealBasedMode (walletMode acid) realModeToWallet nodeResources $
                    runNode nodeResources workers
  where
      walletMode acid walletAction = do
          realModeContext <- ask
          let walletContext = WalletContext realModeContext acid
          lift $ runReaderT walletAction walletContext

      workers = mconcatPair
          [ updateTriggerWorker
          , httpWorker
          ]
