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
