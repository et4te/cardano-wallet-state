{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Wallet.Mode
  (
    -- * The primary wallet monad.
    MonadWalletMode
    -- * The wallet work mode.
  , WalletMode
    -- * The wallets' implicit context.
  , WalletContext (..)

  , wcPersistentState_L

  , realModeToWallet
  ) where

import           Universum

import           Control.Lens (makeLensesWith, lens)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Morph (hoist)
import           Data.Default (def)
import           Data.Acid (AcidState)
import qualified Data.Text as T

import           Pos.Block.BListener (MonadBListener (..))
import           Pos.Block.Slog (HasSlogContext (..), HasSlogGState (..))
import           Pos.Client.KeyStorage (MonadKeysRead (..), getSecretDefault)
import           Pos.Communication.Limits (HasAdoptedBlockVersionData (..))
import           Pos.Core (HasPrimaryKey (..), HasConfiguration)
import           Pos.DB (MonadDB (..), MonadDBRead (..), MonadGState (..),
                         DBSum (..), NodeDBs)
import           Pos.Generator (BlockGenMode)
import           Pos.GState (HasGStateContext (..), getGStateImplicit)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Reporting (HasReportingContext (..))
import           Pos.Shutdown (HasShutdownContext (..))
import           Pos.Slotting (MonadSlots (..), MonadSlotsData, HasSlottingVar (..))
import           Pos.Ssc (HasSscContext (..))
import           Pos.Txp (MonadTxpLocal (..), MempoolExt, txNormalize,
                          txProcessTransaction, txProcessTransactionNoLock)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.OutboundQueue (formatKnownPeersReader)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Util (HasLens (..), postfixLFields)
import           Pos.WorkMode (RealMode, RealModeContext (..), EmptyMempoolExt)

import           Mockable (Production)

import           System.Wlog (HasLoggerName (..))

import           Wallet.State (TransientState(..), PersistentState(..),
                               acquireTransientState)
import           Wallet.State.Acid (lookupConstant)

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

-- | The pervasive wallet context.
data WalletContext = WalletContext
  { wcRealModeContext :: !(RealModeContext EmptyMempoolExt)
  -- | The block listener updates the transient state everytime a block is
  -- applied / rolled back and reads the persistent state in order to do so.
  -- | The persistent state is updated by the REST API and is read by the
  -- block listener everytime a block update occurs. For informational purposes
  -- it should also be readable by the http worker.
  , wcPersistentState :: !(AcidState PersistentState)
  }

makeLensesWith postfixLFields ''WalletContext

----------------------------------------------------------------------
-- Monadic
----------------------------------------------------------------------

type WalletMode = ReaderT WalletContext Production

class (m ~ WalletMode, HasConfigurations, HasCompileInfo)
  => MonadWalletMode m

instance (HasConfigurations, HasCompileInfo)
  => MonadWalletMode WalletMode

realModeToWallet :: RealMode EmptyMempoolExt a -> WalletMode a
realModeToWallet = withReaderT wcRealModeContext

------------------------------------------------------------------------------
-- Monadic Instances
------------------------------------------------------------------------------

instance HasConfiguration => MonadBListener WalletMode where
  onApplyBlocks blunds = do
    -- Acquire the wallet mode context.
    WalletContext{..} <- ask

    -- Acquire the persistent state used to filter the blocks.
    userAddress <- lookupConstant wcPersistentState "<user_key>"

    putText $ "[BListener] Address = " `T.append` show userAddress
    putText $ "[BListener] Latest block = "

    -- forM blunds $ \(block, _) -> do
    --   putText $ show block

    -- loadBlundsWhile (const True) blunds

    -- Acquire the transient state used to build the next state.
    -- transientState <- acquireTransientState wcTransientState

    -- Filter the blocks according to the persistent state and fold onto the
    -- existing transient state then swap the MVar to hold the updated fold.
    -- let nextCount = foldl' (countOutputs persistentState) transientState blunds
    -- swapMVar wcTransientState (TransientState nextCount)

    pure mempty

  onRollbackBlocks _ = realModeToWallet $ do
    putText "Called onRollbackBlocks ..."
    pure mempty

instance HasConfiguration => MonadDBRead WalletMode where
  dbGet = realModeToWallet ... dbGet
  dbIterSource tag p = hoist (hoist realModeToWallet) (dbIterSource tag p)
  dbGetSerBlock = realModeToWallet ... dbGetSerBlock
  dbGetSerUndo = realModeToWallet ... dbGetSerUndo

instance HasConfiguration => MonadDB WalletMode where
  dbPut = realModeToWallet ... dbPut
  dbWriteBatch = realModeToWallet ... dbWriteBatch
  dbDelete = realModeToWallet ... dbDelete
  dbPutSerBlund = realModeToWallet ... dbPutSerBlund

instance HasConfiguration => MonadGState WalletMode where
  gsAdoptedBVData = realModeToWallet ... gsAdoptedBVData

instance HasConfiguration => HasAdoptedBlockVersionData WalletMode where
  adoptedBVData = gsAdoptedBVData

instance MonadKnownPeers WalletMode where
  updatePeersBucket = realModeToWallet ... updatePeersBucket

instance MonadFormatPeers WalletMode where
  formatKnownPeers = formatKnownPeersReader (rmcOutboundQ . wcRealModeContext)

instance MonadKeysRead WalletMode where
  getSecret = getSecretDefault

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx WalletMode)
  => MonadSlots ctx WalletMode where
  getCurrentSlot = realModeToWallet getCurrentSlot
  getCurrentSlotBlocking = realModeToWallet getCurrentSlotBlocking
  getCurrentSlotInaccurate = realModeToWallet getCurrentSlotInaccurate
  currentTimeSlotting = realModeToWallet currentTimeSlotting

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo)
  => MonadTxpLocal WalletMode where
  txpNormalize = withReaderT wcRealModeContext txNormalize
  txpProcessTx = withReaderT wcRealModeContext . txProcessTransaction

instance (HasConfigurations)
  => MonadTxpLocal (BlockGenMode EmptyMempoolExt WalletMode) where
  txpNormalize = withCompileInfo def $ txNormalize
  txpProcessTx = withCompileInfo def $ txProcessTransactionNoLock

----------------------------------------------------------------------
-- Instance Derivations
----------------------------------------------------------------------

type instance MempoolExt WalletMode = EmptyMempoolExt

instance HasLens DBSum WalletContext DBSum where
  lensOf =
    let getter ctx              = RealDB (ctx ^. (lensOf @NodeDBs))
        setter ctx (RealDB db') = ctx & (lensOf @NodeDBs) .~ db'
        setter _   (PureDB _)   =
          error "Wallet tried to set pure db instead of node db."
    in lens getter setter

instance HasGStateContext WalletContext where
  gStateContext = getGStateImplicit

instance HasNodeType WalletContext where
  getNodeType _ = NodeEdge

instance HasPrimaryKey WalletContext where
  primaryKey = wcRealModeContext_L . primaryKey

instance HasReportingContext WalletContext where
  reportingContext = wcRealModeContext_L . reportingContext

instance HasUserSecret WalletContext where
  userSecret = wcRealModeContext_L . userSecret

instance HasShutdownContext WalletContext where
  shutdownContext = wcRealModeContext_L . shutdownContext

instance HasSlottingVar WalletContext where
  slottingTimestamp = wcRealModeContext_L . slottingTimestamp

  slottingVar = wcRealModeContext_L . slottingVar

instance HasSlogGState WalletContext where
  slogGState = wcRealModeContext_L . slogGState

instance HasSlogContext WalletContext where
  slogContext = wcRealModeContext_L . slogContext

instance HasSscContext WalletContext where
  sscContext = wcRealModeContext_L . sscContext

instance {-# OVERLAPPABLE #-}
  HasLens tag (RealModeContext EmptyMempoolExt) r =>
      HasLens tag WalletContext r
  where
      lensOf = wcRealModeContext_L . lensOf @tag

instance {-# OVERLAPPING #-} HasLoggerName WalletMode where
  modifyLoggerName f action = do
    walletContext <- ask
    let walletToRealMode :: WalletMode a -> RealMode EmptyMempoolExt a
        walletToRealMode =
          withReaderT (\realContext -> set wcRealModeContext_L realContext walletContext)
    realModeToWallet $ modifyLoggerName f $ walletToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog WalletMode where
  jsonLog = realModeToWallet ... jsonLog
