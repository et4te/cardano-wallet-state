{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLists            #-}

module Main where

import           Universum hiding (bracket)

import qualified Control.Foldl as L
import           Control.Lens (makeLensesWith, lens)
import           Control.Monad.Random.Strict (evalRandT)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Morph (hoist)
import           Data.Acid (AcidState, closeAcidState)
import           Data.Default (def)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Formatting (sformat, shown, (%))
import           Mockable (Production, bracket, runProduction, currentTime)
import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Slog (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.BListener (MonadBListener(..))
import           Pos.Block.Logic (normalizeMempool)
import           Pos.Block.Logic.VAR (applyBlocks, rollbackBlocks,
                                      applyWithRollback, verifyAndApplyBlocks)
import           Pos.Block.Types (Blund)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Client.Txp (MonadBalances (..), getOwnUtxo, getBalanceFromUtxo)
import           Pos.Client.KeyStorage (getSecretKeysPlain, MonadKeysRead(..), getSecretDefault)
import           Pos.Communication (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Core (Address, Coin, HasConfiguration, HeaderHash, headerHash,
                           prevBlockL, GenesisData(..), genesisData, Timestamp(..),
                           isRedeemAddress, HasPrimaryKey (..), BlockCount,
                           gdBootStakeholders, genesisSecretKeys, makeAddress, AddrSpendingData (..),
                           AddrStakeDistribution (..), AddrAttributes (..)
                          )
import           Pos.Core.Block (Block, MainBlockchain)
import           Pos.Core.Block.Blockchain (GenericBlock (..))
import           Pos.Core.Block.Main (Body (..))
import           Pos.Core.Txp (Tx (..))
import           Pos.Crypto (SecretKey(..), ShouldCheckPassphrase (..), PassPhrase (..),
                             deriveHDPassphrase, deriveHDSecretKey, packHDAddressAttr,
                             encToPublic, encToSecret, firstHardened, mkEncSecretUnsafe)
import           Pos.DB (getBlock, DBSum (..), MonadDB (..), MonadGState (..), NodeDBs, dbGetDefault, dbIterSourceDefault)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Block (getUndo)
import           Pos.DB.Block.Load (loadBlundsWhile, loadBlocksWhile)
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (MonadDBRead(..))
import           Pos.Generator (BlockGenMode)
import           Pos.Generator.Block (TxGenParams (..), BlockGenParams (..), genBlocks,
                                      tgpTxCountRange)
import           Pos.Generator.BlockEvent.DSL (BlockApplyResult (..), emitBlockApply, pathSequence)
import           Pos.GState (getTip, getGStateImplicit, GStateContext, HasGStateContext(..))
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Launcher (HasConfigurations, NodeParams (..), LoggingParams(..), withConfigurations)
import           Pos.Launcher (runRealBasedMode, runNode, loggerBracket, bracketNodeResources)
import           Pos.Merkle (MerkleTree(..))
import           Pos.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Reporting (HasReportingContext (..))
import           Pos.Shutdown (HasShutdownContext (..))
import           Pos.Slotting (MonadSlots(..), MonadSlotsData(..), HasSlottingVar(..))
import           Pos.Ssc.Types (HasSscContext (..), SscParams)
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Txp (MempoolExt, MonadTxpMem, MonadTxpLocal (..), txpGlobalSettings,
                          txNormalize, txProcessTransaction, txProcessTransactionNoLock,
                          GenesisUtxo(..), genesisUtxo, utxoToAddressCoinPairs,
                          TxpGlobalSettings, TxPayload(..), TxOut(..), TxIn(..))
import           Pos.Update (updateTriggerWorker)
import           Pos.Util.Chrono (OldestFirst (..), NewestFirst (..),
                                  nonEmptyOldestFirst, nonEmptyNewestFirst)
import           Pos.Util.CompileInfo (HasCompileInfo(..), retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (usVss, HasUserSecret(..))
import           Pos.Util (HasLens (..), postfixLFields, mconcatPair, lensOf')
import           Pos.WorkMode (MinWorkMode, EmptyMempoolExt, RealMode, RealModeContext(..))
import           System.Wlog (HasLoggerName (..), LoggerName, logDebug, logInfo)
import           System.Random (mkStdGen, randomIO)
import           Pos.Util.OutboundQueue
  (updatePeersBucketReader, formatKnownPeersReader)

import           Wallet.Mode (MonadWalletMode, WalletMode, WalletContext(..),
                              realModeToWallet)
import           Wallet.State (TransientState(..), PersistentState(..), ConstantEntry (..))
import           Wallet.State (acquireTransientState)
import           Wallet.State.Acid (openPersistentState, insertConstant, lookupConstant,
                                    getDynamicSet)

------------------------------------------------------------------------------

generateBlunds
  :: MonadWalletMode m
  => [SecretKey]  -- ^ The secret keys used to generate txs.
  -> BlockCount   -- ^ The number of blocks to generate.
  -> Int          -- ^ The seed used to generate blocks.
  -> (Word, Word) -- ^ The range of tx outputs to generate.
  -> Word         -- ^ The maximum number of tx outputs to generate.
  -> m [Blund]
generateBlunds secretKeys blockCount seed txCountRange maxOutputs =
  withStateLock HighPriority "wallet-backend" $ \_ -> do
    txpSettings <- view (lensOf' @TxpGlobalSettings)
    let blockGenParams =
          BlockGenParams
              { _bgpSecrets           = mkAllSecretsSimple secretKeys
              , _bgpGenStakeholders   = gdBootStakeholders genesisData
              , _bgpBlockCount        = blockCount
              , _bgpTxGenParams       = TxGenParams txCountRange maxOutputs
              , _bgpInplaceDB         = True
              , _bgpSkipNoKey         = False
              , _bgpTxpGlobalSettings = txpSettings
              }
    withCompileInfo def $ do
      evalRandT (genBlocks blockGenParams maybeToList) (mkStdGen seed)

prevBlock :: HasConfiguration => Block -> HeaderHash
prevBlock = view prevBlockL

traverseChain
  :: (HasCompileInfo, MonadWalletMode m)
  => HeaderHash
  -> Integer
  -> m ()
traverseChain tip i = do
  block' <- getBlock tip
  case block' of
    Nothing -> do
      putText ("Done! " `T.append` show i)
      return ()
    Just block -> do
      undo <- getUndo $ headerHash block
      traverseChain (prevBlock block) (i+1)

secretKeyToAddress (SecretKey xprv) passphrase = do
  encryptedSecretKey <- mkEncSecretUnsafe passphrase xprv
  pure $ encryptedSecretKeyToAddress encryptedSecretKey Nothing

encryptedSecretKeyToAddress encryptedSecretKey hdAddressPayload =
  let publicKey = PubKeyASD $ encToPublic encryptedSecretKey in
  let addressAttributes = AddrAttributes hdAddressPayload BootstrapEraDistr in
    makeAddress publicKey addressAttributes

-- deriveTestHD :: SecretKey -> PassPhrase -> 
deriveTestHD rootKey passphrase = do
    let hdPassphrase = deriveHDPassphrase $ encToPublic rootKey
    let hdAddressPayload = packHDAddressAttr hdPassphrase [a', b', c']
    let (Just encryptedSecretKey) = deriveHDSecretKey checkPassphrase passphrase rootKey c'
    -- FIXME The address returned here is not correct since it contains no HD attribute
    -- but this is necessary for testing since block generation doesn't generate attributes.
    -- Also for testing we use plain secret keys since this is what is used in block
    -- generation.
    let secretKey = encToSecret encryptedSecretKey
    address <- secretKeyToAddress secretKey passphrase
    -- let address = encryptedSecretKeyToAddress encryptedSecretKey (Just hdAddressPayload)
    pure (secretKey, address)
  where
    a' = firstHardened
    b' = firstHardened
    c' = firstHardened

    checkPassphrase = ShouldCheckPassphrase True

httpWorker
  :: (HasCompileInfo, MonadWalletMode m)
  => ([WorkerSpec m], OutSpecs)
httpWorker = first pure $ worker mempty $ runHttpServer
  where
    runHttpServer sendActions = do
      WalletContext{..} <- ask

      tip <- getTip
      putText $ "[Init] Latest block = " `T.append` show tip

      -- Assume a priori that we have two HD wallets secret keys and HD wallet addresses.

      -- There must be at least two distinct secret keys.
      let (Just (secretKey1:secretKey2:_)) = genesisSecretKeys

      address1 <- secretKeyToAddress secretKey1 (PassPhrase "")
      address2 <- secretKeyToAddress secretKey2 (PassPhrase "")

      let (SecretKey xprv) = secretKey1
      encryptedSecret1 <- mkEncSecretUnsafe (PassPhrase "passphrase") xprv
      (secretKey3, address3) <- deriveTestHD encryptedSecret1 (PassPhrase "passphrase")

      putText $ "[Main] Address 1 = " `T.append` show address1
      putText $ "[Main] Address 2 = " `T.append` show address2
      putText $ "[Main] HD Address (No Attributes) = " `T.append` show address3

      putText "[Main] Looking up a constant piece of data in httpWorker ..."
      userAddress <- lookupConstant wcPersistentState "<user_key>"
      putText $ "[Main] User Address = " `T.append` show userAddress

      putText "Lookup up a dynamic piece of data in httpWorker ..."

      -- Trigger onApplyBlocks by generating valid blocks
      putText "[Main] Generating valid blunds ..."

      blunds <- generateBlunds [secretKey1,secretKey2,secretKey3] 1 1 (1,1) 1

      tip <- getTip

      putText $ "[Main] Latest block = " `T.append` show tip

      putText $ show $ length blunds

      return ()

initAcidState :: (AcidState PersistentState -> Production ()) -> Production ()
initAcidState f =
  openPersistentState >>= f

initTransientState :: (MonadIO m) => m (MVar TransientState)
initTransientState =
  newMVar $ TransientState 0

-- | For each dynamic piece of data within the set ...
synchronise :: MonadIO m => AcidState PersistentState -> m ()
synchronise acid = do
  putText "Synchronising dynamic set ..."

  -- Fetch all dynamic pieces of data.
  dynamicSet <- getDynamicSet acid

  putText $ show dynamicSet

  -- Run
  return ()

actionWithWallet
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SscParams
    -> NodeParams
    -> Production ()
actionWithWallet sscParams nodeParams =
  bracketNodeResources nodeParams sscParams
    txpGlobalSettings
    initNodeDBs $ \nodeResources -> do
        initAcidState $ \acid -> do
            -- Create a wallet based on one of the genesis secret keys.
            address <- defaultAddress
            insertConstant acid "<user_key>" address

            -- Generate blocks based on this secret.

            -- Create a constant piece of data such as an address which is later used
            -- to find relevant data and update the dynamic set.

            synchronise acid

            -- Create a dynamic piece of data (an address)
            -- address <- defaultAddress

            -- For each dynamic piece of data, we run the synchronisation process in
            -- parallel across each.

            -- synchronise psDynamicSet

            runRealBasedMode (walletMode acid) realModeToWallet nodeResources $
                runNode nodeResources workers
  where
    defaultAddress :: HasConfigurations => Production Address
    defaultAddress = do
      let (Just (secretKey1:_)) = genesisSecretKeys
      secretKeyToAddress secretKey1 (PassPhrase "")

--    initWallet = do

      -- The main thread is responsible for initialising the persistent state. Later,
      -- the http handlers can update the data. AcidState works fine here.

      -- realistically store much more than an address but for demo purposes holds just
      -- one address.

      -- persistentState <- openPersistentState

      -- The persistent state is used to filter the transient state upon initialisation.
      -- blunds <- getTip >>= loadBlundsWhile (const True)
      -- let blunds = map identity blunds

      -- let initState = filterState blunds address 0

    walletMode acid walletAction = do
        realModeContext <- ask
        let walletContext = WalletContext realModeContext acid
        lift $ runReaderT walletAction walletContext

    workers = mconcatPair
        [ updateTriggerWorker
        , httpWorker
        ]

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> Production ()
action (SimpleNodeArgs (commonNodeArgs@CommonNodeArgs {..}) (nodeArgs@NodeArgs {..})) = do
  whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData True
  logInfo $ sformat ("System start time is " % shown) $ gdStartTime genesisData
  currentParams <- CLI.getNodeParams loggerName commonNodeArgs nodeArgs
  let vssSK = fromJust $ npUserSecret currentParams ^. usVss
  let sscParams = CLI.gtSscParams commonNodeArgs vssSK (npBehaviorConfig currentParams)
  actionWithWallet sscParams currentParams


loggerName :: LoggerName
loggerName = "node"

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
  args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
  let loggingParams = CLI.loggingParams "wallet-backend" commonNodeArgs
  let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
  loggerBracket (loggingParams { lpConsoleLog = Just False }) . runProduction $ do
    CLI.printFlags
    withConfigurations conf $ action args
