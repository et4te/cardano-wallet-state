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

import           Control.Monad.Random.Strict (evalRandT)
import           Crypto.Random.Types (MonadRandom)
import           Data.Acid (AcidState)
import           Data.Default (def)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Formatting (sformat, shown, (%))
import           Mockable (Production, runProduction)
import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Types (Blund)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (OutSpecs, WorkerSpec, worker)
import           Pos.Core (Address, HasConfiguration, HeaderHash, headerHash, prevBlockL, GenesisData(..),
                           genesisData, BlockCount, gdBootStakeholders, genesisSecretKeys, makeAddress,
                           AddrSpendingData (..), AddrStakeDistribution (..), AddrAttributes (..))
import           Pos.Core.Block (Block)
import           Pos.Crypto (EncryptedSecretKey (..), SecretKey (..), ShouldCheckPassphrase (..), PassPhrase (..),
                             deriveHDPassphrase, deriveHDSecretKey, packHDAddressAttr,
                             encToPublic, encToSecret, firstHardened, mkEncSecretUnsafe)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.DB (getBlock)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Block (getUndo)
import           Pos.Generator.Block (TxGenParams (..), BlockGenParams (..), genBlocks)
import           Pos.GState (getTip)
import           Pos.Launcher (HasConfigurations, NodeParams (..), LoggingParams(..), withConfigurations)
import           Pos.Launcher (runRealBasedMode, runNode, loggerBracket, bracketNodeResources)
import           Pos.Ssc.Types (SscParams)
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Txp (txpGlobalSettings, TxpGlobalSettings)
import           Pos.Update (updateTriggerWorker)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Util (mconcatPair, lensOf')

import           System.Wlog (LoggerName, logInfo)
import           System.Random (mkStdGen)

import           Wallet.Mode (MonadWalletMode, WalletContext(..), realModeToWallet)
import           Wallet.State (TransientState(..), PersistentState(..))
import           Wallet.State.Acid (openPersistentState, insertConstant, getDynamicSet)

------------------------------------------------------------------------------
-- Block Generation
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


------------------------------------------------------------------------------
-- Blockchain Traversal
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------
-- HD Address Derivation
------------------------------------------------------------------------------

secretKeyToAddress :: MonadRandom m => SecretKey -> PassPhrase -> m Address
secretKeyToAddress (SecretKey xprv) passphrase = do
  encryptedSecretKey <- mkEncSecretUnsafe passphrase xprv
  pure $ encryptedSecretKeyToAddress encryptedSecretKey Nothing

encryptedSecretKeyToAddress :: EncryptedSecretKey -> Maybe HDAddressPayload -> Address
encryptedSecretKeyToAddress encryptedSecretKey hdAddressPayload =
  let publicKey = PubKeyASD $ encToPublic encryptedSecretKey in
  let addressAttributes = AddrAttributes hdAddressPayload BootstrapEraDistr in
    makeAddress publicKey addressAttributes

deriveTestHD :: MonadRandom m => SecretKey -> PassPhrase -> m (SecretKey, Address)
deriveTestHD rootKey passphrase = do
    let hdPassphrase = deriveHDPassphrase $ encToPublic rootKey
    let _hdAddressPayload = packHDAddressAttr hdPassphrase [a', b', c']
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

------------------------------------------------------------------------------
-- Workers hooked into the node at startup.
------------------------------------------------------------------------------

-- In reality this will be a http server which has access to the state as it is updated by
-- the block listener callbacks. For testing I have simply added functions which generate
-- blocks from genesis secrets and checks that the tip of the chain has been advanced. The
-- actual state is expected to be updated in the onApplyBlocks callback of the BListener
-- instance derivation.
httpWorker
  :: (HasCompileInfo, MonadWalletMode m)
  => ([WorkerSpec m], OutSpecs)
httpWorker = first pure $ worker mempty $ runHttpServer
  where
    runHttpServer _sendActions = do
      WalletContext{..} <- ask

      tip <- getTip
      putText $ "[Init] Latest block = " `T.append` show tip

      -- Assume a priori that we have two HD wallets secret keys and HD wallet addresses.

      -- There must be at least two distinct secret keys.
      let (Just (secretKey1:secretKey2:_)) = genesisSecretKeys

      -- The addresses can be used later to find relevant transactions for testing.
      address1 <- secretKeyToAddress secretKey1 (PassPhrase "")
      address2 <- secretKeyToAddress secretKey2 (PassPhrase "")

      let (SecretKey xprv) = secretKey1
      encryptedSecret1 <- mkEncSecretUnsafe (PassPhrase "passphrase") xprv
      (secretKey3, address3) <- deriveTestHD encryptedSecret1 (PassPhrase "passphrase")

      putText $ "[Main] Address 1 = " `T.append` show address1
      putText $ "[Main] Address 2 = " `T.append` show address2
      putText $ "[Main] HD Address (No Attributes) = " `T.append` show address3

      -- Trigger onApplyBlocks by generating valid blocks
      putText "[Main] Generating valid blunds ..."

      -- When generating blunds we must use a majority of secret keys which are 'genesis'.
      blunds <- generateBlunds [secretKey1,secretKey2,secretKey3] 1 1 (1,1) 1

      -- Here simply show that the tip of the blockchain was updated by the blund generation.
      tip <- getTip

      putText $ "[Main] Latest block = " `T.append` show tip

      putText $ show $ length blunds

      return ()

------------------------------------------------------------------------------
-- Database functions called at startup of node (TEST).
------------------------------------------------------------------------------

initAcidState :: (AcidState PersistentState -> Production ()) -> Production ()
initAcidState f =
  openPersistentState >>= f

synchronise :: MonadIO m => AcidState PersistentState -> m ()
synchronise acid = do
  putText "Synchronising dynamic set ..."

  -- Fetch data which has a tip
  dynamicSet <- getDynamicSet acid

  putText $ show dynamicSet

  return ()

------------------------------------------------------------------------------
-- Beyond this point, initialisation of node, logger configuration, CLI params.
------------------------------------------------------------------------------

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
        -- First the acid state database is initialised then a node is run.
        initAcidState $ \acid -> do
            -- On initialisation we synchronise with the current state of the blockchain.
            synchronise acid

            --------------------------------------------------------------------------------
            -- TESTING
            --------------------------------------------------------------------------------

            -- For testing purposes we create an address based on the first genesis secret
            -- key and store it in the db as a constant piece of data. This will later allow
            -- us to track blocks pertaining to this address.

            -- Create a wallet based on one of the genesis secret keys.
            address <- defaultAddress
            insertConstant acid "<user_key>" address

            runRealBasedMode (walletMode acid) realModeToWallet nodeResources $
                runNode nodeResources workers
  where
    defaultAddress :: HasConfigurations => Production Address
    defaultAddress = do
      let (Just (secretKey1:_)) = genesisSecretKeys
      secretKeyToAddress secretKey1 (PassPhrase "")

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
