module Wallet.BlockGeneration
  (
    generateBlunds
  ) where

import           Universum
import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default (def)
import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Types (Blund)
import           Pos.Core (BlockCount, gdBootStakeholders, genesisData)
import           Pos.Crypto (SecretKey (..))
import           Pos.Generator.Block (TxGenParams (..), BlockGenParams (..), genBlocks)
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Txp (TxpGlobalSettings)
import           Pos.Util (lensOf')
import           Pos.Util.CompileInfo (withCompileInfo)
import           System.Random (mkStdGen)
import           Wallet.Mode (MonadWalletMode)

-- | Generate block undos
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
