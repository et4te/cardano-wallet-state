module Wallet.Http
  (
    runHttpServer
  ) where

import           Universum
import           Pos.Core (genesisSecretKeys)
import           Pos.Crypto (SecretKey (..), PassPhrase (..), mkEncSecretUnsafe)
import           Wallet.Address (secretKeyToAddress, deriveTestHD)
import           Wallet.BlockGeneration (generateBlunds)
import           Wallet.Mode (MonadWalletMode)

-- | Start a http server ...
runHttpServer :: MonadWalletMode m => t -> m ()
runHttpServer _sendActions = do
    -- Trigger artificial block generation
    let (Just (secretKey1:secretKey2:_)) = genesisSecretKeys

    _address1 <- secretKeyToAddress secretKey1 (PassPhrase "")
    _address2 <- secretKeyToAddress secretKey2 (PassPhrase "")

    let (SecretKey xprv) = secretKey1
    encryptedSecret1 <- mkEncSecretUnsafe (PassPhrase "passphrase") xprv
    (secretKey3, _address3) <- deriveTestHD encryptedSecret1 (PassPhrase "passphrase")

    _ <- generateBlunds [secretKey1, secretKey2, secretKey3] 1 1 (1, 1) 1

    return ()
