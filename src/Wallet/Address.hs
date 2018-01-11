module Wallet.Address
  (
    secretKeyToAddress
  , encryptedSecretKeyToAddress
  , deriveTestHD
  ) where

----------------------------------------------------------------------------------------------------

import           Universum
import           Crypto.Random.Types (MonadRandom)
import           Pos.Core (Address, AddrAttributes (..), AddrSpendingData (..),
                           AddrStakeDistribution (..), makeAddress)
import           Pos.Crypto (EncryptedSecretKey (..), SecretKey (..), PassPhrase (..),
                             ShouldCheckPassphrase (..), firstHardened, encToSecret, encToPublic,
                             mkEncSecretUnsafe, deriveHDPassphrase, deriveHDSecretKey,
                             packHDAddressAttr)
import           Pos.Crypto.HD (HDAddressPayload (..))

-- | Convert a secret key to an address.
secretKeyToAddress :: MonadRandom m => SecretKey -> PassPhrase -> m Address
secretKeyToAddress (SecretKey xprv) passphrase = do
  encryptedSecretKey <- mkEncSecretUnsafe passphrase xprv
  pure $ encryptedSecretKeyToAddress encryptedSecretKey Nothing

-- | Convert an encrypted secret key to an address.
encryptedSecretKeyToAddress :: EncryptedSecretKey -> Maybe HDAddressPayload -> Address
encryptedSecretKeyToAddress encryptedSecretKey hdAddressPayload =
  let publicKey = PubKeyASD $ encToPublic encryptedSecretKey in
  let addressAttributes = AddrAttributes hdAddressPayload BootstrapEraDistr in
    makeAddress publicKey addressAttributes

-- | Derive a secret key / address pair from an encrypted secret key and passphrase.
deriveTestHD :: MonadRandom m => EncryptedSecretKey -> PassPhrase -> m (SecretKey, Address)
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

