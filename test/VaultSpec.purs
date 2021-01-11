module Test.VaultSpec where 

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vault (Key)
import Data.Vault as Vault
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- could potentially run into race condition?
keyBoolean :: Key Boolean
keyBoolean = unsafePerformEffect Vault.newKey 

keyInt :: Effect (Key Int)
keyInt = Vault.newKey

keyString :: Effect (Key String)
keyString = Vault.newKey

spec :: Spec Unit
spec = do
  describe "vault" $ do
    it "creates and lookup values" do 
      k1 <- liftEffect keyString
      k2 <- liftEffect keyInt

      let kValue = "secret"
      let vault  = Vault.insert k1 kValue Vault.empty
        
      Vault.lookup k1 vault `shouldEqual` Just kValue 
      Vault.lookup k2 vault `shouldEqual` (Nothing :: Maybe Int)

    it "creates and delete values" do 
      k <- liftEffect keyString

      let kValue = "secret"
      let vault  = Vault.insert keyBoolean true $ Vault.insert k kValue Vault.empty   

      Vault.lookup k vault `shouldEqual` Just kValue 
      Vault.lookup keyBoolean vault `shouldEqual` Just true

      let vault'  = Vault.delete k vault 
      Vault.lookup k vault' `shouldEqual` (Nothing :: Maybe Int)