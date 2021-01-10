module Data.Vault
  ( Vault 
  , Key 
  , newKey
  , empty
  , insert
  , lookup
  , delete
  )
   where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Vault.Internal (UniqueKey, _newKey)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

-- | Type that we can use tounsafely coerce any lifted type back and forth.
data Any 

-- | Key to access the vault.
data Key a = Key UniqueKey (Ref (Maybe a)) 

-- | A persistent store for values of arbitrary types.
newtype Vault = Vault (Map UniqueKey Any)

toAny :: forall a. a -> Any
toAny = unsafeCoerce

fromAny :: forall a. Any -> a
fromAny = unsafeCoerce

-- | Empty vault.
empty  :: Vault  
empty = Vault $ Map.empty 

-- | Create a new key to use with a vault.
newKey :: forall a. Effect (Key a)
newKey = do 
  u   <- _newKey
  ref <- Ref.new Nothing
  pure $ Key u ref

-- | Insert a value for a given key.
insert :: forall a. Key a -> a -> Vault -> Vault 
insert key@(Key k _) v (Vault m) = Vault $ Map.insert k (toAny v) m 

-- | Lookup the value of a key in the vault.
lookup :: forall a. Key a -> Vault -> Maybe a 
lookup key@(Key k _) (Vault m) = fromAny <$> Map.lookup k m 

-- | Delete a key from the vault.
delete :: forall a. Key a -> Vault -> Vault 
delete (Key k _) (Vault m) = Vault $ Map.delete k m 