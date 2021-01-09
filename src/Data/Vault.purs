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
import Effect.Unsafe (unsafePerformEffect)

-- | Key to access the vault.
data Key a = Key UniqueKey (Ref (Maybe a)) 

-- | A persistent store for a single value.
data Locker = Locker UniqueKey (Effect Unit)

-- | A persistent store for values of arbitrary types.
newtype Vault = Vault (Map UniqueKey Locker)

-- | Put a value into a `Locker`.
lock :: forall a. Key a -> a -> Locker 
lock (Key u ref) x = Locker u $ Ref.write (Just x) ref 

-- | Retrieve a value from the `Locker`.
unlock :: forall a. Key a -> Locker -> Maybe a
unlock (Key k ref) (Locker k' m)
  | k == k' = unsafePerformEffect  do 
      _ <- m
      Ref.read ref 
  | otherwise = Nothing 

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
insert key@(Key k _) v (Vault m) = Vault $ Map.insert k (lock key v) m 

-- | Lookup the value of a key in the vault.
lookup :: forall a. Key a -> Vault -> Maybe a 
lookup key@(Key k _) (Vault m) = Map.lookup k m >>= unlock key

-- | Delete a key from the vault.
delete :: forall a. Key a -> Vault -> Vault 
delete (Key k _) (Vault m) = Vault $ Map.delete k m 