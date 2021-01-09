module Data.Vault.Internal where 

import Prelude 

import Effect (Effect)

foreign import data UniqueKey :: Type

foreign import _newKey      :: Effect UniqueKey
foreign import _eqKey       :: UniqueKey -> UniqueKey -> Boolean 
foreign import _compareKey  :: UniqueKey -> UniqueKey -> Int 

instance instEqKey :: Eq UniqueKey where 
  eq = _eqKey

instance instOrdKey :: Ord UniqueKey where 
  compare x y = case _compareKey x y of
    1 -> GT
    0 -> EQ
    _ -> LT
