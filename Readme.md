# proxy-mapping

This module provides functions to map Proxy types.

    p1 :: Proxy (Either Int Bool)
    p1 = Proxy
  
    p2 = proxy1of2 p2
  
    ghci> :t p2
    p2 :: Proxy Int

A toy example with a class that creates Strings from Proxy types:

    class FromProxy t where
      createString :: Proxy t -> String
  
    instance FromProxy Int where
      createString _ = \"Int\"
  
    instance FromProxy Bool where
      createString _ = \"Bool\"
  
    instance (FromProxy a, FromProxy b) => FromProxy (Either a b) where
      createString p = "Either " ++ proxy1of2 p ++ " " ++ proxy2of2 p
  
    instance (FromProxy a, FromProxy b) => FromProxy ((->) a b) where
      createString p = createString (proxy1of2 p) ++ " -> " ++ createString (proxy2of2 p)
  
    ghci> createString (Proxy :: Proxy (Either Int Bool))
    "Either Int Bool"
  
    ghci> createString (Proxy :: Proxy (Int -> Bool -> Either Int Bool))
    "Int -> Bool -> Either Int Bool"

 A toy example where an Integer is computed from a Proxy type:

    {-\# Language DataKinds \#-}
    {-\# Language KindSignatures \#-}
    import GHC.TypeLits
    import Data.Kind (Type)
    import Data.Proxy
    import Data.Proxy.Mapping
  
    class Compute t where
      compute :: Proxy t -> Integer
  
    data Number (n :: Nat)
    data Add a b
    data Apply (f :: Type -> Type) (a :: Type)
  
    instance KnownNat n => Compute (Number n) where
      compute p = natVal (proxy1of1 p)
  
    instance (Compute a, Compute b) => Compute (Add a b) where
      compute p = (compute $ proxy1of2 p) + (compute $ proxy2of2 p)
  
    instance (Compute a, Compute (f a)) => Compute (Apply f a) where
      compute p = compute (proxyApply (proxy1of2 p) (proxy2of2 p))

Running it in ghci:

    ghci> compute (Proxy :: Proxy (Apply (Add (Number 1)) (Number 2)))
    3
