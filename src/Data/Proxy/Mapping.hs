-- | This module provides functions to map Proxy types
--
-- @
--  p1 :: Proxy (Either Int Bool)
--  p1 = Proxy
--
--  p2 = proxy1of2 p2
--
--  ghci> :t p2
--  p2 :: Proxy Int
-- @
--
-- A toy example with a class that creates Strings from Proxy types:
--
-- @
--  class FromProxy t where
--    createString :: Proxy t -> String
--
--  instance FromProxy Int where
--    createString _ = \"Int\"
--
--  instance FromProxy Bool where
--    createString _ = \"Bool\"
--
--  instance (FromProxy a, FromProxy b) => FromProxy (Either a b) where
--    createString p = "Either " ++ proxy1of2 p ++ " " ++ proxy2of2 p
--
--  instance (FromProxy a, FromProxy b) => FromProxy ((->) a b) where
--    createString p = createString (proxy1of2 p) ++ " -> " ++ createString (proxy2of2 p)
--
--  ghci> createString (Proxy :: Proxy (Either Int Bool))
--  "Either Int Bool"
--
--  ghci> createString (Proxy :: Proxy (Int -> Bool -> Either Int Bool))
--  "Int -> Bool -> Either Int Bool"
-- @
--
-- A toy example where an Integer is computed from a Proxy type:
--
-- @
--  {-\# Language DataKinds \#-}
--  {-\# Language KindSignatures \#-}
--  import GHC.TypeLits
--  import Data.Kind (Type)
--  import Data.Proxy
--  import Data.Proxy.Mapping
--
--  class Compute t where
--    compute :: Proxy t -> Integer
--
--  data Number (n :: Nat)
--  data Add a b
--  data Apply (f :: Type -> Type) (a :: Type)
--
--  instance KnownNat n => Compute (Number n) where
--    compute p = natVal (proxy1of1 p)
--
--  instance (Compute a, Compute b) => Compute (Add a b) where
--    compute p = (compute $ proxy1of2 p) + (compute $ proxy2of2 p)
--
--  instance (Compute a, Compute (f a)) => Compute (Apply f a) where
--    compute p = compute (proxyApply (proxy1of2 p) (proxy2of2 p))
-- @
--
-- @
--  ghci> compute (Proxy :: Proxy (Apply (Add (Number 1)) (Number 2)))
--  3
-- @
--
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# language TypeInType #-}
module Data.Proxy.Mapping
  ( -- * Head and Tail
    proxyHead
  , proxyTail
    -- * Application
  , proxyApply
    -- * Successor and Predecessor
  , proxySucc
  , proxyPred
    -- * Accessing type constructor and parameters
  , proxy0of1
  , proxy1of1
  , proxy0of2
  , proxy1of2
  , proxy2of2
  , proxy0of3
  , proxy1of3
  , proxy2of3
  , proxy3of3
  , proxy0of4
  , proxy1of4
  , proxy2of4
  , proxy3of4
  , proxy4of4
  , proxy0of5
  , proxy1of5
  , proxy2of5
  , proxy3of5
  , proxy4of5
  , proxy5of5
  , proxy0of6
  , proxy1of6
  , proxy2of6
  , proxy3of6
  , proxy4of6
  , proxy5of6
  , proxy6of6
  , proxy0of7
  , proxy1of7
  , proxy2of7
  , proxy3of7
  , proxy4of7
  , proxy5of7
  , proxy6of7
  , proxy7of7
  , proxy0of8
  , proxy1of8
  , proxy2of8
  , proxy3of8
  , proxy4of8
  , proxy5of8
  , proxy6of8
  , proxy7of8
  , proxy8of8
  , proxy0of9
  , proxy1of9
  , proxy2of9
  , proxy3of9
  , proxy4of9
  , proxy5of9
  , proxy6of9
  , proxy7of9
  , proxy8of9
  , proxy9of9
  , proxy0of10
  , proxy1of10
  , proxy2of10
  , proxy3of10
  , proxy4of10
  , proxy5of10
  , proxy6of10
  , proxy7of10
  , proxy8of10
  , proxy9of10
  , proxy10of10
  , proxy0of11
  , proxy1of11
  , proxy2of11
  , proxy3of11
  , proxy4of11
  , proxy5of11
  , proxy6of11
  , proxy7of11
  , proxy8of11
  , proxy9of11
  , proxy10of11
  , proxy11of11
  , proxy0of12
  , proxy1of12
  , proxy2of12
  , proxy3of12
  , proxy4of12
  , proxy5of12
  , proxy6of12
  , proxy7of12
  , proxy8of12
  , proxy9of12
  , proxy10of12
  , proxy11of12
  , proxy12of12
  , proxy0of13
  , proxy1of13
  , proxy2of13
  , proxy3of13
  , proxy4of13
  , proxy5of13
  , proxy6of13
  , proxy7of13
  , proxy8of13
  , proxy9of13
  , proxy10of13
  , proxy11of13
  , proxy12of13
  , proxy13of13
  , proxy0of14
  , proxy1of14
  , proxy2of14
  , proxy3of14
  , proxy4of14
  , proxy5of14
  , proxy6of14
  , proxy7of14
  , proxy8of14
  , proxy9of14
  , proxy10of14
  , proxy11of14
  , proxy12of14
  , proxy13of14
  , proxy14of14
  , proxy0of15
  , proxy1of15
  , proxy2of15
  , proxy3of15
  , proxy4of15
  , proxy5of15
  , proxy6of15
  , proxy7of15
  , proxy8of15
  , proxy9of15
  , proxy10of15
  , proxy11of15
  , proxy12of15
  , proxy13of15
  , proxy14of15
  , proxy15of15
  , proxy0of16
  , proxy1of16
  , proxy2of16
  , proxy3of16
  , proxy4of16
  , proxy5of16
  , proxy6of16
  , proxy7of16
  , proxy8of16
  , proxy9of16
  , proxy10of16
  , proxy11of16
  , proxy12of16
  , proxy13of16
  , proxy14of16
  , proxy15of16
  , proxy16of16
  )
  where

import GHC.TypeLits
import Data.Proxy

-- | Mapping to the head of the type list (x)
proxyHead :: Proxy (x ': xs) -> Proxy x
proxyHead _ = Proxy

-- | Mapping to the tail of the type list (xs)
proxyTail :: Proxy (x ': xs) -> Proxy xs
proxyTail _ = Proxy

-- | Applying a type constructor to a type
proxyApply :: Proxy f -> Proxy a -> Proxy (f a)
proxyApply _ _ = Proxy

-- | Mapping to the successor of a 'Nat'
proxySucc :: Proxy n -> Proxy (n+1)
proxySucc _ = Proxy

-- | Mapping to the predecessor of a 'Nat'
proxyPred :: (1 <= n) => Proxy n -> Proxy (n-1)
proxyPred _ = Proxy

-- | Mapping to the type constructor (t) of a type with 1 parameter
proxy0of1 :: Proxy (t a) -> Proxy t
proxy0of1 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 1 parameter
proxy1of1 :: Proxy (t a) -> Proxy a
proxy1of1 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 2 parameters
proxy0of2 :: Proxy (t a b) -> Proxy t
proxy0of2 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 2 parameters
proxy1of2 :: Proxy (t a b) -> Proxy a
proxy1of2 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 2 parameters
proxy2of2 :: Proxy (t a b) -> Proxy b
proxy2of2 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 3 parameters
proxy0of3 :: Proxy (t a b c) -> Proxy t
proxy0of3 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 3 parameters
proxy1of3 :: Proxy (t a b c) -> Proxy a
proxy1of3 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 3 parameters
proxy2of3 :: Proxy (t a b c) -> Proxy b
proxy2of3 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 3 parameters
proxy3of3 :: Proxy (t a b c) -> Proxy c
proxy3of3 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 4 parameters
proxy0of4 :: Proxy (t a b c d) -> Proxy t
proxy0of4 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 4 parameters
proxy1of4 :: Proxy (t a b c d) -> Proxy a
proxy1of4 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 4 parameters
proxy2of4 :: Proxy (t a b c d) -> Proxy b
proxy2of4 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 4 parameters
proxy3of4 :: Proxy (t a b c d) -> Proxy c
proxy3of4 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 4 parameters
proxy4of4 :: Proxy (t a b c d) -> Proxy d
proxy4of4 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 5 parameters
proxy0of5 :: Proxy (t a b c d e) -> Proxy t
proxy0of5 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 5 parameters
proxy1of5 :: Proxy (t a b c d e) -> Proxy a
proxy1of5 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 5 parameters
proxy2of5 :: Proxy (t a b c d e) -> Proxy b
proxy2of5 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 5 parameters
proxy3of5 :: Proxy (t a b c d e) -> Proxy c
proxy3of5 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 5 parameters
proxy4of5 :: Proxy (t a b c d e) -> Proxy d
proxy4of5 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 5 parameters
proxy5of5 :: Proxy (t a b c d e) -> Proxy e
proxy5of5 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 6 parameters
proxy0of6 :: Proxy (t a b c d e f) -> Proxy t
proxy0of6 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 6 parameters
proxy1of6 :: Proxy (t a b c d e f) -> Proxy a
proxy1of6 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 6 parameters
proxy2of6 :: Proxy (t a b c d e f) -> Proxy b
proxy2of6 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 6 parameters
proxy3of6 :: Proxy (t a b c d e f) -> Proxy c
proxy3of6 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 6 parameters
proxy4of6 :: Proxy (t a b c d e f) -> Proxy d
proxy4of6 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 6 parameters
proxy5of6 :: Proxy (t a b c d e f) -> Proxy e
proxy5of6 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 6 parameters
proxy6of6 :: Proxy (t a b c d e f) -> Proxy f
proxy6of6 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 7 parameters
proxy0of7 :: Proxy (t a b c d e f g) -> Proxy t
proxy0of7 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 7 parameters
proxy1of7 :: Proxy (t a b c d e f g) -> Proxy a
proxy1of7 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 7 parameters
proxy2of7 :: Proxy (t a b c d e f g) -> Proxy b
proxy2of7 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 7 parameters
proxy3of7 :: Proxy (t a b c d e f g) -> Proxy c
proxy3of7 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 7 parameters
proxy4of7 :: Proxy (t a b c d e f g) -> Proxy d
proxy4of7 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 7 parameters
proxy5of7 :: Proxy (t a b c d e f g) -> Proxy e
proxy5of7 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 7 parameters
proxy6of7 :: Proxy (t a b c d e f g) -> Proxy f
proxy6of7 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 7 parameters
proxy7of7 :: Proxy (t a b c d e f g) -> Proxy g
proxy7of7 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 8 parameters
proxy0of8 :: Proxy (t a b c d e f g h) -> Proxy t
proxy0of8 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 8 parameters
proxy1of8 :: Proxy (t a b c d e f g h) -> Proxy a
proxy1of8 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 8 parameters
proxy2of8 :: Proxy (t a b c d e f g h) -> Proxy b
proxy2of8 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 8 parameters
proxy3of8 :: Proxy (t a b c d e f g h) -> Proxy c
proxy3of8 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 8 parameters
proxy4of8 :: Proxy (t a b c d e f g h) -> Proxy d
proxy4of8 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 8 parameters
proxy5of8 :: Proxy (t a b c d e f g h) -> Proxy e
proxy5of8 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 8 parameters
proxy6of8 :: Proxy (t a b c d e f g h) -> Proxy f
proxy6of8 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 8 parameters
proxy7of8 :: Proxy (t a b c d e f g h) -> Proxy g
proxy7of8 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 8 parameters
proxy8of8 :: Proxy (t a b c d e f g h) -> Proxy h
proxy8of8 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 9 parameters
proxy0of9 :: Proxy (t a b c d e f g h i) -> Proxy t
proxy0of9 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 9 parameters
proxy1of9 :: Proxy (t a b c d e f g h i) -> Proxy a
proxy1of9 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 9 parameters
proxy2of9 :: Proxy (t a b c d e f g h i) -> Proxy b
proxy2of9 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 9 parameters
proxy3of9 :: Proxy (t a b c d e f g h i) -> Proxy c
proxy3of9 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 9 parameters
proxy4of9 :: Proxy (t a b c d e f g h i) -> Proxy d
proxy4of9 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 9 parameters
proxy5of9 :: Proxy (t a b c d e f g h i) -> Proxy e
proxy5of9 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 9 parameters
proxy6of9 :: Proxy (t a b c d e f g h i) -> Proxy f
proxy6of9 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 9 parameters
proxy7of9 :: Proxy (t a b c d e f g h i) -> Proxy g
proxy7of9 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 9 parameters
proxy8of9 :: Proxy (t a b c d e f g h i) -> Proxy h
proxy8of9 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 9 parameters
proxy9of9 :: Proxy (t a b c d e f g h i) -> Proxy i
proxy9of9 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 10 parameters
proxy0of10 :: Proxy (t a b c d e f g h i j) -> Proxy t
proxy0of10 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 10 parameters
proxy1of10 :: Proxy (t a b c d e f g h i j) -> Proxy a
proxy1of10 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 10 parameters
proxy2of10 :: Proxy (t a b c d e f g h i j) -> Proxy b
proxy2of10 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 10 parameters
proxy3of10 :: Proxy (t a b c d e f g h i j) -> Proxy c
proxy3of10 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 10 parameters
proxy4of10 :: Proxy (t a b c d e f g h i j) -> Proxy d
proxy4of10 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 10 parameters
proxy5of10 :: Proxy (t a b c d e f g h i j) -> Proxy e
proxy5of10 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 10 parameters
proxy6of10 :: Proxy (t a b c d e f g h i j) -> Proxy f
proxy6of10 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 10 parameters
proxy7of10 :: Proxy (t a b c d e f g h i j) -> Proxy g
proxy7of10 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 10 parameters
proxy8of10 :: Proxy (t a b c d e f g h i j) -> Proxy h
proxy8of10 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 10 parameters
proxy9of10 :: Proxy (t a b c d e f g h i j) -> Proxy i
proxy9of10 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 10 parameters
proxy10of10 :: Proxy (t a b c d e f g h i j) -> Proxy j
proxy10of10 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 11 parameters
proxy0of11 :: Proxy (t a b c d e f g h i j k) -> Proxy t
proxy0of11 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 11 parameters
proxy1of11 :: Proxy (t a b c d e f g h i j k) -> Proxy a
proxy1of11 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 11 parameters
proxy2of11 :: Proxy (t a b c d e f g h i j k) -> Proxy b
proxy2of11 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 11 parameters
proxy3of11 :: Proxy (t a b c d e f g h i j k) -> Proxy c
proxy3of11 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 11 parameters
proxy4of11 :: Proxy (t a b c d e f g h i j k) -> Proxy d
proxy4of11 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 11 parameters
proxy5of11 :: Proxy (t a b c d e f g h i j k) -> Proxy e
proxy5of11 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 11 parameters
proxy6of11 :: Proxy (t a b c d e f g h i j k) -> Proxy f
proxy6of11 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 11 parameters
proxy7of11 :: Proxy (t a b c d e f g h i j k) -> Proxy g
proxy7of11 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 11 parameters
proxy8of11 :: Proxy (t a b c d e f g h i j k) -> Proxy h
proxy8of11 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 11 parameters
proxy9of11 :: Proxy (t a b c d e f g h i j k) -> Proxy i
proxy9of11 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 11 parameters
proxy10of11 :: Proxy (t a b c d e f g h i j k) -> Proxy j
proxy10of11 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 11 parameters
proxy11of11 :: Proxy (t a b c d e f g h i j k) -> Proxy k
proxy11of11 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 12 parameters
proxy0of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy t
proxy0of12 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 12 parameters
proxy1of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy a
proxy1of12 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 12 parameters
proxy2of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy b
proxy2of12 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 12 parameters
proxy3of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy c
proxy3of12 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 12 parameters
proxy4of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy d
proxy4of12 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 12 parameters
proxy5of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy e
proxy5of12 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 12 parameters
proxy6of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy f
proxy6of12 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 12 parameters
proxy7of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy g
proxy7of12 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 12 parameters
proxy8of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy h
proxy8of12 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 12 parameters
proxy9of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy i
proxy9of12 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 12 parameters
proxy10of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy j
proxy10of12 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 12 parameters
proxy11of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy k
proxy11of12 _ = Proxy

-- | Mapping to the 12th type parameter (l) of a type with 12 parameters
proxy12of12 :: Proxy (t a b c d e f g h i j k l) -> Proxy l
proxy12of12 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 13 parameters
proxy0of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy t
proxy0of13 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 13 parameters
proxy1of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy a
proxy1of13 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 13 parameters
proxy2of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy b
proxy2of13 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 13 parameters
proxy3of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy c
proxy3of13 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 13 parameters
proxy4of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy d
proxy4of13 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 13 parameters
proxy5of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy e
proxy5of13 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 13 parameters
proxy6of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy f
proxy6of13 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 13 parameters
proxy7of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy g
proxy7of13 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 13 parameters
proxy8of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy h
proxy8of13 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 13 parameters
proxy9of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy i
proxy9of13 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 13 parameters
proxy10of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy j
proxy10of13 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 13 parameters
proxy11of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy k
proxy11of13 _ = Proxy

-- | Mapping to the 12th type parameter (l) of a type with 13 parameters
proxy12of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy l
proxy12of13 _ = Proxy

-- | Mapping to the 13th type parameter (m) of a type with 13 parameters
proxy13of13 :: Proxy (t a b c d e f g h i j k l m) -> Proxy m
proxy13of13 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 14 parameters
proxy0of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy t
proxy0of14 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 14 parameters
proxy1of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy a
proxy1of14 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 14 parameters
proxy2of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy b
proxy2of14 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 14 parameters
proxy3of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy c
proxy3of14 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 14 parameters
proxy4of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy d
proxy4of14 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 14 parameters
proxy5of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy e
proxy5of14 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 14 parameters
proxy6of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy f
proxy6of14 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 14 parameters
proxy7of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy g
proxy7of14 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 14 parameters
proxy8of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy h
proxy8of14 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 14 parameters
proxy9of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy i
proxy9of14 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 14 parameters
proxy10of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy j
proxy10of14 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 14 parameters
proxy11of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy k
proxy11of14 _ = Proxy

-- | Mapping to the 12th type parameter (l) of a type with 14 parameters
proxy12of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy l
proxy12of14 _ = Proxy

-- | Mapping to the 13th type parameter (m) of a type with 14 parameters
proxy13of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy m
proxy13of14 _ = Proxy

-- | Mapping to the 14th type parameter (n) of a type with 14 parameters
proxy14of14 :: Proxy (t a b c d e f g h i j k l m n) -> Proxy n
proxy14of14 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 15 parameters
proxy0of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy t
proxy0of15 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 15 parameters
proxy1of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy a
proxy1of15 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 15 parameters
proxy2of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy b
proxy2of15 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 15 parameters
proxy3of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy c
proxy3of15 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 15 parameters
proxy4of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy d
proxy4of15 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 15 parameters
proxy5of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy e
proxy5of15 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 15 parameters
proxy6of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy f
proxy6of15 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 15 parameters
proxy7of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy g
proxy7of15 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 15 parameters
proxy8of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy h
proxy8of15 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 15 parameters
proxy9of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy i
proxy9of15 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 15 parameters
proxy10of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy j
proxy10of15 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 15 parameters
proxy11of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy k
proxy11of15 _ = Proxy

-- | Mapping to the 12th type parameter (l) of a type with 15 parameters
proxy12of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy l
proxy12of15 _ = Proxy

-- | Mapping to the 13th type parameter (m) of a type with 15 parameters
proxy13of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy m
proxy13of15 _ = Proxy

-- | Mapping to the 14th type parameter (n) of a type with 15 parameters
proxy14of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy n
proxy14of15 _ = Proxy

-- | Mapping to the 15th type parameter (o) of a type with 15 parameters
proxy15of15 :: Proxy (t a b c d e f g h i j k l m n o) -> Proxy o
proxy15of15 _ = Proxy

-- | Mapping to the type constructor (t) of a type with 16 parameters
proxy0of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy t
proxy0of16 _ = Proxy

-- | Mapping to the 1st type parameter (a) of a type with 16 parameters
proxy1of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy a
proxy1of16 _ = Proxy

-- | Mapping to the 2nd type parameter (b) of a type with 16 parameters
proxy2of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy b
proxy2of16 _ = Proxy

-- | Mapping to the 3rd type parameter (c) of a type with 16 parameters
proxy3of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy c
proxy3of16 _ = Proxy

-- | Mapping to the 4th type parameter (d) of a type with 16 parameters
proxy4of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy d
proxy4of16 _ = Proxy

-- | Mapping to the 5th type parameter (e) of a type with 16 parameters
proxy5of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy e
proxy5of16 _ = Proxy

-- | Mapping to the 6th type parameter (f) of a type with 16 parameters
proxy6of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy f
proxy6of16 _ = Proxy

-- | Mapping to the 7th type parameter (g) of a type with 16 parameters
proxy7of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy g
proxy7of16 _ = Proxy

-- | Mapping to the 8th type parameter (h) of a type with 16 parameters
proxy8of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy h
proxy8of16 _ = Proxy

-- | Mapping to the 9th type parameter (i) of a type with 16 parameters
proxy9of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy i
proxy9of16 _ = Proxy

-- | Mapping to the 10th type parameter (j) of a type with 16 parameters
proxy10of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy j
proxy10of16 _ = Proxy

-- | Mapping to the 11th type parameter (k) of a type with 16 parameters
proxy11of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy k
proxy11of16 _ = Proxy

-- | Mapping to the 12th type parameter (l) of a type with 16 parameters
proxy12of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy l
proxy12of16 _ = Proxy

-- | Mapping to the 13th type parameter (m) of a type with 16 parameters
proxy13of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy m
proxy13of16 _ = Proxy

-- | Mapping to the 14th type parameter (n) of a type with 16 parameters
proxy14of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy n
proxy14of16 _ = Proxy

-- | Mapping to the 15th type parameter (o) of a type with 16 parameters
proxy15of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy o
proxy15of16 _ = Proxy

-- | Mapping to the 16th type parameter (p) of a type with 16 parameters
proxy16of16 :: Proxy (t a b c d e f g h i j k l m n o p) -> Proxy p
proxy16of16 _ = Proxy

