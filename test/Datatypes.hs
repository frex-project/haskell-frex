{-# LANGUAGE KindSignatures, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs, FlexibleContexts #-}
{-# LANGUAGE InstanceSigs,DeriveLift,DeriveDataTypeable, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}


module Datatypes where

import Data.Coproduct

import Data.Monoid
import Data.PartiallyStatic
import Language.Haskell.TH.Syntax

-- 1. partially-static integer lists, concretely
data PSIList1 where
  Nil1 :: PSIList1
  Cons1 :: Int → PSIList1 → PSIList1
  Dyn1 :: Code [Int] → PSIList1

infixr 5 `Cons1`


fold1 :: b -> (Int -> b -> b) -> (Code [Int] -> b) -> PSIList1 -> b
fold1 n c d Nil1 = n
fold1 n c d (Cons1 x xs) = c x (fold1 n c d xs)
fold1 n c d (Dyn1 y) = d y

sum1 :: PSIList1 -> FreeExt Monoid (Code (Sum Int)) (Sum Int)
sum1 = fold1 mempty (\s d -> sta (Sum s) `mappend` d)
         (\d -> dyn [|| Sum (sum $$d) ||])

-- 2. partially-static integer lists via a generic fixed point operator
newtype Fix f = Roll { unRoll :: f (Fix f) }

data IList r where
  Nil :: IList r
  Cons :: Int → r → IList r

infixr 5 `Cons`, `cons2`, `cons2'`

nil2' :: Fix IList
nil2' = Roll Nil

cons2' :: Int → Fix IList → Fix IList
cons2' x xs = Roll (Cons x xs)

fold2' :: (IList b -> b) -> Fix IList -> b
fold2' s (Roll Nil) = s Nil
fold2' s (Roll (Cons x xs)) = s (Cons x (fold2' s xs))

sum2' :: Fix IList -> Int
sum2' = fold2' s
  where s Nil = 0
        s (Cons x xs) = x + xs

data PSFix f where
  Sta :: f (PSFix f) → PSFix f 
  Dyn :: Code (Fix f) → PSFix f 

type PSIList2 = PSFix IList

nil2 :: PSIList2
nil2 = Sta Nil

cons2 :: Int → PSIList2 → PSIList2
cons2 x xs = Sta (Cons x xs)

dyn2 :: Code (Fix IList) → PSIList2
dyn2 = Dyn


fold2 :: (IList b -> b) -> (Code (Fix IList) -> b) -> PSFix IList -> b
fold2 s d (Sta Nil) = s Nil
fold2 s d (Sta (Cons x xs)) = s (Cons x (fold2 s d xs))
fold2 s d (Dyn c) = d c

sum2 :: PSIList2 -> FreeExt Monoid (Code (Sum Int)) (Sum Int)
sum2 = fold2 s d
  where s Nil = mempty
        s (Cons x xs) = sta (Sum x) `mappend` xs
        d c = dyn [|| Sum (sum2' $$c) ||]



-- 3. partially-static integer lists via coproducts
--    FA (+) FX = F(A + X)
data FreeM f a = Pure a | FreeM (f (FreeM f a))

class Functor f ⇒ Alg f a where alg :: f a → a

instance Functor f ⇒ Free (Alg f) a where
  newtype FreeA (Alg f) a = X {unX :: FreeM f a }
  pvar = X . Pure
  X (Pure v)  `pbind` k = k v
  X (FreeM v) `pbind` k = alg (fmap g v)
   where g x = X x `pbind` k

instance Functor f ⇒ 
  Coproduct (Alg f) (FreeA (Alg f) a) (FreeA (Alg f) b) where
   newtype Coprod (Alg f) (FreeA (Alg f) a) (FreeA (Alg f) b) =
     L { unL :: FreeA (Alg f) (Coprod Set a b) }

   inl x = L (fmap Inl x)
   inr y = L (fmap Inr y)
   eva g h (L e) = e `pbind` eva (g . pvar) (h . pvar)

instance Functor f ⇒ Alg f (FreeA (Alg f) a) where
  alg x = X (FreeM (fmap unX x))

instance Functor f ⇒ Alg f (Coprod (Alg f) (FreeA (Alg f) a) (FreeA (Alg f) b)) where
  alg x = L (X (FreeM (fmap (unX . unL) x)))

instance Functor f ⇒ Functor (FreeM f) where
  fmap :: (a -> b) -> FreeM f a -> FreeM f b
  fmap f (Pure v) = Pure (f v)
  fmap f (FreeM x) = FreeM (fmap (fmap f) x)
 
instance Functor f ⇒ Functor (FreeA (Alg f)) where
  fmap :: (a -> b) -> FreeA (Alg f) a -> FreeA (Alg f) b
  fmap f (X v) = X (fmap f v)

--

instance Functor IList where
  fmap f Nil = Nil
  fmap f (Cons x r) = Cons x (f r)

data Empty

-- Initial IList algebra
type IntList = FreeA (Alg IList) Empty

type PSIList3 = FreeExt (Alg IList) (Code IntList) IntList

nil3' :: IntList
nil3' = alg Nil

cons3' :: Int → IntList → IntList
cons3' x xs = alg (Cons x xs)

instance Alg IList Int where
  alg Nil = 0
  alg (Cons x xs) = x + xs

sum3' :: IntList → Int
sum3' x =  x `pbind` (\y -> 0)

nil3 :: PSIList3
nil3 = alg Nil

cons3 :: Int → PSIList3 → PSIList3
cons3 x xs = alg (Cons x xs)

infixr 5 `cons3`, `cons3'`
       
dyn3 :: Code (IntList) →  PSIList3
dyn3 = dyn

instance Alg IList (FreeExt Monoid (Code (Sum Int)) (Sum Int)) where
  alg Nil = mempty
  alg (Cons x xs) = sta (Sum x) `mappend` xs
  
sum3 :: PSIList3 -> FreeExt Monoid (Code (Sum Int)) (Sum Int)
sum3 = eva (`pbind` g) (`pbind` d)
  where g _ = alg Nil
        d c = dyn [|| Sum (sum3' $$c) ||]

