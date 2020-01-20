{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.CMonoid where

import Data.Semigroup
import Data.Monoid
import Data.Coproduct.Classes (Free(..))
import Data.MultiSet

class Monoid m ⇒ CMonoid m

-- free commutative monoids
instance Ord x ⇒ Free CMonoid x where 
   newtype FreeA CMonoid x = CM (MultiSet x)
   pvar = CM . singleton
   CM b `pbind` f = emit (toList b)
     where emit [] = mempty
           emit [x] = f x
           emit (x:xs) = f x `mappend` emit xs 

instance Ord x ⇒ Semigroup (FreeA CMonoid x) where
  CM x <> CM y = CM (x `union` y)
  
instance Ord x ⇒ Monoid (FreeA CMonoid x) where
  mempty = CM empty
  
  

instance Ord x ⇒ CMonoid (FreeA CMonoid x) where

instance CMonoid (Product Int) where

