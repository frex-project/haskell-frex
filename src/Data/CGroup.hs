{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.CGroup where

import Data.CMonoid
import Data.Coproduct.Classes (Free(..))
import qualified Data.Map as Map

class CMonoid c ⇒ CGroup c where cinv :: c → c

-- free Abelian groups
instance Ord x ⇒ Free CGroup x where 
   newtype FreeA CGroup x = CG (Map.Map x Int)
   pvar x = CG (Map.singleton x 1)
   CG b `pbind` f = emit (Map.toList b)
     where emit [] = mempty
           emit [(x,1)] = f x
           emit ((x,0):xs) = emit xs
           emit ((x,n):xs) | n < 0 = cinv (f x) `mappend` emit ((x,n+1):xs)
                           | otherwise = f x `mappend` emit ((x,n-1):xs)

instance Ord x ⇒ Semigroup (FreeA CGroup x) where
  CG x <> CG y = CG (Map.unionWith (+) x y)
  
instance Ord x ⇒ Monoid (FreeA CGroup x) where
  mempty = CG Map.empty
  

instance Ord x ⇒ CMonoid (FreeA CGroup x)

instance Ord x ⇒ CGroup (FreeA CGroup x) where
  cinv (CG m) = CG (Map.map negate m)

