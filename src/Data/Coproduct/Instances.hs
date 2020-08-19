{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}

module Data.Coproduct.Instances where
import Data.Coproduct.Classes
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

import Data.CMonoid
import Data.CGroup
import Data.Ring
import Data.BoolRing
import Data.DLattice

-- cf.
-- http://hackage.haskell.org/package/monoid-extras-0.4.2/docs/Data-Monoid-Coproduct.html
-- https://hackage.haskell.org/package/comonad-4.2.7.2/docs/Data-Functor-Coproduct.html
-- https://hackage.haskell.org/package/data-category-0.6.0/docs/Data-Category-Coproduct.html

{-- Coproduct of monoids --}
data AorB = A | B

data Alternate :: AorB → ★ → ★ → ★ where
   Empty :: Alternate any a b
   ConsA :: a → Alternate B a b → Alternate A a b
   ConsB :: b → Alternate A a b → Alternate B a b

instance (Monoid a, Monoid b) ⇒ Coproduct Monoid a b where
   data Coprod Monoid a b where
      M :: Alternate any a b → Coprod Monoid a b

   inl a = M (a `ConsA` Empty)
   inr b = M (b `ConsB` Empty)

   eva (f :: α → δ) (g :: β → δ) (M c) = eva' c
     where eva' :: Alternate start α β → δ
           eva' Empty = mempty
           eva' (a `ConsA` Empty) = f a
           eva' (b `ConsB` Empty) = g b
           eva' (a `ConsA` m) = f a `mappend` eva' m
           eva' (b `ConsB` m) = g b `mappend` eva' m

instance (Monoid α, Monoid β) ⇒ Semigroup (Coprod Monoid α β) where
  M l <> M r = l `mul` r
   where mul :: (Monoid a, Monoid b) ⇒
                 Alternate start a b → Alternate start' a b → Coprod Monoid a b
         l `mul` Empty = M l
         Empty `mul` r = M r
         ConsA a m `mul` r | M m' ← mul m r = M (a `consA` m')
         ConsB b m `mul` r | M m' ← mul m r = M (b `consB` m')

         consA :: Monoid a ⇒ a → Alternate start a b → Alternate A a b
         a `consA` Empty = a `ConsA` Empty
         a `consA` ConsA a' m = (a `mappend` a') `ConsA` m
         a `consA` r@(ConsB _ _) = a `ConsA` r

         consB :: Monoid b ⇒ b → Alternate start a b → Alternate B a b
         b `consB` Empty = b `ConsB` Empty
         b `consB` ConsB b' m =  (b `mappend` b') `ConsB` m
         b `consB` r@(ConsA _ _) = b `ConsB` r



instance (Monoid α, Monoid β) ⇒ Monoid (Coprod Monoid α β) where
  mempty = M Empty
  
{-- Coproduct of sets --}
class Set a
instance Set a
  
instance Coproduct Set a b where
  data Coprod Set a b = Inl a | Inr b
    deriving (Show)
  inl = Inl
  inr = Inr
  eva f g (Inl x) = f x ; eva f g (Inr y) = g y

instance Free Set x where
  newtype FreeA Set x = F { unF :: x }
    deriving (Show)
  pvar = F
  F x `pbind` k = k x

instance (CMonoid a, CMonoid b) ⇒ Coproduct CMonoid a b where
  data Coprod CMonoid a b = C a b

  inl a = C a mempty
  inr b = C mempty b
  eva f g (C a b) = f a `mappend` g b

instance (CMonoid α, CMonoid β) ⇒ CMonoid (Coprod CMonoid α β) where

instance (CMonoid α, CMonoid β) ⇒ Semigroup (Coprod CMonoid α β) where
  C a b <> C a' b' = C (a `mappend` a') (b `mappend` b')
  
instance (CMonoid α, CMonoid β) ⇒ Monoid (Coprod CMonoid α β) where
  mempty = C mempty mempty
  

{-- Coproduct of abelian groups --}
instance (CGroup a, CGroup b) ⇒ Coproduct CGroup a b where
   newtype Coprod CGroup a b = Cg { unCG :: Coprod CMonoid a b }
     deriving (Semigroup, CMonoid, Monoid)
   inl = Cg . inl
   inr = Cg . inr
   eva f g = eva f g . unCG

instance (CGroup α, CGroup β) ⇒ CGroup (Coprod CGroup α β) where
   cinv (Cg (C a b)) = Cg (C (cinv a) (cinv b))

-- Free monoids
instance Free Monoid x where
   newtype FreeA Monoid x = P [x] deriving (Semigroup, Monoid)
   pvar x = P [x]
   P [] `pbind` f = mempty
   P [x] `pbind` f = f x
   P xs `pbind` f = Prelude.foldr (mappend . f) mempty xs

-- Coproduct of commutative rings
instance (Ring α, Ord x) ⇒ Coproduct Ring α (FreeA Ring x) where
  newtype Coprod Ring α (FreeA Ring x) = CR { unCR :: Multinomial x α }
    deriving (Ring)

  inl a = CR (MN (Map.singleton MultiSet.empty a))
  inr (RingA (MN x)) = CR (MN (Map.map initMN x))
  eva f g = evalMN f (g . pvar) . unCR

-- Coproduct of Boolean rings
instance (BoolRing α, Ord x) ⇒ Coproduct BoolRing α (FreeA BoolRing x) where
  newtype Coprod BoolRing α (FreeA BoolRing x) = BR { unBR :: Polynomial x α }
    deriving (Ring, BoolRing)

  inl a = BR (PN (Map.singleton Set.empty a))
  inr (BoolRingA (PN x)) = BR (PN (Map.map initPN x))
  eva f g = evalPN f (g . pvar) . unBR

-- Coproduct of distributive lattices
instance (DLattice α, Ord x) ⇒ Coproduct DLattice α (FreeA DLattice x) where
  newtype Coprod DLattice α (FreeA DLattice x) = DL { unDL :: NormalForm x α }
    deriving (DLattice, Eq)

  inl a = DL (NF (Map.singleton Set.empty a))
  inr (DLatticeA (NF x)) = DL (NF (Map.map initNF x))
  eva f g = evalNF f (g . pvar) . unDL
