{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BoolRing where

import Data.Ring
import Data.Coproduct.Classes (Free(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.CMonoid

-- Boolean rings, i.e. commutative ring with x * x = x
-- (equivalent to Boolean algebras)
class Ring r ⇒ BoolRing r
-- class (CGroup r, Monoid r) ⇒ BoolRing r

-- c₁xy + c₂x + … + cₙy
-- polynomials with coefficients in a and variables in x appearing at most once in each term
newtype Polynomial x a = PN (Map.Map (Set.Set x) a)
  deriving (Show)

instance (Ord x, BoolRing a) ⇒ BoolRing (Polynomial x a)

instance (Ord x, BoolRing a) ⇒ Ring (Polynomial x a) where
  r₀  = PN Map.empty
  PN l ⊕ PN r = PN (Map.unionWith (⊕) l r)
  rneg (PN m) = PN (Map.map rneg m)
  r₁ = PN (Map.singleton Set.empty r₁)
  PN l ⊗ PN r =
    -- (l₁ + l₂ + … + lₙ) × r  ≡  l₁×r + l₂×r + … + lₙ×r
    Map.foldrWithKey (\xs c acc → acc ⊕ mul₁ r xs c) r₀ l
     where 
       -- cxᵢ(r₁ + r₂ + … + rₘ)  ≡  cxᵢr₁ + cxᵢr₂ + … + cxᵢrₘ
       mul₁ r xs c =
         Map.foldrWithKey
           (\rxs r (PN acc) →
               PN $  ( Map.insertWith (⊕)
                     (rxs `Set.union` xs)
                     (c ⊗ r)
                     acc))
           r₀ r


evalPN :: (BoolRing a, BoolRing b) ⇒ (a → b) → (x → b) → Polynomial x a → b
evalPN h e (PN xss) =
  Map.foldrWithKey
    (\xs a sum →
       sum ⊕ Set.foldr
                 (\x prod → prod ⊗ (e x))
                 (h a)
                 xs)
    r₀
    xss

initPN :: BoolRing r ⇒ Bool → r
initPN False = r₀
initPN True = r₁

instance BoolRing Bool
instance Ring Bool where (⊕) = (/=); (⊗) = (&&); r₀ = False; r₁ = True; rneg = id

instance Ord x ⇒ Free BoolRing x where
   newtype FreeA BoolRing x = BoolRingA (Polynomial x Bool)
      deriving (Ring, BoolRing)
   pvar x = BoolRingA (PN (Map.singleton (Set.singleton x) True)) -- True × x
   BoolRingA xss `pbind` f = evalPN initPN f xss


newtype BoolRingOr n = BRO { bro :: n }

instance BoolRing r ⇒ Semigroup (BoolRingOr r) where (BRO x) <> (BRO y) = BRO (x ⊕ y ⊕ (x ⊗ y))
instance BoolRing r ⇒ Monoid (BoolRingOr r) where mempty = BRO r₀ ; mappend (BRO x) (BRO y) = BRO (x ⊕ y ⊕ (x ⊗ y))
instance BoolRing r ⇒ CMonoid (BoolRingOr r)
