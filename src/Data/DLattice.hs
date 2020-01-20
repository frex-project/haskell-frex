{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DLattice where

import Data.Coproduct.Classes (Free(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.CMonoid

-- Distributive lattices 
class (Eq r) ⇒ DLattice r where
  (⊕), (⊗) :: r → r → r
  r₀, r₁ :: r

-- c₁xy + c₂x + … + cₙy
-- polynomials with coefficients in a and variables in x appearing at most once in each term
-- with the additional condition that if c₁ <= c₂ then c₁xy + c₂x = c₂x 
newtype NormalForm x a = NF (Map.Map (Set.Set x) a)
  deriving (Show, Eq)

instance (Ord x, DLattice a) ⇒ DLattice (NormalForm x a) where
  r₀  = NF Map.empty
  (NF l) ⊕ (NF r) = NF (Map.foldrWithKey (insertNF) l r) where
    -- Insert term while simplifying to normal form
    insertNF z k l = if (not (Map.null (Map.filterWithKey (\y r → (Set.isSubsetOf y z) && (r ⊗ k == k)) l)))
                   then l  -- list already contains a larger term
                   else    -- replace smaller terms
                    Map.insert z k
                    (Map.filterWithKey (\y r → not ((Set.isSubsetOf z y) && (r ⊗ k == r))) l)

  r₁ = NF (Map.singleton Set.empty r₁)

  NF l ⊗ NF r =
    -- (l₁ + l₂ + … + lₙ) × r  ≡  l₁×r + l₂×r + … + lₙ×r
    Map.foldrWithKey (\xs c acc → acc ⊕ mul₁ r xs c) r₀ l
     where 
       -- cxᵢ(r₁ + r₂ + … + rₘ)  ≡  cxᵢr₁ + cxᵢr₂ + … + cxᵢrₘ
       mul₁ r xs c =
         Map.foldrWithKey
           (\rxs r (NF acc) →
                NF $ Map.insert
                     (rxs `Set.union` xs)
                     (c ⊗ r)
                     acc)
           r₀ r


evalNF :: (DLattice a, DLattice b) ⇒ (a → b) → (x → b) → NormalForm x a → b
evalNF h e (NF xss) =
  Map.foldrWithKey
    (\xs a sum →
       sum ⊕ Set.foldr
                 (\x prod → prod ⊗ (e x))
                 (h a)
                 xs)
    r₀
    xss

initNF :: DLattice r ⇒ Bool → r
initNF False = r₀
initNF True = r₁

instance DLattice Bool where (⊕) = (||); (⊗) = (&&); r₀ = False; r₁ = True

instance Ord x ⇒ Free DLattice x where
   newtype FreeA DLattice x = DLatticeA (NormalForm x Bool)
      deriving (DLattice, Eq)
   pvar x = DLatticeA (NF (Map.singleton (Set.singleton x) True)) -- True × x
   DLatticeA xss `pbind` f = evalNF initNF f xss


-- commutative monoid instances for ⊕ and ⊗
newtype DLatticeAdd n = DA { da :: n }
newtype DLatticeMul n = DM { dm :: n }

instance DLattice r ⇒ Semigroup (DLatticeAdd r) where  (DA x) <> (DA y) = DA (x ⊕ y)
instance DLattice r ⇒ Monoid (DLatticeAdd r) where mempty = DA r₀ ; mappend (DA x) (DA y) = DA (x ⊕ y)
instance DLattice r ⇒ CMonoid (DLatticeAdd r)
instance DLattice r ⇒ Semigroup (DLatticeMul r) where (DM x) <> (DM y) = DM (x ⊗ y)
instance DLattice r ⇒ Monoid (DLatticeMul r) where mempty = DM r₁ ; mappend (DM x) (DM y) = DM (x ⊗ y)
instance DLattice r ⇒ CMonoid (DLatticeMul r)
