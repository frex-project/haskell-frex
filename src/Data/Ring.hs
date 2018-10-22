{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ring where

import Data.CMonoid
import Data.Coproduct.Classes (Free(..))
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet

-- Commutative rings
-- This is essentially Num without signum.  Perhaps we should just use Num instead
class Ring a where
  (⊕), (⊗) :: a → a → a
  rneg :: a → a
  r₀, r₁ :: a

-- c₁xᵃyᵇ + c₂xᵃyᵇ + … + cₙxᵃyᵇ
-- multinomials with variables in x and coefficients in a
newtype Multinomial x a = MN (Map.Map (MultiSet.MultiSet x) a)
  deriving (Show)

instance (Ord x, Ring a) ⇒ Ring (Multinomial x a) where
  r₁ = MN (Map.singleton MultiSet.empty r₁)
  MN l ⊗ MN r =
    -- (l₁ + l₂ + … + lₙ) × r  ≡  l₁×r + l₂×r + … + lₙ×r
    Map.foldrWithKey (\xs c acc → acc ⊕ mul₁ r xs c) r₀ l
     where 
       -- cxᵢʲ(r₁' + r₂ + … + rₘ)  ≡  cxᵢʲr₁' + cxᵢʲr₂ + … + cxᵢʲrₘ
       mul₁ r xs c =
         Map.foldrWithKey
           (\rxs r (MN acc) →
               MN $ Map.insert
                     (rxs `MultiSet.union` xs)
                     (c ⊗ r)
                     acc)
           r₀ r
  r₀ = MN Map.empty
  MN l ⊕ MN r = MN (Map.unionWith (⊕) l r)
  rneg (MN m) {- - -} = MN (Map.map rneg m)

evalMN :: (Ring a, Ring b) ⇒ (a → b) → (x → b) → Multinomial x a → b
evalMN h e (MN xss) =
  Map.foldrWithKey
    (\xs a sum →
       sum ⊕ MultiSet.foldOccur
               (\x n prod → prod ⊗ (e x ^^ n))
               (h a)
               xs)
    r₀
    xss
  where a ^^ 0 = r₁ -- 1
        a ^^ 1 = a
        a ^^ n | odd n     = let x = a ^^ ((n-1) `div` 2)
                             in (x ⊗ x) ⊗ a
               | otherwise = let x = a ^^ ( n    `div` 2)
                             in x ⊗ x

initMN :: Ring r ⇒ Int → r
initMN 0 = r₀
initMN 1 = r₁
initMN n | n < 0 = rneg (initMN (-n))
         | odd n     = two ⊗ initMN (n `div` 2) ⊕ r₁
         | otherwise = two ⊗ initMN (n `div` 2)
  where two = r₁ ⊕ r₁

instance Ord x ⇒ Free Ring x where
   newtype FreeA Ring x = RingA (Multinomial x Int)
      deriving (Ring)
   pvar x = RingA (MN (Map.singleton (MultiSet.singleton x) r₁)) -- 1×x¹
   RingA xss `pbind` f = evalMN initMN f xss

instance Ring Int where (⊕) = (+); (⊗) = (*); rneg = negate; r₀ = 0; r₁ = 1
instance Ring Double where (⊕) = (+); (⊗) = (*); rneg = negate; r₀ = 0; r₁ = 1

-- commutative monoid instances for ⊕ and ⊗
newtype RingAdd n = RA { ra :: n }
newtype RingMul n = RM { rm :: n }

instance Ring r ⇒ Monoid (RingAdd r) where mempty = RA r₀ ; mappend (RA x) (RA y) = RA (x ⊕ y)
instance Ring r ⇒ CMonoid (RingAdd r)
instance Ring r ⇒ Monoid (RingMul r) where mempty = RM r₁ ; mappend (RM x) (RM y) = RM (x ⊗ y)
instance Ring r ⇒ CMonoid (RingMul r)

