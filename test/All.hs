{-# LANGUAGE TemplateHaskell, DeriveLift #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module All where

import qualified Prelude
import Prelude hiding (all, any)
import InstanceLifting()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.PartiallyStatic
import Data.Coproduct
import Data.DLattice
import CodeComparison()

all :: (a → Bool, Code (a → Bool)) → FreeExt Monoid (Code [a]) [a] → FreeExt DLattice (Code Bool) Bool
all (f, f') c = dm (eva (DM . inl . g) (DM . inr . h) c)
  where g x | Prelude.all f x = r₁
            | otherwise = r₀
        h = pvar . (`pbind` (\x → [|| Prelude.all $$f' $$x ||]))

any :: (a → Bool, Code (a → Bool)) → FreeExt Monoid (Code [a]) [a] → FreeExt DLattice (Code Bool) Bool
any (f, f') c = da (eva (DA . inl . g) (DA . inr . h) c)
  where g x | Prelude.any f x = r₁
            | otherwise = r₀
        h = pvar . (`pbind` (\x → [|| Prelude.any $$f' $$x ||]))


-- optimized cd for Booleans
cdBool :: FreeExt DLattice (Code Bool) Bool → Code Bool
cdBool (DL (NF p)) = cd (cdBool' (map Set.elems (Map.keys (Map.filter (== True) p))))
 where
  cdBool' [] = sta False
  cdBool' (xs : xss) = foldr (\p sum → sum `or` term p) (term xs) xss
  or (Inl True) _ = Inl True
  or _ (Inl True) = Inl True
  or (Inl False) y = y
  or x (Inl False) = x
  or (Inr (F x)) (Inr (F y)) = Inr (F (x ⊕ y))
  term [] = sta True
  term (x : xs) = dyn (foldr (⊗) x xs)

