{-# LANGUAGE FlexibleContexts #-}
module LinAlg where

import Data.List (transpose)
import Prelude hiding ((<*>))
import Data.Ring
import Data.Coproduct
import Data.PartiallyStatic
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Language.Haskell.TH.Syntax (Lift)

type PsIntRing = FreeExt Ring Int

dot :: Ring r ⇒ [r] → [r] → r
dot xs ys = sumr (zipWith (⊗) xs ys)

sumr :: Ring r ⇒ [r] → r
sumr = foldr (⊕) r₀

cdNumRing :: (Num n, Eq n, Ring (Code n), Lift n) ⇒ FreeExt Ring n → Code n
cdNumRing (CR (MN m)) = cd $ sumBits $ Map.assocs m
 where
  sumBits = foldr (\(xs, c) r → (prodVars xs `prod2` sta c) `sum2` r) (sta 0)
  prodVars = prodN . map dyn . MultiSet.elems

-- TODO: we could simplify further here, reducing the number of multiplications as for 'power'
prodN :: (Num n, Eq n, Lift n) ⇒ [FreeExt Set n] → FreeExt Set n
prodN = foldr prod2 (sta 1)

sum2 :: (Num n, Eq n, Lift n) ⇒ FreeExt Set n → FreeExt Set n → FreeExt Set n
sum2 (Inl 0) r = r
sum2 l (Inl 0) = l
sum2 (Inl l) (Inl r) = sta (l + r)
sum2 l r = dyn [|| $$(cd l) + $$(cd r) ||]

prod2 :: (Num n, Eq n, Lift n) ⇒ FreeExt Set n → FreeExt Set n → FreeExt Set n
prod2 _ (Inl 0) = sta 0
prod2 (Inl 0) _ = sta 0
prod2 x (Inl 1) = x
prod2 (Inl 1) y = y
prod2 l r = dyn [|| $$(cd l) * $$(cd r) ||]


-- matrix-matrix multiplication
mmmul :: Ring r ⇒ [[r]] → [[r]] → [[r]]
mmmul m n = [[dot a b | b <- transpose n] | a <- m]
