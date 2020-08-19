{-# LANGUAGE NPlusKPatterns, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Power where

import Data.Monoid
import Data.CMonoid
import Data.Coproduct
import InstanceLifting
import Language.Haskell.TH.Syntax
import Data.PartiallyStatic
import Control.Monad (liftM)
import Data.MultiSet

deriving instance (Integral n) ⇒ Integral (Product n)
deriving instance Real n ⇒ Real (Product n)
deriving instance Enum n ⇒ Enum (Product n)

-- A destructor that does better than the default.
-- (Could we make this the default?  'eva' doesn't know about
--  code and 'PartiallyStatic.cd' doesn't know about CMonoid).
cdPower :: FreeExt CMonoid (Code (Product Int)) (Product Int) -> Code Int
cdPower (C 0 (CM r)) = [|| 0 ||]
cdPower (C 1 (CM r)) = powerVn (toOccurList r) (\x -> [|| getProduct $$x ||])
cdPower (C l (CM r)) = powerVn (toOccurList r) (\x -> [|| getProduct (l * $$x) ||])


-- reducing multiplications for one variable
powerV1 :: (Integral a, Lift a) => Int -> Code a -> (Code a -> Code b) -> Code b
powerV1 0 x k = k [|| 1 ||]
powerV1 1 x k = k x
powerV1 n x k | even n = [|| let y = $$x * $$x in
                            $$(powerV1 (n `div` 2) [||y||] k) ||]
             | otherwise = [|| let y = $$x * $$x in
                            $$(powerV1 ((n - 1) `div` 2) [||y||]
                              (\z -> k [|| $$x * $$z ||]))||]

-- reducing multiplications for multiple variables
powerVn :: (Integral a, Lift a) => [(Code a, Int)] -> (Code a -> Code b) -> Code b
powerVn [] k = k [|| 1 ||]
powerVn [(x, n)] k = powerV1 n x k
powerVn ((x,n):xs) k = powerV1 n x $ \y ->
                       powerVn xs $ \z ->
                       k [|| $$y * $$z ||]

-- the example code itself
power :: CMonoid i ⇒ i → Int → i
power x 0 = mempty
power x (n+1) = x `mappend` power x n
