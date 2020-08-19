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
cdPower :: FreeExt CMonoid Code (Product Int) -> Code Int
cdPower (C 0 (CM r)) = Code [|| 0 ||]
cdPower (C 1 (CM r)) = powerVn (toOccurList r)
                       (\(Code x) -> Code [|| getProduct $$x ||])
cdPower (C l (CM r)) = powerVn (toOccurList r)
                       (\(Code x) -> Code [|| getProduct (l * $$x) ||])


-- reducing multiplications for one variable
powerV1 :: (Integral a, Lift a) => Int -> Code a -> (Code a -> Code b) -> Code b
powerV1 0 x k = k (Code [|| 1 ||])
powerV1 1 x k = k x
powerV1 n (Code x) k | even n = Code [|| let y = $$x * $$x in
                                         $$(code (powerV1 (n `div` 2) (Code [||y||]) k)) ||]
                     | otherwise = Code [|| let y = $$x * $$x in
                                            $$(code (powerV1 ((n - 1) `div` 2) (Code [||y||])
                                               (\(Code z) ->
                                                   k (Code [|| $$x * $$z ||]))))||]

-- reducing multiplications for multiple variables
powerVn :: (Integral a, Lift a) => [(Code a, Int)] -> (Code a -> Code b) -> Code b
powerVn [] k = k (Code [|| 1 ||])
powerVn [(x, n)] k = powerV1 n x k
powerVn ((x,n):xs) k = powerV1 n x $ \(Code y) ->
                       powerVn xs $ \(Code z) ->
                       k (Code [|| $$y * $$z ||])

-- the example code itself
power :: CMonoid i ⇒ i → Int → i
power x 0 = mempty
power x (n+1) = x `mappend` power x n
