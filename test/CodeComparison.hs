{-# LANGUAGE FlexibleInstances #-}

module CodeComparison where

import Data.PartiallyStatic (Code)
import Control.Monad (liftM)
import Language.Haskell.TH (runQ, pprint, unType, examineCode)

import System.IO.Unsafe (unsafePerformIO)

-- We use code values as keys in ordered containers such as maps.
-- It would be a little more principled to define a new type of
-- code values that support comparison, and generate a fresh id
-- with each injection into the type.  But this does the trick for now.

unsafeStringOf :: Code α → String
unsafeStringOf x = unsafePerformIO $ 
                   (liftM pprint $ runQ (liftM unType (examineCode x)))

instance Eq (Code α) where x == y = unsafeStringOf x == unsafeStringOf y
instance Ord (Code α)  where x <= y = unsafeStringOf x <= unsafeStringOf y
