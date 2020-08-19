module IsDigit where

import Data.Coproduct
import Data.PartiallyStatic
import qualified Data.Char as Char

isDigit :: FreeExt Set (Code Char) Char → FreeExt Set (Code Bool) Bool
isDigit = eva (sta . Char.isDigit)
              ((\(F x) → dyn [|| Char.isDigit $$x ||]))
