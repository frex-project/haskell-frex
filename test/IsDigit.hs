module IsDigit where

import Data.Coproduct
import Data.PartiallyStatic
import qualified Data.Char as Char

isDigit :: FreeExt Set Char → FreeExt Set Bool
isDigit = eva (sta . Char.isDigit)
              ((\(F x) → dyn [|| Char.isDigit $$x ||]))
