module IsDigit where

import Data.Coproduct
import Data.PartiallyStatic
import qualified Data.Char as Char

isDigit :: FreeExt Set Code Char → FreeExt Set Code Bool
isDigit = eva (sta . Char.isDigit)
              ((\(F (Code x)) → dyn (Code [|| Char.isDigit $$x ||])))
