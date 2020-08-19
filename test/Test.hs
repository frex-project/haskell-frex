module Main where

import System.Exit
import Control.Monad (liftM, when)
import Language.Haskell.TH (runQ, pprint, unType)
import Language.Haskell.TH.Syntax
import Data.PartiallyStatic
import Data.Monoid
import Data.CMonoid
import Data.Ring
import Data.BoolRing
import qualified Data.Char as Char
import qualified Printf
import qualified Power
import qualified LinAlg
import qualified All
import qualified Datatypes
import qualified IsDigit
import Datatypes (PSIList1 (..), cons2, nil2, dyn2, cons2', nil2', cons3, nil3, cons3', nil3', dyn3)

showCode ::  Code a → IO String
showCode (Code c) = do e ← runQ (liftM unType c) ; return $ pprint e

printCode :: String -> Code a → IO ()
printCode description c = do
  e ← showCode c
  putStrLn $ "  " ++ description ++ "\n"
  putStrLn $ "    " ++ e ++ "\n"

assertEqual :: (Eq a, Show a) ⇒ a → a → IO ()
assertEqual x y =
  when (x /= y) $ do
  putStrLn $ "FAIL: " ++ show x ++ " /= " ++ show y
  exitWith (ExitFailure 1)

startSection :: String -> IO ()
startSection s = putStrLn $ "** " ++ s ++ " **\n"
endSection :: IO ()
endSection = sequence_ $ replicate 1 $ putStrLn ""

main :: IO ()
main = do

  startSection "Printf"

  -- Running the unstaged version
  assertEqual (Printf.unstagedExample 3 4) "3ab4"

  -- With the standard staged printf the generated code contains 4
  -- catenations
  printCode "Naive staging (four catenations)"
    Printf.stagedCode

  -- With the partially-static printf the generated code contains 2
  -- catenations
  printCode "Staging with partially-static data (two catenations)"
    Printf.psCode

  -- With the partially-static printf and a custom residualization
  -- function the generated code contains a single n-ary catenation,
  -- equivalent to
  --
  --   \x y -> concat [show x, "ab", show y]
  printCode "Staging with partially-static data (single nary catenation)"
    Printf.psCodeN
  endSection



  startSection "Power"

  assertEqual (Power.power 3 4 :: Product Int) 81
  assertEqual (Power.power 3 7 :: Product Int) 2187

  printCode "Naive staging (7 multiplications)"
    (Power.power (Code [|| 3 ||] :: Code Int) 7 )

  printCode "Staging with partially-static data (4 multiplications)"
    (Power.cdPower (Power.power (dyn (Code [|| Product 3 ||])) 7))

  printCode "combining separated calls to 'power' (4 multiplications)"
    (Code [|| \x' y' ->
         $$(let x = dyn (Code [||x'||]) ; y = dyn (Code [||y'||]) in
            code $ Power.cdPower
             (Power.power x 3 `mappend` y `mappend` Power.power x 3)) ||])


  endSection



  startSection "Inner product"

  printCode "Staging with partially-static data"
    (Code [|| \y₁ y₂ y₃ → 
         $$(let v₁ = [sta 0,         sta 3       , sta 1        ]
                v₂ = [dyn (Code [||y₁||]) , dyn (Code [||y₂||]), dyn (Code [||y₃||]) ]
             in code $ LinAlg.cdNumRing $ (LinAlg.dot v₁ v₂ :: FreeExt Ring Code Int)) ||])

  endSection



  startSection "All and any"

  printCode "Staging 'all' with partially-static data"
    (Code [|| \x →
         $$(code $ All.cdBool (All.all (even, Code [||even||])
                (sta [2,4] `mappend` dyn (Code [||x||]) `mappend` sta [3]))) ||])

  printCode "Staging 'all' with more partially-static data"
    (Code [|| \x →
         $$(code $ All.cdBool (All.all (even, Code [||even||])
                (sta [2,4] `mappend` dyn (Code [||x||]) `mappend` sta [2]))) ||])

  printCode "Staging 'any' with partially-static data (1)"
    (Code [|| \x →
         $$(code $ All.cdBool (All.any (even, Code [||even||])
                (sta [2,4] `mappend` dyn (Code [||x||]) `mappend` sta [3]))) ||])

  printCode "Staging 'any' with partially-static data (2)"
    (Code [|| \x →
         $$(code $ All.cdBool (All.any (even, Code [||even||])
                (sta [1,3] `mappend` dyn (Code [||x||]) `mappend` sta [2] `mappend` dyn (Code [||x||])))) ||])

  printCode "Staging 'any' with partially-static data (3)"
    (Code [|| \x →
         $$(code $ All.cdBool (All.any (even, Code [||even||])
                (sta [1,3] `mappend` dyn (Code [||x||]) `mappend` sta [3]))) ||])

  endSection



  startSection "Possibly-static data"

  printCode "IsDigit with a static argument"
    (cd (IsDigit.isDigit (sta '3')))

  printCode "IsDigit with a dynamic argument"
    (Code [|| \x -> $$(code $ cd (IsDigit.isDigit (dyn (Code [||x||])))) ||])

  endSection


  startSection "Partially-static algebraic datatypes"

  let psList1 = 1 `Cons1` 2 `Cons1` 3 `Cons1` Dyn1 (Code [|| [4, 5, 6] ||])
  printCode "Staging with partially-static data (concrete)"
    (cd (Datatypes.sum1 psList1))

  let psList2 = 1 `cons2` 2 `cons2` 3 `cons2` dyn2 (Code [|| 4 `cons2'` 5 `cons2'` 6 `cons2'` nil2' ||])
  printCode "Staging with partially-static data (open-recursive / F-algebra)"
    (cd (Datatypes.sum2 psList2))

  let psList3 = 1 `cons3` 2 `cons3` 3 `cons3` dyn3 (Code [|| 4 `cons3'` 5 `cons3'` 6 `cons3'` nil3' ||])
  printCode "Staging with partially-static data (coproducts)"
    (cd (Datatypes.sum3 psList3))
  
  endSection
