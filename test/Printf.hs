{-# LANGUAGE TypeFamilies #-}

-- PartialTypeSignatures are useful for type-based
-- selection of sprintf implementation
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Printf where

import Data.PartiallyStatic
import Data.Coproduct
import InstanceLifting()

class Format f where
  type Acc f α :: ★
  lit :: String → f a a
  cat :: f b a → f c b → f c a
  int :: f a (Acc f Int → a)
  str :: f a (Acc f String → a)
  sprintf :: f (Acc f String) a → a

-- unstaged printf
newtype Fmt r a = Fmt {fmt :: (String → r) → (String → a)}

instance Format Fmt where
  type Acc Fmt α = α
  lit x = Fmt $ \k s → k (s ++ x)
  f `cat` g = Fmt (fmt f . fmt g)
  int = Fmt $ \k s x → k (s ++ show x)
  str = Fmt $ \k s x → k (s ++ x)
  sprintf p = fmt p id ""

-- staged printf
newtype FmtS r a = FmtS {fmtS :: (Code String → r) → (Code String → a)}

instance Format FmtS where
  type Acc FmtS α = Code α
  lit x = FmtS $ \k (Code s) → k (Code [|| $$s ++ x ||])
  f `cat` g = FmtS (fmtS f . fmtS g)
  int = FmtS $ \k (Code s) (Code x) → k (Code [|| $$s ++ show $$x ||])
  str = FmtS $ \k (Code s) (Code x) → k (Code [|| $$s ++ $$x ||])
  sprintf p = fmtS p id (Code [|| "" ||])

-- partially-static printf
newtype FmtPS r a = FmtPS { fmtPS :: (FreeExt Monoid Code String → r) →
                                     (FreeExt Monoid Code String → a) }

instance Format FmtPS where
  type Acc FmtPS α = Code α
  lit x = FmtPS $ \k s → k (s `mappend` sta x)
  f `cat` g = FmtPS (fmtPS f . fmtPS g)
  int = FmtPS $ \k s (Code x) → k (s `mappend` dyn (Code [|| show $$x ||]))
  str = FmtPS $ \k s x → k (s `mappend` dyn x)
  sprintf (FmtPS p) = p cd mempty

-- partially-static printf with n-ary destructor function
newtype FmtPS2 r a = FmtPS2 { fmtPS2 :: (FreeExt Monoid Code String → r) →
                                        (FreeExt Monoid Code String → a) }

instance Format FmtPS2 where
  type Acc FmtPS2 α = Code α
  lit x = FmtPS2 $ \k s → k (s `mappend` sta x)
  f `cat` g = FmtPS2 (fmtPS2 f . fmtPS2 g)
  int = FmtPS2 $ \k s (Code x) → k (s `mappend` dyn (Code [|| show $$x ||]))
  str = FmtPS2 $ \k s x → k (s `mappend` dyn x)
  sprintf (FmtPS2 p) = p cdStrings mempty

exampleFmt :: Format f ⇒ f a (Acc f Int → Acc f Int → a)
exampleFmt = (int `cat` lit "a") `cat` (lit "b" `cat` int)

unstagedExample :: Int → Int → String
unstagedExample = sprintf (exampleFmt :: Fmt _ _)

stagedCode :: Code (Int → Int → String)
stagedCode = Code [|| \x y → $$(code (sprintf (exampleFmt :: FmtS _ _) (Code [||x||]) (Code [||y||]))) ||]

psCode :: Code (Int → Int → String)
psCode = Code [|| \x y → $$(code (sprintf (exampleFmt :: FmtPS _ _) (Code [||x||]) (Code [||y||]))) ||]

-- a destructor for strings that generates a single n-ary concatenation
cdStrings :: FreeExt Monoid Code String -> Code String
cdStrings m = Code [|| Prelude.concat $$(code (liftList (eva g h m))) ||]
  where g "" = []
        g s = [Code [||s||]]
        h (P s) = s

liftList :: [Code a] -> Code [a]
liftList [] = Code [||[]||]
liftList (Code x:xs) = Code [||$$x : $$(code (liftList xs))||]

sprintfN (FmtPS p) = p cdStrings mempty

psCodeN :: Code (Int → Int → String)
psCodeN = Code [|| \x y → $$(code (sprintfN exampleFmt (Code [||x||]) (Code [||y||]))) ||]

benchFmt :: Format f => f c (Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> Acc f String
                          -> c)
benchFmt = (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           (str `cat` lit "," `cat` lit " ")
           `cat`
           str
