{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Data.PartiallyStatic where
import Data.Coproduct
import Language.Haskell.TH.Syntax hiding (Code)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad (liftM)

type Code a = TH.Code Q a

-- Free extension
type FreeExt algebra name α = Coprod algebra α (FreeA algebra name)

type FreeExtCon algebra name α =
  Coproduct algebra α (FreeA algebra name)

sta :: (algebra α, FreeExtCon algebra name α) ⇒
       α → FreeExt algebra name α
sta = inl

dyn :: (Free algebra name, FreeExtCon algebra name α) ⇒
       name → FreeExt algebra name α
dyn = inr . pvar

cd :: (Lift α, Free algebra (Code α), algebra (Code α),
       FreeExtCon algebra (Code α) α) ⇒
      FreeExt algebra (Code α) α → Code α
cd = eva tlift (`pbind` id)

tlift :: Lift α ⇒ α → Code α
tlift = liftCode . liftM TExp . lift
