{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Data.PartiallyStatic where
import Data.Coproduct
import Language.Haskell.TH.Syntax
import Control.Monad (liftM)

-- Variables 
type Code a = Q (TExp a) -- (for now; more elaborate later)

-- Free extension
type FreeExt algebra α = Coprod algebra α (FreeA algebra (Code α))

type FreeExtCon algebra α =
  Coproduct algebra α (FreeA algebra (Code α))

sta :: (algebra α, FreeExtCon algebra α) ⇒
       α → FreeExt algebra α
sta = inl

dyn :: (Free algebra (Code α), FreeExtCon algebra α) ⇒
       Code α → FreeExt algebra α
dyn = inr . pvar

cd :: (Lift α, Free algebra (Code α), algebra (Code α),
       FreeExtCon algebra α) ⇒
      FreeExt algebra α → Code α
cd = eva tlift (`pbind` id)

tlift :: Lift α ⇒ α → Q (TExp α)
tlift = liftM TExp . lift
