{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Data.PartiallyStatic where
import Data.Coproduct
import Language.Haskell.TH.Syntax
import Control.Monad (liftM)

newtype Code a = Code { code :: Q (TExp a) }

-- Free extension
type FreeExt algebra name α = Coprod algebra α (FreeA algebra (name α))

type FreeExtCon algebra name α =
  Coproduct algebra α (FreeA algebra (name α))

sta :: (algebra α, FreeExtCon algebra name α) ⇒
       α → FreeExt algebra name α
sta = inl

dyn :: (Free algebra (name α), FreeExtCon algebra name α) ⇒
       name α → FreeExt algebra name α
dyn = inr . pvar

cd :: (Lift α, Free algebra (Code α), algebra (Code α),
       FreeExtCon algebra Code α) ⇒
      FreeExt algebra Code α → Code α
cd = eva tlift (`pbind` id)

tlift :: Lift α ⇒ α → Code α
tlift = Code . liftM TExp . lift
