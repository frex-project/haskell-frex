{-# LANGUAGE MultiParamTypeClasses, UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, TypeFamilies #-}

module Data.Coproduct.Classes where

{-- The coproduct interface --}
class (algebra α, algebra β, algebra (Coprod algebra α β)) ⇒ Coproduct algebra α β where
   data family Coprod algebra α β :: ★
   inl :: α → Coprod algebra α β
   inr :: β → Coprod algebra α β
   eva :: algebra δ ⇒ (α → δ) → (β → δ) → Coprod algebra α β → δ

{-- Free algebras with variables in var --}
class algebra (FreeA algebra var) ⇒ Free algebra var where
  data family FreeA algebra var :: *

  pvar :: var → FreeA algebra var
  pbind :: algebra c ⇒ FreeA algebra var → (var → c) → c

