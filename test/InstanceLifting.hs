{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveLift #-}

module InstanceLifting where

import Data.Ring
import Data.BoolRing
import Data.DLattice
import Data.CGroup
import Data.CMonoid
import Data.Monoid
import Data.PartiallyStatic
import CodeComparison()
import Language.Haskell.TH.Syntax (Lift)

deriving instance Lift a ⇒ Lift (Sum a)
deriving instance Lift a ⇒ Lift (Product a)

instance {-# OVERLAPS #-} Monoid m ⇒ Monoid (Code m) where
   mempty = [|| mempty ||]
   x `mappend` y = [|| $$x `mappend` $$y ||] 

instance {-# OVERLAPS #-} Monoid (Code String) where
   mempty = [|| "" ||]
   x `mappend` y = [|| $$x ++ $$y ||] 

instance CGroup (Code Int) where
  cinv x = [|| - $$x ||]
instance CMonoid (Code Int) where
instance Monoid (Code Int) where
  mempty = [|| 0 ||]
  mappend x y = [|| $$x + $$y ||]

instance CMonoid (Code Bool) where
instance CGroup (Code Bool) where
  cinv = id
instance Monoid (Code Bool) where
  mempty = [|| True ||]
  mappend x y = [|| $$x && $$y ||]
instance BoolRing (Code Bool)

instance Ring (Code Int) where
  x ⊕ y = [|| $$x + $$y ||]
  x ⊗ y = [|| $$x * $$y ||]
  rneg x = [|| - $$x ||]
  r₀ = [|| 0 ||]
  r₁ = [|| 1 ||]

instance Ring (Code Bool) where
  x ⊕ y = [|| $$x /= $$y ||]
  x ⊗ y = [|| $$x && $$y ||]
  rneg x = [|| $$x ||]
  r₀ = [|| False ||]
  r₁ = [|| True ||]

instance DLattice (Code Bool) where
  x ⊕ y = [|| $$x || $$y ||]
  x ⊗ y = [|| $$x && $$y ||]
  r₀ = [|| False ||]
  r₁ = [|| True ||]
