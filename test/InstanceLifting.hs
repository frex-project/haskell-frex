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
import Language.Haskell.TH.Lift

$(deriveLift ''Sum)
$(deriveLift ''Product)

-- deriving instance Lift a ⇒ Lift (Sum a)
-- deriving instance Lift a ⇒ Lift (Product a)

instance {-# OVERLAPS #-} Monoid m ⇒ Semigroup (Code m) where
   x <> y = [|| $$x `mappend` $$y ||]
  
instance {-# OVERLAPS #-} Monoid m ⇒ Monoid (Code m) where
   mempty = [|| mempty ||]
   
instance {-# OVERLAPS #-} Semigroup (Code String) where
   x <> y = [|| $$x ++ $$y ||] 

instance {-# OVERLAPS #-} Monoid (Code String) where
   mempty = [|| "" ||]

instance CGroup (Code Int) where
  cinv x = [|| - $$x ||]
instance CMonoid (Code Int) where
  
instance Semigroup (Code Int) where
  x <> y = [|| $$x + $$y ||]

instance Monoid (Code Int) where
  mempty = [|| 0 ||]

instance CMonoid (Code Bool) where
instance CGroup (Code Bool) where
  cinv = id
instance Semigroup (Code Bool) where
  x <> y = [|| $$x && $$y ||]
instance Monoid (Code Bool) where
  mempty = [|| True ||]

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
