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
   Code x <> Code  y = Code [|| $$x `mappend` $$y ||]
  
instance {-# OVERLAPS #-} Monoid m ⇒ Monoid (Code m) where
   mempty = Code [|| mempty ||]
   
instance {-# OVERLAPS #-} Semigroup (Code String) where
   Code x <> Code y = Code [|| $$x ++ $$y ||] 

instance {-# OVERLAPS #-} Monoid (Code String) where
   mempty = Code [|| "" ||]

instance CGroup (Code Int) where
  cinv (Code x) = Code [|| - $$x ||]
instance CMonoid (Code Int) where
  
instance Semigroup (Code Int) where
  Code x <> Code y = Code [|| $$x + $$y ||]

instance Monoid (Code Int) where
  mempty = Code [|| 0 ||]

instance CMonoid (Code Bool) where
instance CGroup (Code Bool) where
  cinv = id
instance Semigroup (Code Bool) where
  Code x <> Code y = Code [|| $$x && $$y ||]
instance Monoid (Code Bool) where
  mempty = Code [|| True ||]

instance BoolRing (Code Bool)

instance Ring (Code Int) where
  Code x ⊕ Code y = Code [|| $$x + $$y ||]
  Code x ⊗ Code y = Code [|| $$x * $$y ||]
  rneg (Code x) = Code [|| - $$x ||]
  r₀ = Code [|| 0 ||]
  r₁ = Code [|| 1 ||]

instance Ring (Code Bool) where
  Code x ⊕ Code y = Code [|| $$x /= $$y ||]
  Code x ⊗ Code y = Code [|| $$x && $$y ||]
  rneg (Code x) = Code [|| $$x ||]
  r₀ = Code [|| False ||]
  r₁ = Code [|| True ||]

instance DLattice (Code Bool) where
  Code x ⊕ Code y = Code [|| $$x || $$y ||]
  Code x ⊗ Code y = Code [|| $$x && $$y ||]
  r₀ = Code [|| False ||]
  r₁ = Code [|| True ||]
