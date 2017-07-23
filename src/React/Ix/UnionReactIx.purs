module React.Ix.UnionReactIx where

import Control.Monad.Eff (kind Effect)
import Prelude (Unit, (*>), (<$>))
import React.Ix (ReactThisIx)
import React.Ix.EffR (EffR(..))
import Type.Row (RProxy)
import Unsafe.Coerce (unsafeCoerce)

class UnionReactIx (e :: # Effect) (i1 :: # Type) (o1 :: # Type) a1 (i2 :: # Type) (o2 :: # Type) a2 (ui :: # Type) (uo :: # Type) ua where
  union :: Union i1 i2 ui => Union o1 o2 uo => EffR e (RProxy i1) (RProxy o1) a1 -> EffR e (RProxy i2) (RProxy o2) a2 -> EffR e (RProxy ui) (RProxy uo) ua

instance unionReactIxUnit :: UnionReactIx e i1 o1 Unit i2 o2 Unit ui uo Unit  where
  union (EffR m1) (EffR m2) = EffR (m1 *> m2)

instance unionReactIxReactThixIx :: UnionReactIx e i1 o1 (ReactThisIx p s o1) i2 o2 (ReactThisIx p s o2) ui uo (ReactThisIx p s uo) where
  union (EffR m1) (EffR m2) = EffR (m1 *> (coerceIx <$> m2))
    where
      coerceIx :: ReactThisIx p s o2 -> ReactThisIx p s uo
      coerceIx = unsafeCoerce

infixr 5 union as :<>
