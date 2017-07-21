module React.Ix.EffR where

import Control.IxMonad (class IxMonad)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Newtype (class Newtype)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ap, pure, (<$>), (<<<), (>>=))

newtype EffR (e :: # Effect) i o a = EffR (Eff e a)

derive instance newtypeEffR :: Newtype (EffR e i o a) _

instance functorEffR :: Functor (EffR e i o) where
  map f (EffR m) = EffR (f <$> m)

instance applyEffR :: Apply (EffR e i i) where
  apply = ap

instance applicativeEff :: Applicative (EffR e i i) where
  pure = EffR <<< pure

instance bindEffR :: Bind (EffR e i i) where
  bind (EffR m) f = EffR (m >>= \a -> case f a of EffR m' -> m')

instance monadEffR :: Monad (EffR e i i)

instance monadEffEffR :: MonadEff e (EffR e i i) where
  liftEff = EffR

instance ixMonadEffR :: IxMonad (EffR e) where
  ipure a = EffR (pure a)
  ibind (EffR m) f = EffR (m >>= \a -> case f a of EffR m' -> m')

unsafePerformEffR :: forall e i o a. EffR e i o a -> a
unsafePerformEffR (EffR m) = unsafePerformEff m
