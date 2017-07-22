module React.Ix.EffR where

import Control.IxMonad (class IxMonad)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Newtype (class Newtype)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ap, pure, (<$>), (<<<), (>>=))
import Type.Prelude (RProxy)

-- | Indexed monad that track changes of types i o.  Note that here we are
-- | tracking changes on the type level while in `ReactThisIx` we are tracking
-- | changes using `# Type` kind.
newtype EffR (e :: # Effect) i o a = EffR (Eff e a)

derive instance newtypeEffR :: Newtype (EffR e (RProxy i) (RProxy o) a) _

instance functorEffR :: Functor (EffR e (RProxy i) (RProxy o)) where
  map f (EffR m) = EffR (f <$> m)

instance applyEffR :: Apply (EffR e (RProxy i) (RProxy i)) where
  apply = ap

instance applicativeEff :: Applicative (EffR e (RProxy i) (RProxy i)) where
  pure = EffR <<< pure

instance bindEffR :: Bind (EffR e (RProxy i) (RProxy i)) where
  bind (EffR m) f = EffR (m >>= \a -> case f a of EffR m' -> m')

instance monadEffR :: Monad (EffR e (RProxy i) (RProxy i))

instance monadEffEffR :: MonadEff e (EffR e (RProxy i) (RProxy i)) where
  liftEff = EffR

instance ixMonadEffR :: IxMonad (EffR e) where
  ipure a = EffR (pure a)
  ibind (EffR m) f = EffR (m >>= \a -> case f a of EffR m' -> m')

unsafePerformEffR :: forall e i o a. EffR e (RProxy i) (RProxy o) a -> a
unsafePerformEffR (EffR m) = unsafePerformEff m
