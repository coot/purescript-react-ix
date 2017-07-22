module React.Ix
  ( class Subrow
  , getProp
  , getPropIx
  , setProp
  , setPropIx
  , insertPropIx
  , nullifyPropIx

  , RenderIx
  , GetInitialStateIx
  , ComponentWillMountIx
  , ComponentDidMountIx
  , ComponentWillReceivePropsIx
  , ShouldComponentUpdateIx
  , ComponentWillUpdateIx
  , ComponentDidUpdateIx
  , ComponentWillUnmountIx

  , ReactThisIx(..)
  , ReactSpecIx

  , specIx
  , specIx'

  , toReactSpec
  , fromReactSpec
  , createClassIx

  , refFn
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn2, runEffFn3)
import Data.Symbol (reflectSymbol)
import Prelude (Unit, pure, unit, void, ($), ($>), (<$>))
import React (Disallowed, ReactClass, ReactElement, ReactProps, ReactRefs, ReactSpec, ReactState, ReactThis, ReadOnly, ReadWrite, createClass)
import React.DOM.Props (Props, unsafeMkProps)
import React.Ix.EffR (EffR(..), unsafePerformEffR)
import Type.Data.Symbol (class IsSymbol, SProxy)
import Type.Row (class RowLacks)

newtype ReactThisIx p s (r :: # Type) = ReactThisIx (ReactThis p s)

class Subrow (r :: # Type) (s :: # Type)
instance srInst :: Union r t s => Subrow r s

foreign import unsafeGetImpl :: forall a b eff. EffFn2 eff String a b

-- | Safe way of reading a property. Note that if you set a callback within a
-- | life-cycle method where the property is defined, and it will be called
-- | after the component is unmounted this might return undefined (if
-- | you removed that property in `componentWillUnmount`).  The usual
-- | workaround that is to set `isMounted` property in `componentWillMount` and
-- | then reset it on `componentWillUnmount` and check it before reading in
-- | async callbacks.
getProp
  :: forall r r' l a p s eff
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> ReactThisIx p s r
  -> Eff eff a
getProp l (ReactThisIx r) = runEffFn2 unsafeGetImpl (reflectSymbol l) r

getPropIx
  :: forall r r' l a p s eff
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> ReactThisIx p s r
  -> EffR eff { | r} { | r} a
getPropIx l r = EffR (getProp l r)

foreign import unsafeSetImpl :: forall a b c eff. EffFn3 eff String a b c

setProp
  :: forall r1 r2 r l a b p s eff
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> ReactThisIx p s r1
  -> Eff eff (ReactThisIx p s r2)
setProp l b (ReactThisIx r) = ReactThisIx <$> runEffFn3 unsafeSetImpl (reflectSymbol l) b r

setPropIx
  :: forall r1 r2 r l a b p s eff
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> ReactThisIx p s r1
  -> EffR eff { | r} { | r} (ReactThisIx p s r2)
setPropIx l b r = EffR $ setProp l b r

foreign import unsafeInsertImpl :: forall a b c eff. EffFn3 eff String a b c

insertPropIx
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> ReactThisIx p s r1
  -> EffR eff { | r1} { | r2} (ReactThisIx p s r2)
insertPropIx l a (ReactThisIx r) = EffR $ ReactThisIx <$> runEffFn3 unsafeInsertImpl (reflectSymbol l) a r

foreign import unsafeNullifyImpl :: forall a b eff. EffFn2 eff String a b

-- | Set a property as null and remove it from `EffR`.
-- | Set to null rather than delete just to be consistent with react, which
-- | calls ref callbacks with `null` when component is unmounted.
nullifyPropIx
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> ReactThisIx p s r2
  -> EffR eff { | r2} { | r1} (ReactThisIx p s r1)
nullifyPropIx l (ReactThisIx r) = EffR $ ReactThisIx <$> runEffFn2 unsafeNullifyImpl (reflectSymbol l) r

-- | A render function.
type RenderIx props state ri ro eff
   = ReactThisIx props state ri
   -> EffR
      ( props :: ReactProps
      , refs :: ReactRefs Disallowed
      , state :: ReactState ReadOnly
      | eff
      )
      { | ri} { | ro}
      ReactElement

-- | A get initial state function.
type GetInitialStateIx props state r eff
    = ReactThisIx props state r
    -> EffR
      ( props :: ReactProps
      , state :: ReactState Disallowed
      , refs :: ReactRefs Disallowed
      | eff
      )
      { | r} { | r}
      state

-- | A component will mount function.
type ComponentWillMountIx props state r eff
   = ReactThisIx props state ()
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs Disallowed
      | eff
      )
      {} { | r}
      (ReactThisIx props state r)

-- | A component did mount function.
type ComponentDidMountIx props state r eff
   = ReactThisIx props state r
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Unit

-- | A component will receive props function.
type ComponentWillReceivePropsIx props state r eff
   = ReactThisIx props state r
   -> props
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Unit

-- | A should component update function.
type ShouldComponentUpdateIx props state (r :: # Type) (eff :: # Effect)
   = ReactThisIx props state r
  -> props
  -> state
  -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Boolean

-- | A component will update function.
type ComponentWillUpdateIx props state r eff
   = ReactThisIx props state r
   -> props
   -> state
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Unit

-- | A component did update function.
type ComponentDidUpdateIx props state r eff
   = ReactThisIx props state r
   -> props
   -> state
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadOnly
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Unit

-- | A component will unmount function.
type ComponentWillUnmountIx props state r ro eff
   = ReactThisIx props state r
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadOnly
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | ro}
      (ReactThisIx props state ro)

-- | Track added properties on the type level.
-- |
-- | * `ri` row describes added properties in `ComponentWillMountIx` (_callbacks_)
-- | * `rr` row describes added properties within render method (_refs_)
-- | * `ro` row is the state of `ReactThis` after `componetnWillUnmount`.
-- |
-- | Likely you want to use
-- | ``` purescript
-- | ReactSpecIx p s ri rr () eff
-- | ```
-- | This will ensure that you don't leak memory, by keeping a reference.
type ReactSpecIx p s (ri :: # Type) (rr :: # Type) (ro :: # Type) (eff :: # Effect) =
  Subrow ri rr =>
  Subrow ro rr =>
  { render :: RenderIx p s ri rr eff
  , displayName :: String
  , getInitialState :: GetInitialStateIx p s ri eff
  , componentWillMount :: ComponentWillMountIx p s ri eff
  , componentDidMount :: ComponentDidMountIx p s rr eff
  , componentWillReceiveProps :: ComponentWillReceivePropsIx p s rr eff
  , shouldComponentUpdate :: ShouldComponentUpdateIx p s rr eff
  , componentWillUpdate :: ComponentWillUpdateIx p s rr eff
  , componentDidUpdate :: ComponentDidUpdateIx p s rr eff
  , componentWillUnmount :: ComponentWillUnmountIx p s rr ro eff
  }

specIx'
  :: forall p s ri rr ro eff
   . Subrow ro rr
  => Subrow ri rr
  => GetInitialStateIx p s ri eff
  -> ComponentWillMountIx p s ri eff
  -> ComponentWillUnmountIx p s rr ro eff
  -> RenderIx p s ri rr eff
  -> ReactSpecIx p s ri rr ro eff
specIx' getInitialState componentWillMount componentWillUnmount renderFn =
  { render: renderFn
  , displayName: ""
  , getInitialState: getInitialState
  , componentWillMount: componentWillMount
  , componentDidMount: \_ -> pure unit
  , componentWillReceiveProps: \_ _ -> pure unit
  , shouldComponentUpdate: \_ _ _ -> pure true
  , componentWillUpdate: \_ _ _ -> pure unit
  , componentDidUpdate: \_ _ _ -> pure unit
  , componentWillUnmount: componentWillUnmount
  }

specIx
  :: forall p s eff
   . s
  -> RenderIx p s () () eff
  -> ReactSpecIx p s () () () eff
specIx s r = (specIx' (\_ -> pure s) pure pure r)

toReactSpec
  :: forall p s ri rr ro eff
   . Subrow ri rr
  => Subrow ro rr
  => ReactSpecIx p s ri rr ro eff
  -> ReactSpec p s eff
toReactSpec
  { render
  , displayName
  , getInitialState
  , componentWillMount
  , componentDidMount
  , componentWillReceiveProps
  , shouldComponentUpdate
  , componentWillUpdate
  , componentDidUpdate
  , componentWillUnmount
  }
  = { render: \this -> case render (ReactThisIx this) of EffR m -> m
    , displayName
    , getInitialState: \this -> case getInitialState (ReactThisIx this) of EffR m -> m
    , componentWillMount: \this -> case componentWillMount (ReactThisIx this) of EffR m -> void m
    , componentDidMount: \this -> case componentDidMount (ReactThisIx this) of EffR m -> m
    , componentWillReceiveProps: \this p -> case componentWillReceiveProps (ReactThisIx this) p of EffR m -> void m
    , shouldComponentUpdate: \this p s -> case shouldComponentUpdate (ReactThisIx this) p s of EffR m -> m
    , componentWillUpdate: \this p s -> case componentWillUpdate  (ReactThisIx this) p s of EffR m -> m
    , componentDidUpdate: \this p s -> case componentDidUpdate (ReactThisIx this) p s of EffR m -> m
    , componentWillUnmount: \this -> case componentWillUnmount (ReactThisIx this) of EffR m -> void m
    }

fromReactSpec
  :: forall p s eff
   . ReactSpec p s eff
  -> ReactSpecIx p s () () () eff
fromReactSpec
  { render
  , displayName
  , getInitialState
  , componentWillMount
  , componentDidMount
  , componentWillReceiveProps
  , shouldComponentUpdate
  , componentWillUpdate
  , componentDidUpdate
  , componentWillUnmount
  }
  = { render: \(ReactThisIx rThis) -> EffR $ render rThis
    , displayName
    , getInitialState: \(ReactThisIx rThis) -> EffR $ getInitialState rThis
    , componentWillMount: \(ReactThisIx rThis) ->  EffR (componentWillMount rThis) $> ReactThisIx rThis
    , componentDidMount: \(ReactThisIx rThis) -> EffR $ componentDidMount rThis
    , componentWillReceiveProps: \(ReactThisIx rThis) p -> EffR $ componentWillReceiveProps rThis p
    , shouldComponentUpdate: \(ReactThisIx rThis) p s -> EffR $ shouldComponentUpdate rThis p s
    , componentWillUpdate: \(ReactThisIx rThis) p s -> EffR $  componentWillUpdate rThis p s
    , componentDidUpdate: \(ReactThisIx rThis) p s -> EffR $ componentDidUpdate rThis p s
    , componentWillUnmount: \(ReactThisIx rThis) -> EffR (componentWillUnmount rThis) $> ReactThisIx rThis
    }

createClassIx
  :: forall p s ri rr ro eff
   . Subrow ri rr
  => Subrow ro rr
  => ReactSpecIx p s ri rr ro eff
  -> ReactClass p
createClassIx spc = createClass (toReactSpec spc)

refFn
  :: forall element eff ri ro
   . (element -> EffR eff ri ro Unit)
  -> EffR eff ri ro Props
refFn fn = EffR $ pureEff (unsafeMkProps "ref" (\e -> unsafePerformEffR (fn e)))
  where
    pureEff :: forall a e. a -> Eff e a
    pureEff = pure
