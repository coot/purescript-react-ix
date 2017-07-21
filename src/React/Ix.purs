module React.Ix where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn2, runEffFn3)
import DOM.HTML.Types (HTMLElement)
import Data.Symbol (reflectSymbol)
import Prelude (Unit, pure, unit, ($))
import React (Disallowed, ReactElement, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite)
import React.DOM.Props (Props, unsafeMkProps)
import React.Ix.EffR (EffR(..), unsafePerformEffR)
import Type.Data.Symbol (class IsSymbol, SProxy)
import Type.Row (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)

newtype This p s (r :: # Type) = This (ReactThis p s)

class Subrow (r :: # Type) (s :: # Type)
instance srInst :: Union r t s => Subrow r s

foreign import unsafeGetImpl :: forall a b eff. EffFn2 eff String a b

get
  :: forall r r' l a p s eff
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> This p s r
  -> Eff eff a
get l r = runEffFn2 unsafeGetImpl (reflectSymbol l) r

getIx
  :: forall r r' l a p s eff
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> This p s r
  -> EffR eff { | r} { | r} a
getIx l r = EffR (get l r)

foreign import unsafeSetImpl :: forall a b c eff. EffFn3 eff String a b c

set
  :: forall r1 r2 r l a b p s eff
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> This p s r1
  -> Eff eff (This p s r2)
set l b r = runEffFn3 unsafeSetImpl (reflectSymbol l) b r

setIx
  :: forall r1 r2 r l a b p s eff
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> This p s r1
  -> EffR eff { | r} { | r} (This p s r2)
setIx l b r = EffR $ set l b r

foreign import unsafeInsertImpl :: forall a b c eff. EffFn3 eff String a b c

insertIx
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> This p s r1
  -> EffR eff { | r1} { | r2} (This p s r2)
insertIx l a r = EffR $ runEffFn3 unsafeInsertImpl (reflectSymbol l) a r

foreign import unsafeDeleteImpl :: forall a b eff. EffFn2 eff String a b

deleteIx
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> This p s r2
  -> EffR eff { | r2} { | r1} (This p s r1)
deleteIx l r = EffR $ runEffFn2 unsafeDeleteImpl (reflectSymbol l) r

-- | A render function.
type Render props state ri ro eff
   = This props state ri
   -> EffR
      ( props :: ReactProps
      , refs :: ReactRefs Disallowed
      , state :: ReactState ReadOnly
      | eff
      )
      { | ri} { | ro}
      ReactElement

-- | A get initial state function.
type GetInitialState props state r eff
    = This props state r
    -> EffR
      ( props :: ReactProps
      , state :: ReactState Disallowed
      , refs :: ReactRefs Disallowed
      | eff
      )
      { | r} { | r}
      state

-- | A component will mount function.
type ComponentWillMount props state r eff
   = This props state ()
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs Disallowed
      | eff
      )
      {} { | r}
      (This props state r)

-- | A component did mount function.
type ComponentDidMount props state r eff
   = This props state r
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | r}
      Unit

-- | A component will receive props function.
type ComponentWillReceiveProps props state r eff
   = This props state r
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
type ShouldComponentUpdate props state (r :: # Type) (eff :: # Effect)
   = This props state r
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
type ComponentWillUpdate props state r eff
   = This props state r
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
type ComponentDidUpdate props state r eff
   = This props state r
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
type ComponentWillUnmount props state r ro eff
   = This props state r
   -> EffR
      ( props :: ReactProps
      , state :: ReactState ReadOnly
      , refs :: ReactRefs ReadOnly
      | eff
      )
      { | r} { | ro}
      (This props state ro)

type Spec p s (r :: # Type) (rr :: # Type) (ro :: # Type) (eff :: # Effect) =
  Subrow r rr =>
  Subrow ro rr =>
  { render :: Render p s r rr eff
  , displayName :: String
  , getInitialState :: GetInitialState p s r eff
  , componentWillMount :: ComponentWillMount p s r eff
  , componentDidMount :: ComponentDidMount p s r eff
  , componentWillReceiveProps :: ComponentWillReceiveProps p s r eff
  , shouldComponentUpdate :: ShouldComponentUpdate p s r eff
  , componentWillUpdate :: ComponentWillUpdate p s r eff
  , componentDidUpdate :: ComponentDidUpdate p s r eff
  , componentWillUnmount :: ComponentWillUnmount p s rr ro eff
  }

spec'
  :: forall p s r rr ro eff
   . Subrow ro rr
  => Subrow r rr
  => GetInitialState p s r eff
  -> ComponentWillMount p s r eff
  -> ComponentWillUnmount p s rr ro eff
  -> Render p s r rr eff
  -> Spec p s r rr ro eff
spec' getInitialState componentWillMount componentWillUnmount renderFn =
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

spec
  :: forall p s eff
   . s
  -> Render p s () () eff
  -> Spec p s () () () eff
spec s r = (spec' (\_ -> pure s) pure pure r)

refFn
  :: forall eff ri ro
   . (HTMLElement -> EffR eff ri ro Unit)
  -> EffR eff ri ro Props
refFn fn = unsafeCoerce (unsafeMkProps "ref" (\e -> unsafePerformEffR (fn e)))
