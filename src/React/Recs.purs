module React.Recs where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn2, runEffFn3)
import Data.Maybe (Maybe(Just))
import Data.Symbol (reflectSymbol)
import Prelude (Unit, add, bind, pure, show, unit, ($), (>>=))
import React (Disallowed, Event, ReactElement, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, readState, transformState)
import React.DOM as D
import React.DOM.Props as P
import Type.Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class RowLacks)

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

foreign import unsafeInsertImpl :: forall a b c eff. EffFn3 eff String a b c

insert
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> This p s r1
  -> Eff eff (This p s r2)
insert l a r = runEffFn3 unsafeInsertImpl (reflectSymbol l) a r

foreign import unsafeDeleteImpl :: forall a b eff. EffFn2 eff String a b

delete
  :: forall r1 r2 l a p s eff
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> This p s r2
  -> Eff eff (This p s r1)
delete l r = runEffFn2 unsafeDeleteImpl (reflectSymbol l) r

-- | A render function.
type Render props state r eff =
  This props state r ->
  Eff
    ( props :: ReactProps
    , refs :: ReactRefs Disallowed
    , state :: ReactState ReadOnly
    | eff
    ) ReactElement

-- | A get initial state function.
type GetInitialState props state r eff =
  This props state r ->
  Eff
    ( props :: ReactProps
    , state :: ReactState Disallowed
    , refs :: ReactRefs Disallowed
    | eff
    ) state

-- | A component will mount function.
type ComponentWillMount props state r eff =
  This props state () ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs Disallowed
    | eff
    ) (This props state r)

-- | A component did mount function.
type ComponentDidMount props state r eff =
  This props state r ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A component will receive props function.
type ComponentWillReceiveProps props state r eff =
   This props state r ->
   props ->
   Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A should component update function.
type ShouldComponentUpdate props state r eff =
  This props state r ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Boolean

-- | A component will update function.
type ComponentWillUpdate props state r eff =
  This props state r ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A component did update function.
type ComponentDidUpdate props state r eff =
  This props state r ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A component will unmount function.
type ComponentWillUnmount props state r ro eff =
  This props state r ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    , refs :: ReactRefs ReadOnly
    | eff
    ) (This props state ro)

type Spec p s (r :: # Type) (ro :: # Type) (eff :: # Effect) =
  Subrow ro r =>
  { render :: Render p s r eff
  , displayName :: String
  , getInitialState :: GetInitialState p s r eff
  , componentWillMount :: ComponentWillMount p s r eff
  , componentDidMount :: ComponentDidMount p s r eff
  , componentWillReceiveProps :: ComponentWillReceiveProps p s r eff
  , shouldComponentUpdate :: ShouldComponentUpdate p s r eff
  , componentWillUpdate :: ComponentWillUpdate p s r eff
  , componentDidUpdate :: ComponentDidUpdate p s r eff
  , componentWillUnmount :: ComponentWillUnmount p s r ro eff
  }

spec'
  :: forall p s r ro eff
   . Subrow ro r
  => GetInitialState p s r eff
  -> ComponentWillMount p s r eff
  -> ComponentWillUnmount p s r ro eff
  -> Render p s r eff
  -> Spec p s r ro eff
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
  -> Render p s () eff
  -> Spec p s () () eff
spec s r = (spec' (\_ -> pure s) pure pure r)

-- test

sSpec :: forall eff. Spec Unit Unit () () eff
sSpec = spec unit (\_ -> pure $ D.div' [ D.text "Hello world!" ])

cSpec
  :: forall eff
   . Spec Unit Int
      ( count :: Maybe Int
      , name :: String
      , handler
          :: Event
          -> Eff
              ( props :: ReactProps
              , refs :: ReactRefs ReadOnly
              , state :: ReactState ReadWrite
              | eff )
              Unit )
      ( name :: String)
      eff
cSpec = (spec' (\_ -> pure 0) componentWillMount componentWillUnmount render)
  { displayName = "cSpec" }
    where
      handler this ev =
        -- `This` is not `ReactThis`
        transformState (case this of This rThis -> rThis) (add 1)

      componentWillMount this =
        insert (SProxy :: SProxy "count") (Just 0) this
        >>= insert (SProxy :: SProxy "handler") (handler this)
        >>= insert (SProxy :: SProxy "name") "cSpec"

      -- this can be derived by the compiler
      componentWillUnmount this =
        delete (SProxy :: SProxy "count") this
        >>= delete (SProxy :: SProxy "handler")

      render (this@This that) = do
        c <- readState that
        h <- get (SProxy :: SProxy "handler") this
        pure $ D.div [ P.onClick h ] [ D.text (show c) ]
