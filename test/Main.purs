module Test.Main where

import Prelude

import Control.IxMonad (ipure, (:>>=))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement, readHTMLElement)
import Data.Array ((!!))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foreign (Foreign, isNull, readString, toForeign)
import Data.Maybe (Maybe(Just, Nothing))
import Enzyme (mount)
import Enzyme.ReactWrapper as E
import Enzyme.Types (ENZYME)
import React (Event, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, ReactSpec, createElement, readState, spec, transformState)
import React.DOM as D
import React.DOM.Props as P
import React.Ix (ComponentDidMountIx, ComponentWillMountIx, ComponentWillUnmountIx, ReactSpecIx, ReactThisIx(ReactThisIx), RenderIx, createClassIx, fromReactSpec, getProp, getPropIx, insertPropIx, nullifyPropIx, refFn, specIx, specIx', toReactSpec)
import React.Ix.EffR (EffR)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Karma (runKarma)
import Type.Data.Symbol (SProxy(..))

foreign import unsafeListPropsImpl :: forall a eff. EffFn1 eff a (Array { key :: String, value :: Foreign })

unsafeListProps :: forall eff a. a -> Eff eff (Array { key :: String, value :: Foreign })
unsafeListProps = runEffFn1 unsafeListPropsImpl

refSpec :: forall eff. Ref (Maybe HTMLElement) ->  ReactSpecIx Unit Unit () ( element :: HTMLElement ) ( element :: HTMLElement ) ( ref :: REF | eff )
refSpec ref = (specIx' (\_ -> ipure unit) willMount willUnmount renderFn)
    { componentDidMount = didMount }
  where
    willMount :: ComponentWillMountIx Unit Unit () ( ref :: REF | eff )
    willMount = ipure

    willUnmount :: ComponentWillUnmountIx Unit Unit ( element :: HTMLElement ) ( element :: HTMLElement) ( ref :: REF | eff )
    willUnmount this = ipure this

    didMount :: ComponentDidMountIx Unit Unit ( element :: HTMLElement ) ( ref :: REF | eff )
    didMount this =
      getPropIx (SProxy :: SProxy "element") this
      :>>= liftEff <<< writeRef ref <<< Just
      :>>= \_ -> ipure unit

    setRef
      :: forall e
       . ReactThisIx Unit Unit ()
      -> HTMLElement
      -> EffR e
          {}
          { element :: HTMLElement }
          Unit
    setRef this el = do
      void $ insertPropIx (SProxy :: SProxy "element") el this

    renderFn :: RenderIx Unit Unit () (element :: HTMLElement) ( ref :: REF | eff )
    renderFn this =
      refFn (setRef this)
      :>>= \prop ->
        ipure $ D.div
          [ prop
          ]
          [ D.text ":)" ]

sSpec :: forall eff. ReactSpecIx Unit Unit () () () eff
sSpec = specIx unit (\_ -> pure $ D.div' [ D.text "Hello world!" ])

cWillUnmount :: forall eff. ComponentWillUnmountIx Unit Int ( count :: Maybe Int, name :: String ) ( name :: String ) eff
cWillUnmount this = do
  nullifyPropIx (SProxy :: SProxy "count") this

cWillMount :: forall eff. ComponentWillMountIx Unit Int ( count :: Maybe Int, name :: String ) eff
cWillMount this =
  insertPropIx (SProxy :: SProxy "count") (Just 0) this
  :>>= insertPropIx (SProxy :: SProxy "name") "cSpec"

cSpec
  :: forall eff
   . Ref (Array { key :: String, value :: Foreign })
  -> ReactSpecIx Unit { count :: Int, name :: String }
      ( count :: Maybe Int
      , name :: String
      , handler
          :: Event
          -> Eff
              ( props :: ReactProps
              , refs :: ReactRefs ReadOnly
              , state :: ReactState ReadWrite
              | eff )
              Unit
      )
      ( count :: Maybe Int
      , name :: String
      , handler
          :: Event
          -> Eff
              ( props :: ReactProps
              , refs :: ReactRefs ReadOnly
              , state :: ReactState ReadWrite
              | eff )
              Unit
      )
      ()
      (ref :: REF | eff)
cSpec ref =
  (specIx' (\_ -> pure { count: 0, name: "" })
    componentWillMount
    componentWillUnmount
    render)
  { displayName = "cSpec"
  , componentDidMount = componentDidMount
  }
    where
      handler (ReactThisIx rThis) ev =
        -- `This` is not `ReactThis`
        transformState rThis (\s@{ count } -> s { count = count + 1 })

      componentWillMount this =
        insertPropIx (SProxy :: SProxy "count") (Just 0) this
        :>>= \t -> insertPropIx (SProxy :: SProxy "handler") (handler t) t
        :>>= insertPropIx (SProxy :: SProxy "name") "cSpec"
        :>>= (\t' -> t' <$ getPropIx (SProxy :: SProxy "name") t')

      componentDidMount
        :: ComponentDidMountIx
            Unit { count :: Int, name :: String }
            ( count :: Maybe Int
            , name :: String
            , handler
                :: Event
                -> Eff
                    ( props :: ReactProps
                    , refs :: ReactRefs ReadOnly
                    , state :: ReactState ReadWrite
                    | eff )
                    Unit
            )
            ( ref :: REF | eff )
      componentDidMount this@(ReactThisIx rThis) = liftEff $ do
        n <- getProp (SProxy :: SProxy "name") this
        transformState rThis (_ { name = n })

      -- this could be derived by the compiler
      componentWillUnmount this =
        nullifyPropIx (SProxy :: SProxy "count") this
        :>>= nullifyPropIx (SProxy :: SProxy "handler")
        :>>= nullifyPropIx (SProxy :: SProxy "name")
        :>>= (\t@ReactThisIx rThis -> liftEff do
                ps <- unsafeListProps rThis
                writeRef ref ps
                pure t)

      render (this@ReactThisIx that) = do
        { count } <- liftEff $ readState that
        h <- getPropIx (SProxy :: SProxy "handler") this
        pure $ D.div [ P.onClick h ] [ D.text (show count) ]

main :: forall e. Eff (avar :: AVAR, console :: CONSOLE, dom :: DOM, enzyme :: ENZYME, ref :: REF | e) Unit
main = runKarma do
  suite "ReactSpecIx" do
    test "write / read custom property" do
      name <- liftEff $ do
        r <- newRef []
        w <- mount (createElement (createClassIx (cSpec r)) unit [])
        n <- E.stateByKey "name" w
        _ <- E.unmount w
        pure n

      case runExcept (readString name) of
        Left _ -> failure "custom property not set"
        Right s -> equal "cSpec" s

    test "componendWillUnmount"
      let
        isNullKey :: String -> Array { key :: String, value :: Foreign } -> Boolean
        isNullKey s ps =
          case (A.filter (\ { key } -> key == s) ps) !! 0 of
            Nothing -> false
            Just { value } -> isNull value
      in do
        ps <- liftEff $ do
          r <- newRef []
          _ <- mount (createElement (createClassIx (cSpec r)) unit []) >>= E.unmount
          readRef r

        assert "name is not null" (isNullKey "name" ps)
        assert "handler is not null" (isNullKey "handler" ps)
        assert "count is not null" (isNullKey "count" ps)

    test "refFn" do
      e <- liftEff $ do
        r <- newRef Nothing
        w <- mount (createElement (createClassIx (refSpec r)) unit []) >>= E.unmount
        readRef r

      case e of
        Nothing -> failure "ref not set in componentDidMount"
        Just htmlElement ->
          case runExcept (readHTMLElement <<< toForeign $ htmlElement) of
            Left _ -> failure "not a html element"
            Right _ -> success

    test "fromReactSpec"
      let
        s :: forall eff. ReactSpecIx Unit Unit () () () eff
        s = fromReactSpec (spec unit (\_ -> pure (D.div' [])))
      in success

    test "toReactSpec"
      let
        s :: forall eff. ReactSpec Unit Unit eff
        s = toReactSpec sSpec
      in success
