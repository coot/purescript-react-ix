module Test.Main where

import Prelude

import Control.IxMonad (ibind, ipure, (:>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.HTML.Types (HTMLElement)
import Data.Maybe (Maybe(..))
import React (Event, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, readState, transformState)
import React.DOM as D
import React.DOM.Props as P
import React.Recs (ComponentWillMount, ComponentWillUnmount, Render, Spec, This(..), deleteIx, getIx, insertIx, refFn, spec, spec')
import React.Recs.EffR (EffR)
import Type.Data.Symbol (SProxy(..))

refSpec :: forall eff. Spec Unit Unit () ( element :: HTMLElement ) ( element :: HTMLElement ) eff
refSpec = spec' (\_ -> ipure unit) willMount willUnmount renderFn
  where
    willMount :: ComponentWillMount Unit Unit () eff
    willMount = ipure

    willUnmount :: ComponentWillUnmount Unit Unit ( element :: HTMLElement ) ( element :: HTMLElement) eff
    willUnmount = ipure

    setRef
      :: forall e
       . This Unit Unit ()
      -> HTMLElement
      -> EffR e
          {}
          { element :: HTMLElement }
          Unit
    setRef this el =
      void $ insertIx (SProxy :: SProxy "element") el this

    renderFn :: Render Unit Unit () (element :: HTMLElement) eff
    renderFn this =
      refFn (setRef this)
      :>>= \prop ->
        ipure $ D.div
          [ prop
          ]
          [ D.text ":)" ]

sSpec :: forall eff. Spec Unit Unit () () () eff
sSpec = spec unit (\_ -> pure $ D.div' [ D.text "Hello world!" ])

cWillUnmount :: forall eff. ComponentWillUnmount Unit Int ( count :: Maybe Int, name :: String ) ( name :: String ) eff
cWillUnmount this = do
  deleteIx (SProxy :: SProxy "count") this

cWillMount :: forall eff. ComponentWillMount Unit Int ( count :: Maybe Int, name :: String ) eff
cWillMount this =
  insertIx (SProxy :: SProxy "count") (Just 0) this
  :>>= insertIx (SProxy :: SProxy "name") "cSpec"

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
      eff
cSpec = (spec' (\_ -> pure 0) componentWillMount componentWillUnmount render)
  { displayName = "cSpec" }
    where
      handler this ev =
        -- `This` is not `ReactThis`
        transformState (case this of This rThis -> rThis) (add 1)

      componentWillMount t0 = do
        t1 <- insertIx (SProxy :: SProxy "count") (Just 0) t0
        t2 <- insertIx (SProxy :: SProxy "handler") (handler t1) t1
        insertIx (SProxy :: SProxy "name") "cSpec" t2
          where
            bind = ibind

      -- this could be derived by the compiler
      componentWillUnmount this =
        deleteIx (SProxy :: SProxy "count") this
        :>>= deleteIx (SProxy :: SProxy "handler")
        :>>= deleteIx (SProxy :: SProxy "name")

      render (this@This that) = do
        c <- liftEff $ readState that
        h <- getIx (SProxy :: SProxy "handler") this
        pure $ D.div [ P.onClick h ] [ D.text (show c) ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
