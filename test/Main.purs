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
import React.Ix (ComponentWillMountIx, ComponentWillUnmountIx, ReactSpecIx, ReactThisIx(..), RenderIx, deleteIx, getIx, insertIx, refFn, specIx, specIx')
import React.Ix.EffR (EffR)
import Type.Data.Symbol (SProxy(..))

refSpec :: forall eff. ReactSpecIx Unit Unit () ( element :: HTMLElement ) ( element :: HTMLElement ) eff
refSpec = specIx' (\_ -> ipure unit) willMount willUnmount renderFn
  where
    willMount :: ComponentWillMountIx Unit Unit () eff
    willMount = ipure

    willUnmount :: ComponentWillUnmountIx Unit Unit ( element :: HTMLElement ) ( element :: HTMLElement) eff
    willUnmount = ipure

    setRef
      :: forall e
       . ReactThisIx Unit Unit ()
      -> HTMLElement
      -> EffR e
          {}
          { element :: HTMLElement }
          Unit
    setRef this el =
      void $ insertIx (SProxy :: SProxy "element") el this

    renderFn :: RenderIx Unit Unit () (element :: HTMLElement) eff
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
  deleteIx (SProxy :: SProxy "count") this

cWillMount :: forall eff. ComponentWillMountIx Unit Int ( count :: Maybe Int, name :: String ) eff
cWillMount this =
  insertIx (SProxy :: SProxy "count") (Just 0) this
  :>>= insertIx (SProxy :: SProxy "name") "cSpec"

cSpec
  :: forall eff
   . ReactSpecIx Unit Int
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
cSpec = (specIx' (\_ -> pure 0) componentWillMount componentWillUnmount render)
  { displayName = "cSpec" }
    where
      handler this ev =
        -- `This` is not `ReactThis`
        transformState (case this of ReactThisIx rThis -> rThis) (add 1)

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

      render (this@ReactThisIx that) = do
        c <- liftEff $ readState that
        h <- getIx (SProxy :: SProxy "handler") this
        pure $ D.div [ P.onClick h ] [ D.text (show c) ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Well done budy..."
