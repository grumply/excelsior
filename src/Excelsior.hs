{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Excelsior 
  ( SomeAction(..), Action(..)
  , Reducer, Middleware
  , createStore
  , reducer, applyReducers
  , middleware, applyMiddlewares
  , dispatch
  , watch, excel
  ) where

import Ef.Event
import Pure.View hiding (Action)
import Pure.Service hiding (Action)
import Data.Typeable
import Pure.WebSocket.TypeRep (rep)

data SomeAction state = forall e. Action state e => SomeAction e

class Typeable e => Action (state :: *) (e :: *) | e -> state where
    toAction :: e -> SomeAction state
    fromAction :: SomeAction state -> Maybe e

    toAction = SomeAction
    fromAction (SomeAction e) = cast e

instance Typeable state => Action state (SomeAction state) where
    toAction = id
    fromAction = Just

type Reducer state = SomeAction state -> state -> state
type Middleware state = state -> (SomeAction state -> IO state) -> SomeAction state -> IO state

type Handler state = SomeAction state -> state -> IO state

data ExcelsiorState state = ExcelsiorState
  { esCurrentReducers :: [Reducer state]
  , esCurrentMiddlewares :: [Middleware state] 
  , esCurrentHandler :: Handler state
  }

type Excelsior state = Service '[State () (ExcelsiorState state),Observable state]

store :: forall state. Typeable state 
          => state -> [Reducer state] -> [Middleware state] -> Excelsior state
store initial reducers middlewares = Service {..}
  where
    key = "excelsior_store_" <> fromTxt (rep (Proxy :: Proxy state))
    build base = do
        let is = ExcelsiorState reducers middlewares (composeHandler middlewares reducers)
        obs <- observable initial
        return (state is *:* obs *:* base)
    prime = return ()

composeHandler :: forall state . [Middleware state] -> [Reducer state] -> SomeAction state -> state -> IO state
composeHandler middlewares reducers action state = handleMiddlewares action middlewares
    where
        handleMiddlewares action [] = return (reduce action state reducers)
        handleMiddlewares action (m:ms) = m state (\action -> handleMiddlewares action ms) action

        reduce _ state [] = state
        reduce action state (r:rs) = reduce action (r action state) rs

createStore :: (MonadIO c, Typeable state) => state -> [Reducer state] -> [Middleware state] -> c ()
createStore state reducers middlewares = void $ with (store state reducers middlewares) (return ())

reducer :: (Action state a) => (a -> state -> state) -> Reducer state
reducer f (fromAction -> Just a) state = f a state
reducer _ _ state = state

applyReducers :: forall c state. (MonadIO c, Typeable state) => [Reducer state] -> c (Promise ())
applyReducers reducers = with (store (undefined :: state) [] []) $ do
    ExcelsiorState {..} <- get
    let handler = composeHandler esCurrentMiddlewares reducers
    put ExcelsiorState { esCurrentReducers = reducers, esCurrentHandler = handler, .. }

middleware :: (Action state a) => (state -> (SomeAction state -> IO state) -> a -> IO state) -> Middleware state
middleware f state next (fromAction -> Just a) = f state next a
middleware _ _ next action = next action

applyMiddlewares :: forall c state. (MonadIO c, Typeable state) => [Middleware state] -> c (Promise ())
applyMiddlewares middlewares = with (store (undefined :: state) [] []) $ do
    ExcelsiorState {..} <- get
    let handler = composeHandler middlewares esCurrentReducers
    put ExcelsiorState { esCurrentMiddlewares = middlewares, esCurrentHandler = handler, .. }

dispatch :: forall c state a. (Typeable state, MonadIO c, Action state a) => a -> c (Promise ())
dispatch (toAction -> sa) = with (store (undefined :: state) [] []) $ do 
    state <- getO
    ExcelsiorState {..} <- get
    state' <- liftIO (esCurrentHandler sa state)
    setO state'

watch :: forall c ms state. (Typeable state, MonadIO c, ms <: '[Evented]) 
      => (state -> Ef '[Event state] (Ef ms c) ()) -> Ef ms c (Promise (IO ()))
watch = observe (store (undefined :: state) [] [])

excel :: (MVC model ms, Typeable state) => (state -> model ms -> model ms) -> Ef ms IO ()
excel f = void $ watch $ lift . modifyModel . f 