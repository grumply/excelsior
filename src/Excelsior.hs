{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Excelsior 
  ( SomeCommand(..), Command(..)
  , Reducer, Middleware
  , store, createStore
  , reducer, applyReducers
  , middleware, applyMiddlewares
  , command, commands
  , getStore
  , watchIO, watchIO'
  , watch, watch'
  , excel, excel'
  ) where

import Ef.Event
import Pure.View hiding (Command)
import Pure.Service hiding (Command)
import Data.Typeable
import Pure.WebSocket.TypeRep (rep)

data SomeCommand state = forall e. Command state e => SomeCommand e

class Typeable c => Command (state :: *) (c :: *) | c -> state where
    toCommand :: c -> SomeCommand state
    fromCommand :: SomeCommand state -> Maybe c

    {-# INLINE toCommand #-}
    toCommand = SomeCommand
    {-# INLINE fromCommand #-}
    fromCommand (SomeCommand c) = cast c

instance Typeable state => Command state (SomeCommand state) where
    {-# INLINE toCommand #-}
    toCommand = id
    {-# INLINE fromCommand #-}
    fromCommand = Just

type Reducer state = SomeCommand state -> state -> state
type Middleware state = state -> (SomeCommand state -> IO state) -> SomeCommand state -> IO state

type Handler state = SomeCommand state -> state -> IO state

data ExcelsiorState state = ExcelsiorState
  { esCurrentReducers :: [Reducer state]
  , esCurrentMiddlewares :: [Middleware state] 
  , esCurrentHandler :: Handler state
  , esIOCallbacks :: [(Int,state -> IO ())]
  }

type Excelsior state = Service '[State () (ExcelsiorState state),Observable state]

{-# INLINE store #-}
store :: forall state. Typeable state 
      => state -> [Reducer state] -> [Middleware state] -> Excelsior state
store initial reducers middlewares = Service {..}
  where
    key = "excelsior_store_" <> fromTxt (rep (Proxy :: Proxy state))
    build base = do
        let is = ExcelsiorState reducers middlewares (composeHandler middlewares reducers) []
        obs <- observable initial
        return (state is *:* obs *:* base)
    prime = return ()

{-# INLINE composeHandler #-}
composeHandler :: forall state. [Middleware state] -> [Reducer state] -> SomeCommand state -> state -> IO state
composeHandler middlewares reducers command state = handleMiddlewares command middlewares
    where
        handleMiddlewares command [] = return (reduce command state reducers)
        handleMiddlewares command (m:ms) = m state (\command -> handleMiddlewares command ms) command

        reduce _ state [] = state
        reduce command state (r:rs) = reduce command (r command state) rs

createStore :: (MonadIO c, Typeable state) => state -> [Reducer state] -> [Middleware state] -> c ()
createStore state reducers middlewares = void $ with (store state reducers middlewares) (return ())

{-# INLINE reducer #-}
reducer :: (Command state cmd) => (state -> cmd -> state) -> Reducer state
reducer f (fromCommand -> Just a) state = f state a
reducer _ _ state = state

applyReducers :: forall c state. (MonadIO c, Typeable state) => [Reducer state] -> c (Promise ())
applyReducers reducers = with (store (error "applyReducers: store not initialized" :: state) [] []) $ do
    ExcelsiorState {..} <- get
    let handler = composeHandler esCurrentMiddlewares reducers
    put ExcelsiorState { esCurrentReducers = reducers, esCurrentHandler = handler, .. }

{-# INLINE middleware #-}
middleware :: (Command state cmd) => (state -> (forall cmd. Command state cmd => cmd -> IO state) -> cmd -> IO state) -> Middleware state
middleware f state next (fromCommand -> Just a) = f state (next . toCommand) a
middleware _ _ next command = next command

applyMiddlewares :: forall c state. (MonadIO c, Typeable state) => [Middleware state] -> c (Promise ())
applyMiddlewares middlewares = with (store (error "applyMiddlewares: store not initialized" :: state) [] []) $ do
    ExcelsiorState {..} <- get
    let handler = composeHandler middlewares esCurrentReducers
    put ExcelsiorState { esCurrentMiddlewares = middlewares, esCurrentHandler = handler, .. }

{-# INLINE command #-}
command :: forall c state cmd. (Typeable state, MonadIO c, Command state cmd) => cmd -> c ()
command (toCommand -> sc) = void $ with (store (error "command: store not initialized" :: state) [] []) $ do
    state <- getO
    ExcelsiorState {..} <- get
    state' <- liftIO $ esCurrentHandler sc state

    -- phase 1: notify observers
    setO state'

    -- phase 2: call IO callbacks
    liftIO $ mapM_ (($ state') . snd) esIOCallbacks

{-# INLINE commands #-}
commands :: forall c state. (Typeable state, MonadIO c) => [SomeCommand state] -> c ()
commands commands = void $ with (store (error "commands: store not initialized" :: state) [] []) $ do
    state <- getO
    ExcelsiorState {..} <- get
    state' <- liftIO $ foldM (flip esCurrentHandler) state commands 

    -- phase 1: notify observers
    setO state'

    -- phase 2: call IO callbacks
    liftIO $ mapM_ (($ state') . snd) esIOCallbacks

{-# INLINE getStore #-}
getStore :: forall c state. (Typeable state, MonadIO c) => c (Promise state)
getStore = with (store (error "getStore: store not initialized" :: state) [] []) getO

{-# INLINE watchIO_ #-}
watchIO_ :: forall c state. (Typeable state, MonadIO c)
         => Bool -> (state -> IO ()) -> c (Promise (IO ()))
watchIO_ immediate f = with (store (error "watchIO: store not initialized" :: state) [] []) $ do
    u <- fresh
    ExcelsiorState {..} <- get

    -- this is an anti-pattern but we need to be able to remove the callback, 
    -- so we can't store a continuation (state -> IO ()). We also don't want to
    -- have to call reverse on the callbacks every time we send a command. This
    -- shouldn't be a performance issue, especially compared to observe overhead.
    let ioCallbacks = esIOCallbacks ++ [(u,f)]

    put ExcelsiorState { esIOCallbacks = ioCallbacks, .. }

    when immediate $ do
      state <- getO
      liftIO (f state)

    let remove = void $ with (store (error "watchIO.remove: store has been terminated" :: state) [] []) $ do
                   ExcelsiorState {..} :: ExcelsiorState state <- get
                   let ioCallbacks = filter ((/=) u . fst) esIOCallbacks
                   put ExcelsiorState { esIOCallbacks = ioCallbacks, .. }

    return remove

{-# INLINE watchIO #-}
watchIO :: forall c state. (Typeable state, MonadIO c)
        => (state -> IO ()) -> c (Promise (IO ()))
watchIO = watchIO_ False

{-# INLINE watchIO' #-}
watchIO' :: forall c state. (Typeable state, MonadIO c)
         => (state -> IO ()) -> c (Promise (IO ()))
watchIO' = watchIO_ True

{-# INLINE watch #-}
watch :: forall c ms state. (Typeable state, MonadIO c, ms <: '[Evented]) 
      => (state -> Ef '[Event state] (Ef ms c) ()) -> Ef ms c (Promise (IO ()))
watch = observe (store (error "watch: store not initialized" :: state) [] [])

{-# INLINE watch' #-}
watch' :: forall c ms state. (Typeable state, MonadIO c, ms <: '[Evented]) 
       => (state -> Ef ms c ()) -> Ef ms c (Promise (IO ()))
watch' = observe' (store (error "watch': store not initialized" :: state) [] [])

{-# INLINE excel #-}
excel :: (MVC model ms, Typeable state) => (state -> model ms -> model ms) -> Ef ms IO ()
excel f = void $ watch $ lift . modifyModel . f 

{-# INLINE excel' #-}
excel' :: (MVC model ms, Typeable state) => (state -> model ms -> model ms) -> Ef ms IO ()
excel' f = void $ watch' $ modifyModel . f