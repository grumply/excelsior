{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FunctionalDependencies, ViewPatterns, RecordWildCards, FlexibleInstances #-}
module Excelsior
  ( SomeCommand, Command(..), Reducer, Middleware, Callback, Excelsior(..)
  , reducer, middleware, command, watch, watch', unwatch, current
  ) where

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from base
import Control.Concurrent
import Control.Exception (try,SomeException)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Maybe
import Data.Traversable (for,traverse)
import Data.Typeable
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

-- from unordered-containers
import qualified Data.HashMap.Strict as HashMap

type Vault = IORef (HashMap.HashMap String Any)

{-# NOINLINE vault #-}
vault :: Vault
vault = unsafePerformIO (newIORef HashMap.empty)

addStore :: forall state. Typeable state => ExcelsiorState state -> IO ()
addStore e = atomicModifyIORef' vault $ \v -> (HashMap.insert (show (typeOf (undefined :: state))) (unsafeCoerce e) v,())

removeStore :: forall state. Typeable state => ExcelsiorState state -> IO ()
removeStore e = atomicModifyIORef' vault $ \v -> (HashMap.delete (show (typeOf (undefined :: state))) v,())

lookupStore :: forall state. Typeable state => IO (Maybe (ExcelsiorState state))
lookupStore = do
  v <- readIORef vault
  return $ fmap unsafeCoerce $ HashMap.lookup (show (typeOf (undefined :: state))) v

data SomeCommand state = forall e. Command state e => SomeCommand e

data Callback state = Callback
  { ecCallback :: IORef (state -> IO ())
  } deriving Eq

class Typeable c => Command state c | c -> state where
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

type Reducer state command = command -> state -> state

type Middleware state command = (forall cmd. Command state cmd => cmd -> IO state) -> command -> state -> IO state
type SomeReducer state = Reducer state (SomeCommand state)
type SomeMiddleware state = (SomeCommand state -> IO state) -> SomeCommand state -> state -> IO state

type Handler state = SomeCommand state -> state -> IO state

data ExcelsiorState_ state = ExcelsiorState
  { esState     :: state
  , esHandler   :: Handler state
  , esCallbacks :: [IORef (state -> IO ())]
  }

type ExcelsiorState state = MVar (ExcelsiorState_ state)

data Excelsior state = Excelsior
  { initial     :: state
  , reducers    :: [SomeReducer state]
  , middlewares :: [SomeMiddleware state]
  }

instance Typeable state => Pure (Excelsior state) where
  view =
    ComponentIO $ \self -> def
      { construct = do
          Excelsior {..} <- getProps self
          newMVar ExcelsiorState
            { esState = initial
            , esHandler = composeHandler middlewares reducers
            , esCallbacks = []
            }
      , mount = \store -> addStore store >> return store
      , receive = \newprops oldstate -> do
          modifyMVar_ oldstate $ \es -> return es
            { esState = initial newprops
            , esHandler = composeHandler (middlewares newprops) (reducers newprops)
            }
          callCallbacks oldstate
          return oldstate
      , unmount = getState self >>= removeStore
      }
    where
      composeHandler :: forall state. [SomeMiddleware state] -> [SomeReducer state] -> Handler state
      composeHandler middlewares reducers command state = handleMiddlewares command middlewares
        where
          handleMiddlewares :: SomeCommand state -> [SomeMiddleware state] -> IO state
          handleMiddlewares command (m:ms) = m (flip handleMiddlewares ms) command state
          handleMiddlewares command []     = return (reduce command state reducers)

          reduce = foldr . flip id

      callCallbacks :: ExcelsiorState state -> IO ()
      callCallbacks es_ = do
        es <- takeMVar es_
        cbs <- for (esCallbacks es) $ \cb_ -> do
          cb <- readIORef cb_
          eeu <- try (cb (esState es))
          case eeu of
            Left (_ :: SomeException) -> return Nothing
            Right _                   -> return (Just cb_)
        putMVar es_ es { esCallbacks = catMaybes cbs }

{-# INLINE reducer #-}
reducer :: (Command state cmd) => Reducer state cmd -> SomeReducer state
reducer f (fromCommand -> Just a) state = f a state
reducer _ _ state = state

{-# INLINE middleware #-}
middleware :: forall state cmd. (Command state cmd) => Middleware state cmd -> SomeMiddleware state
middleware m next (fromCommand -> Just a) state =
  let
      continue :: forall cmd'. Command state cmd' => cmd' -> IO state
      continue cmd = next (toCommand cmd)
  in
      m continue a state
middleware _ next command _ = next command

{-# INLINE command #-}
command :: forall state cmd. (Typeable state, Command state cmd) => cmd -> IO ()
command (toCommand -> sc) = lookupStore >>= traverse_ runCommand
  where
    runCommand es_ = do
      es <- takeMVar es_
      state' <- esHandler es sc (esState es)
      -- callbacks that throw exceptions are unceremoniously discarded
      cbs <- for (esCallbacks es) $ \cb_ -> do
        cb <- readIORef cb_
        eeu <- try (cb state')
        case eeu of
          Left (_ :: SomeException) -> return Nothing
          Right _                   -> return (Just cb_)
      putMVar es_ es { esState = state', esCallbacks = catMaybes cbs }

{-# INLINE watch #-}
watch :: forall state. (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch f = lookupStore >>= addStoreCallback
  where
    addStoreCallback Nothing = return Nothing
    addStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      f_ <- newIORef f
      let cb = Callback f_
      return (es { esCallbacks = f_ : esCallbacks es },Just cb)

{-# INLINE watch' #-}
watch' :: forall state. (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch' f = lookupStore >>= callAndAddStoreCallback
  where
    callAndAddStoreCallback Nothing = return Nothing
    callAndAddStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      f (esState es)
      f_ <- newIORef f
      let cb = Callback f_
      return (es { esCallbacks = f_ : esCallbacks es },Just cb)

{-# INLINE current #-}
current :: forall state. (Typeable state) => IO (Maybe state)
current = lookupStore >>= viewState
  where
    viewState Nothing = return Nothing
    viewState (Just es_) = (Just . esState) <$> readMVar es_

{-# INLINE unwatch #-}
unwatch :: forall state. (Typeable state) => Callback state -> IO ()
unwatch (Callback cb) = lookupStore >>= traverse_ removeStoreCallback
  where
    removeStoreCallback = flip modifyMVar_ $ \es ->
      return es { esCallbacks = filter (/= cb) (esCallbacks es) }
