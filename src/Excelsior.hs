{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FunctionalDependencies, ViewPatterns, RecordWildCards, FlexibleInstances #-}
module Excelsior
  ( SomeCommand, Command(..), Reducer, Middleware, Callback, Excelsior(..)
  , reducer, middleware
  , command, commandNS
  , watch, watchNS
  , watch', watchNS'
  , currentState
  , lookupState, lookupStateNS
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
import System.Mem.Weak
import Unsafe.Coerce

-- from unordered-containers
import qualified Data.HashMap.Strict as HashMap

type Vault = IORef (HashMap.HashMap String Any)

{-# NOINLINE vault #-}
vault :: Vault
vault = unsafePerformIO (newIORef HashMap.empty)

addStore :: forall state. Typeable state => ExcelsiorState state -> IO ()
addStore e = atomicModifyIORef' vault $ \v -> (HashMap.insert (show (typeOf (undefined :: state))) (unsafeCoerce e) v,())

addStoreNS :: forall state. Typeable state => String -> ExcelsiorState state -> IO ()
addStoreNS ns e = atomicModifyIORef' vault $ \v -> (HashMap.insert (ns ++ ":" ++ show (typeOf (undefined :: state))) (unsafeCoerce e) v,())

removeStore :: forall state. Typeable state => ExcelsiorState state -> IO ()
removeStore e = atomicModifyIORef' vault $ \v -> (HashMap.delete (show (typeOf (undefined :: state))) v,())

removeStoreNS :: forall state. Typeable state => String -> ExcelsiorState state -> IO ()
removeStoreNS ns e = atomicModifyIORef' vault $ \v -> (HashMap.delete (ns ++ ":" ++ show (typeOf (undefined :: state))) v,())

lookupStore :: forall state. Typeable state => IO (Maybe (ExcelsiorState state))
lookupStore = do
  v <- readIORef vault
  return $ fmap unsafeCoerce $ HashMap.lookup (show (typeOf (undefined :: state))) v

lookupStoreNS :: forall state. Typeable state => String -> IO (Maybe (ExcelsiorState state))
lookupStoreNS ns = do
  v <- readIORef vault
  return $ fmap unsafeCoerce $ HashMap.lookup (ns ++ ":" ++ show (typeOf (undefined :: state))) v

data SomeCommand state = forall e. Command state e => SomeCommand e

data Callback state = Callback
  { ecNamespace :: Maybe String
  , ecStateRef :: IORef state
  , ecCallback :: IORef (state -> IO ())
  , ecWeakKey  :: Weak ThreadId
  }

instance Eq (Callback state) where
  (==) c1 c2 = ecCallback c1 == ecCallback c2

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
  { esState     :: IORef state
  , esHandler   :: Handler state
  , esCallbacks :: [(Weak ThreadId,IORef (state -> IO ()))]
  }

type ExcelsiorState state = MVar (ExcelsiorState_ state)

data Excelsior state
  = Excelsior
      { initial     :: state
      , reducers    :: [SomeReducer state]
      , middlewares :: [SomeMiddleware state]
      }
  | ExcelsiorNS
      { namespace   :: String
      , initial     :: state
      , reducers    :: [SomeReducer state]
      , middlewares :: [SomeMiddleware state]
      }

instance Typeable state => Pure (Excelsior state) where
  view =
    ComponentIO $ \self -> def
      { construct = do
          e <- getProps self
          ess <- newIORef (initial e)
          newMVar ExcelsiorState
            { esState = ess
            , esHandler = composeHandler (middlewares e) (reducers e)
            , esCallbacks = []
            }
      , mount = \store -> do
          e <- getProps self
          case e of
            ExcelsiorNS {..} -> addStoreNS namespace store >> return store
            _ -> addStore store >> return store
      , receive = \newprops oldstate -> do
          oldprops <- getProps self
          let update = modifyMVar_ oldstate $ \es -> do
                         modifyIORef (esState es) (const (initial newprops))
                         return es { esHandler = composeHandler (middlewares newprops) (reducers newprops) }
          case oldprops of
            ExcelsiorNS {} ->
              case newprops of
                ExcelsiorNS {}
                  | namespace oldprops == namespace newprops -> do
                      update
                      runCallbacks oldstate
                      return oldstate
                  | otherwise -> do
                      removeStoreNS (namespace oldprops) oldstate
                      ess <- newIORef (initial newprops)
                      newstate <- newMVar ExcelsiorState
                        { esState = ess
                        , esHandler = composeHandler (middlewares newprops) (reducers newprops)
                        , esCallbacks = []
                        }
                      addStoreNS (namespace newprops) newstate
                      return newstate
                _ -> do
                      removeStoreNS (namespace oldprops) oldstate
                      ess <- newIORef (initial newprops)
                      newstate <- newMVar ExcelsiorState
                        { esState = ess
                        , esHandler = composeHandler (middlewares newprops) (reducers newprops)
                        , esCallbacks = []
                        }
                      addStore newstate
                      return newstate
            _ -> do
              update
              runCallbacks oldstate
              return oldstate
      , unmount = do
          props <- getProps self
          case props of
            ExcelsiorNS {} -> getState self >>= removeStoreNS (namespace props)
            _              -> getState self >>= removeStore
      }
    where
      composeHandler :: forall state. [SomeMiddleware state] -> [SomeReducer state] -> Handler state
      composeHandler middlewares reducers command state = handleMiddlewares command middlewares
        where
          handleMiddlewares :: SomeCommand state -> [SomeMiddleware state] -> IO state
          handleMiddlewares command (m:ms) = m (flip handleMiddlewares ms) command state
          handleMiddlewares command []     = return (reduce command state reducers)

          reduce = foldr . flip id

{-# INLINE runCallbacks #-}
runCallbacks :: ExcelsiorState state -> IO ()
runCallbacks es_ = do
    es  <- takeMVar es_
    st  <- readIORef (esState es)
    cbs <- for (esCallbacks es) $ \(tid_,f_) -> do
      mtid <- deRefWeak tid_
      case mtid of
        Nothing -> return Nothing
        Just _ -> do
          f <- readIORef f_
          eeu <- try (f st)
          case eeu of
            Left (_ :: SomeException) -> return Nothing
            Right _                   -> return (Just (tid_,f_))
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
command (toCommand -> sc) = lookupStore >>= traverse_ (\es_ -> runCommand sc es_ >> runCallbacks es_)

{-# INLINE commandNS #-}
commandNS :: forall state cmd. (Typeable state, Command state cmd) => String -> cmd -> IO ()
commandNS ns (toCommand -> sc) = lookupStoreNS ns >>= traverse_ (\es_ -> runCommand sc es_ >> runCallbacks es_)

{-# INLINE runCommand #-}
runCommand :: SomeCommand state -> ExcelsiorState state -> IO ()
runCommand sc es_ = withMVar es_ $ \es -> do
  st <- readIORef (esState es)
  st' <- esHandler es sc st
  writeIORef (esState es) st'

{-# INLINE watch #-}
watch :: (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch f = lookupStore >>= addStoreCallback
  where
    addStoreCallback Nothing = return Nothing
    addStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      tid_ <- mkWeakThreadId =<< myThreadId
      f_ <- newIORef f
      return (es { esCallbacks = (tid_,f_) : esCallbacks es },Just (Callback Nothing (esState es) f_ tid_))

{-# INLINE watchNS #-}
watchNS :: (Typeable state) => String -> (state -> IO ()) -> IO (Maybe (Callback state))
watchNS ns f = lookupStoreNS ns >>= addStoreCallback
  where
    addStoreCallback Nothing = return Nothing
    addStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      tid_ <- mkWeakThreadId =<< myThreadId
      f_ <- newIORef f
      return (es { esCallbacks = (tid_,f_) : esCallbacks es },Just (Callback (Just ns) (esState es) f_ tid_))

{-# INLINE watch' #-}
watch' :: (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch' f = lookupStore >>= callAndAddStoreCallback
  where
    callAndAddStoreCallback Nothing = return Nothing
    callAndAddStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      s <- readIORef (esState es)
      f s
      tid_ <- mkWeakThreadId =<< myThreadId
      f_ <- newIORef f
      return (es { esCallbacks = (tid_,f_) : esCallbacks es },Just (Callback Nothing (esState es) f_ tid_))

{-# INLINE watchNS' #-}
watchNS' :: (Typeable state) => String -> (state -> IO ()) -> IO (Maybe (Callback state))
watchNS' ns f = lookupStoreNS ns >>= callAndAddStoreCallback
  where
    callAndAddStoreCallback Nothing = return Nothing
    callAndAddStoreCallback (Just es_) = modifyMVar es_ $ \es -> do
      s <- readIORef (esState es)
      f s
      tid_ <- mkWeakThreadId =<< myThreadId
      f_ <- newIORef f
      return (es { esCallbacks = (tid_,f_) : esCallbacks es },Just (Callback (Just ns) (esState es) f_ tid_))

{-# INLINE currentState #-}
currentState :: Callback state -> IO state
currentState (Callback _ st_ _ _) = readIORef st_

{-# INLINE lookupState #-}
lookupState :: forall state. (Typeable state) => IO (Maybe state)
lookupState = lookupStore >>= viewState
  where
    viewState Nothing = return Nothing
    viewState (Just es_) = do
      es <- readMVar es_
      Just <$> readIORef (esState es)

{-# INLINE lookupStateNS #-}
lookupStateNS :: forall state. (Typeable state) => String -> IO (Maybe state)
lookupStateNS ns = lookupStoreNS ns >>= viewState
  where
    viewState Nothing = return Nothing
    viewState (Just es_) = do
      es <- readMVar es_
      Just <$> readIORef (esState es)

{-# INLINE unwatch #-}
unwatch :: forall state. Typeable state => Callback state -> IO ()
unwatch (Callback mns _ f0_ _) = do
  mst :: Maybe (ExcelsiorState state) <-
    case mns of
      Just ns -> lookupStoreNS ns
      Nothing -> lookupStore
  case mst of
    Nothing -> return ()
    Just es_ -> modifyMVar_ es_ $ \es -> return (es { esCallbacks = filter (\(tid_,f_) -> f_ /= f0_) (esCallbacks es) })
