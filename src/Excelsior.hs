{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FunctionalDependencies, ViewPatterns, RecordWildCards, FlexibleInstances, PatternSynonyms #-}
module Excelsior where

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
import Prelude
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
  , ecStateRef  :: IORef state
  , ecCallback  :: IORef (state -> IO ())
  , ecWeakKey   :: Weak ThreadId
  , ecWeakStore :: Weak (ExcelsiorState state)
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

type Middleware state command = (command -> state -> IO state) -> command -> state -> IO state
type SomeReducer state = Reducer state (SomeCommand state)
type SomeMiddleware state = (SomeCommand state -> state -> IO state) -> SomeCommand state -> state -> IO state

type Handler state = SomeCommand state -> state -> IO state

data ExcelsiorState_ state = ExcelsiorState
  { esState       :: IORef state
  , esHandler     :: Handler state -- is there a reason to keep this around and not re-build it on each command?
  , esReducers    :: [SomeReducer state]
  , esMiddlewares :: [SomeMiddleware state]
  , esCallbacks   :: [(Weak ThreadId,IORef (state -> IO ()))]
  }

type ExcelsiorState state = MVar (ExcelsiorState_ state)

{-# INLINE excelsior #-}
excelsior :: state -> [SomeReducer state] -> [SomeMiddleware state] -> IO (ExcelsiorState state)
excelsior st rs ms = do
  st_ <- newIORef st
  newMVar ExcelsiorState
    { esState = st_
    , esReducers = rs
    , esMiddlewares = ms
    , esHandler = composeHandler rs ms
    , esCallbacks = []
    }

data Excelsior state
  = Excelsior_
      { initial     :: state
      , reducers    :: [SomeReducer state]
      , middlewares :: [SomeMiddleware state]
      }
  | ExcelsiorNS_
      { namespace   :: String
      , initial     :: state
      , reducers    :: [SomeReducer state]
      , middlewares :: [SomeMiddleware state]
      }

pattern Excelsior :: Typeable state => state -> [SomeReducer state] -> [SomeMiddleware state] -> View
pattern Excelsior initial reducers middlewares = View (Excelsior_ initial reducers middlewares)

pattern ExcelsiorNS :: Typeable state => String -> state -> [SomeReducer state] -> [SomeMiddleware state] -> View
pattern ExcelsiorNS namespace initial reducers middlewares = View (ExcelsiorNS_ namespace initial reducers middlewares)

createStore :: Typeable state => state -> [SomeReducer state] -> [SomeMiddleware state] -> View
createStore = Excelsior

createStoreNS :: Typeable state => String -> state -> [SomeReducer state] -> [SomeMiddleware state] -> View
createStoreNS = ExcelsiorNS

instance Typeable state => Pure (Excelsior state) where
  view =
    Component $ \self -> def
      { construct = do
          e <- ask self
          excelsior (initial e) (reducers e) (middlewares e)

      , mount = \store -> do
          e <- ask self
          case e of
            ExcelsiorNS_ {..} -> addStoreNS namespace store >> return store
            _ -> addStore store >> return store
      , receive = \newprops oldstate -> do
          oldprops <- ask self
          let update = modifyMVar_ oldstate $ \es -> do
                         modifyIORef (esState es) (const (initial newprops))
                         return es { esHandler = composeHandler (reducers newprops) (middlewares newprops) }
          case oldprops of
            ExcelsiorNS_ {} ->
              case newprops of
                ExcelsiorNS_ {}
                  | namespace oldprops == namespace newprops -> do
                      update
                      runCallbacks oldstate
                      return oldstate
                  | otherwise -> do
                      removeStoreNS (namespace oldprops) oldstate
                      ess <- newIORef (initial newprops)
                      newstate <- newMVar ExcelsiorState
                        { esState = ess
                        , esReducers = reducers newprops
                        , esMiddlewares = middlewares newprops
                        , esHandler = composeHandler (reducers newprops) (middlewares newprops)
                        , esCallbacks = []
                        }
                      addStoreNS (namespace newprops) newstate
                      return newstate
                _ -> do
                      removeStoreNS (namespace oldprops) oldstate
                      ess <- newIORef (initial newprops)
                      newstate <- newMVar ExcelsiorState
                        { esState = ess
                        , esReducers = reducers newprops
                        , esMiddlewares = middlewares newprops
                        , esHandler = composeHandler (reducers newprops) (middlewares newprops)
                        , esCallbacks = []
                        }
                      addStore newstate
                      return newstate
            _ -> do
              update
              runCallbacks oldstate
              return oldstate
      , unmounted = do
          props <- ask self
          case props of
            ExcelsiorNS_ {} -> get self >>= removeStoreNS (namespace props)
            _              -> get self >>= removeStore
      }

{-# INLINE composeHandler #-}
composeHandler :: forall state. [SomeReducer state] -> [SomeMiddleware state] -> Handler state
composeHandler reducers = handleMiddlewares
  where
    handleMiddlewares :: [SomeMiddleware state] -> SomeCommand state -> state -> IO state
    handleMiddlewares (m:ms) command state = m (handleMiddlewares ms) command state
    handleMiddlewares []     command state = return (reduce command state reducers)

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
      continue :: forall cmd'. Command state cmd' => cmd' -> state -> IO state
      continue cmd = next (toCommand cmd)
  in
      m continue a state
middleware _ next command state = next command state

{-# INLINE runCommand #-}
runCommand :: SomeCommand state -> ExcelsiorState state -> IO ()
runCommand sc es_ = withMVar es_ $ \es -> do
  st <- readIORef (esState es)
  st' <- esHandler es sc st
  writeIORef (esState es) st'

{-# INLINE command #-}
command :: forall state cmd. (Typeable state, Command state cmd) => cmd -> IO ()
command (toCommand -> sc) = lookupStore >>= traverse_ (\es_ -> runCommand sc es_ >> runCallbacks es_)

{-# INLINE commandNS #-}
commandNS :: forall state cmd. (Typeable state, Command state cmd) => String -> cmd -> IO ()
commandNS ns (toCommand -> sc) = lookupStoreNS ns >>= traverse_ (\es_ -> runCommand sc es_ >> runCallbacks es_)

{-# INLINE watchWith #-}
watchWith :: (Typeable state) => (state -> IO ()) -> ExcelsiorState state -> IO (Callback state)
watchWith f es_ = modifyMVar es_ $ \es -> do
  tid_  <- mkWeakThreadId =<< myThreadId
  f_    <- newIORef f
  wref_ <- mkWeakMVar es_ (return ())
  return (es { esCallbacks = (tid_,f_) : esCallbacks es },Callback Nothing (esState es) f_ tid_ wref_)

{-# INLINE watch #-}
watch :: (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch f = lookupStore >>= traverse (watchWith f)

{-# INLINE watchNS #-}
watchNS :: (Typeable state) => String -> (state -> IO ()) -> IO (Maybe (Callback state))
watchNS ns f = lookupStoreNS ns >>= traverse (watchWith f) 

{-# INLINE watchWith' #-}
watchWith' :: (Typeable state) => (state -> IO ()) -> ExcelsiorState state -> IO (Callback state)
watchWith' f es_ = do
  (run,cb) <- modifyMVar es_ $ \es -> do
    s <- readIORef (esState es)
    tid_ <- mkWeakThreadId =<< myThreadId
    f_ <- newIORef f
    wref_ <- mkWeakMVar es_ (return ())
    return (es { esCallbacks = (tid_,f_) : esCallbacks es },(f s,Callback Nothing (esState es) f_ tid_ wref_))
  run
  return cb

{-# INLINE watch' #-}
watch' :: (Typeable state) => (state -> IO ()) -> IO (Maybe (Callback state))
watch' f = lookupStore >>= traverse (watchWith' f)

{-# INLINE watchNS' #-}
watchNS' :: (Typeable state) => String -> (state -> IO ()) -> IO (Maybe (Callback state))
watchNS' ns f = lookupStoreNS ns >>= traverse (watchWith' f)

{-# INLINE currentState #-}
currentState :: Callback state -> IO state
currentState (Callback _ st_ _ _ _) = readIORef st_

{-# INLINE lookupStateWith #-}
lookupStateWith :: ExcelsiorState state -> IO state
lookupStateWith es_ = do
  es <- readMVar es_
  readIORef (esState es)

{-# INLINE lookupState #-}
lookupState :: (Typeable state) => IO (Maybe state)
lookupState = lookupStore >>= traverse lookupStateWith

{-# INLINE lookupStateNS #-}
lookupStateNS :: (Typeable state) => String -> IO (Maybe state)
lookupStateNS ns = lookupStoreNS ns >>= traverse lookupStateWith

{-# INLINE unwatch #-}
unwatch :: forall state. Typeable state => Callback state -> IO ()
unwatch (Callback _ _ f0_ _ wref_) = do
  mst <- deRefWeak wref_
  case mst of
    Nothing -> return ()
    Just es_ -> modifyMVar_ es_ $ \es -> return (es { esCallbacks = filter (\(tid_,f_) -> f_ /= f0_) (esCallbacks es) })

{-# INLINE watched #-}
watched :: ExcelsiorState state -> IO Bool
watched = fmap not . unwatched

{-# INLINE unwatched #-}
unwatched :: ExcelsiorState state -> IO Bool
unwatched es_ = do
  mes <- tryReadMVar es_
  case mes of
    Nothing -> return False -- assume it is watched if it is in use
    Just es -> return $ Prelude.null (esCallbacks es)

{-# INLINE watcher #-}
-- If no store exists for the `a`, this is equivalent to `\_ -> Null`
watcher :: Typeable a => (a -> View) -> View
watcher w = flip Component () $ \self ->
  def
    { construct = lookupState
    , executing = \a -> do
      watch (\a -> modify_ self $ \_ _ -> a)
      pure a
    , render = \_ -> maybe Null w
    }
