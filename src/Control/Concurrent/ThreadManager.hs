{-|
  A simple thread management API inspired by the one in chapter
  24 of /Real World Haskell/.

  See <http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html>.

  Intended to be imported qualified (suggestion: TM).
 -}

module Control.Concurrent.ThreadManager
  ( ThreadManagerD, ThreadManager
  , ThreadStatus (..)
  , make
  , fork, forkD, forkn, forknD, getStatus, getData, waitFor, waitForAll
  ) where

import Control.Concurrent      (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar, readMVar)
import Control.Exception       (SomeException, try)
import Control.Monad           (join, replicateM, when)
import Control.Applicative     ((<$>))
import qualified Data.Map as M

data ThreadStatus =
    Running
  | Finished
  | Threw SomeException
  deriving Show

newtype ThreadManagerD a = TM (MVar (M.Map ThreadId (MVar (a, ThreadStatus))))
  deriving Eq
type ThreadManager = ThreadManagerD ()

-- | Make a thread manager.
make :: IO (ThreadManagerD a)
make = TM `fmap` newMVar M.empty

-- | Make a managed thread. Uses 'forkIO'.
fork :: ThreadManager -> IO () -> IO ThreadId
fork (TM tm) action =
    modifyMVar tm $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            r <- try action
            putMVar state ((), (either Threw (const Finished) r))
        return (M.insert tid state m, tid)

-- | Make a managed thread with additional initial data.
forkD :: ThreadManagerD a -> a -> IO () -> IO ThreadId
forkD (TM tm) tdata action =
    modifyMVar tm $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            r <- try action
            putMVar state (tdata, (either Threw (const Finished) r))
        return (M.insert tid state m, tid)

-- | Make the given number of managed threads.
forkn :: ThreadManagerD () -> Int -> IO () -> IO [ThreadId]
forkn tm n = replicateM n . fork tm

-- | forkn with initial data
forknD :: ThreadManagerD a -> Int -> a -> IO () -> IO [ThreadId]
forknD tm n tdata = replicateM n . forkD tm tdata

-- | Get the data associated with a managed thread.
getData :: ThreadManagerD a -> ThreadId -> IO (Maybe a)
getData (TM tm) tid =
    modifyMVar tm $ \m ->
      case M.lookup tid m of
        Nothing    -> return (m, Nothing)
        Just state -> tryTakeMVar state >>= \mst ->
          return $
            case mst of
              Nothing  -> (m, Nothing)
              Just (tdata, _) -> (M.delete tid m, Just tdata)

-- | Get the status of a managed thread.
getStatus :: ThreadManagerD a -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (TM tm) tid =
    modifyMVar tm $ \m ->
      case M.lookup tid m of
        Nothing    -> return (m, Nothing)
        Just state -> tryTakeMVar state >>= \mst ->
          return $
            case mst of
              Nothing  -> (m, Just Running)
              Just (_, sth) -> (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManagerD a -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (TM tm) tid =
    join . modifyMVar tm $ \m ->
      return $
        case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
          (Nothing, _)     -> (m, return Nothing)
          (Just state, m') -> (m', Just . snd <$> takeMVar state)

-- | Block until all managed threads terminate.
waitForAll :: ThreadManagerD a -> IO ()
waitForAll tm@(TM tmMvar) = do
    threadMap <- readMVar tmMvar
    let threads = M.keys threadMap
    statuses <- mapM (getStatus tm) threads
    _ <- mapM (waitFor tm) threads
    Control.Monad.when (foldr checkStatus False statuses) $
        waitForAll tm
  where
    checkStatus :: Maybe ThreadStatus -> Bool -> Bool
    checkStatus _ True = True
    checkStatus (Just Running) False = True
    checkStatus _ False = False
