{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool
  ( ThreadHandle (threadId, threadPool),
    ThreadPool,
    newThreadPool,
    newThread,
    newChildThread,
    pollSTM,
    waitCatchSTM,
    poll,
    waitCatch,
    cancelWith,
    numOfRunningThreads,
    cancelAllWith,
    alterIfPending,
    startTimeSTM,
    endTimeSTM,
    elapsedTimeSTM,
    startTime,
    endTime,
    elapsedTime,
    maybeStartTimeSTM,
    maybeEndTimeSTM,
    maybeElapsedTimeSTM,
    maybeStartTime,
    maybeEndTime,
    maybeElapsedTime,
    printThreadPoolStatus,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
    tryPutTMVar,
    tryReadTMVar,
  )
import Control.Exception
  ( Exception (fromException, toException),
    SomeException,
    catch,
    mask,
    throwIO,
  )
import Control.Monad (filterM, when)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import qualified Data.Map.Ordered as OM
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import GHC.IO (unsafePerformIO)

data CancellingException where
  CancellingException :: (Exception e) => e -> CancellingException

instance Show CancellingException where
  show (CancellingException e) = show e

instance Exception CancellingException

data PendingThread a = PendingThread
  { threadId :: ThreadId,
    threadAction :: IO a,
    threadResult :: TMVar (Either SomeException a),
    threadStartTime :: TMVar UTCTime,
    threadEndTime :: TMVar UTCTime
  }

data SomePendingThread where
  SomePendingThread :: (Typeable a) => PendingThread a -> SomePendingThread

threadIdCounter :: IORef Int
threadIdCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE threadIdCounter #-}

data SomeChildrenPendingThread = SomeChildrenPendingThread
  { pendingThread :: SomePendingThread,
    parents :: HS.HashSet ThreadId
  }

type ThreadId = Int

freshThreadId :: IO ThreadId
freshThreadId = atomicModifyIORef' threadIdCounter (\x -> (x + 1, x))

data Thread = Thread
  { _threadAsync :: Async.Async (),
    _threadCancellable :: MVar ()
  }

data ThreadPool = ThreadPool
  { lock :: MVar (),
    poolSize :: Int,
    running :: IORef (HM.HashMap ThreadId Thread),
    queue :: IORef (OM.OMap ThreadId SomePendingThread),
    childrenQueue :: IORef (HM.HashMap ThreadId SomeChildrenPendingThread),
    children :: IORef (HM.HashMap ThreadId (HS.HashSet ThreadId))
  }

printThreadPoolStatus :: ThreadPool -> IO ()
printThreadPoolStatus pool@ThreadPool {..} = do
  lockPool pool
  putStrLn "Running threads:"
  runningThreads <- readIORef running
  mapM_ print $ HM.keys runningThreads
  putStrLn "Pending threads:"
  pendingThreads <- readIORef queue
  mapM_ print $ fst <$> OM.assocs pendingThreads
  putStrLn "Children threads:"
  childrenThreads <- readIORef childrenQueue
  mapM_ print $ HM.keys childrenThreads
  putStrLn "Children:"
  childrenMap <- readIORef children
  print childrenMap
  unlockPool pool

numOfRunningThreads :: ThreadPool -> IO Int
numOfRunningThreads ThreadPool {..} = HM.size <$> readIORef running

data ThreadHandle a = ThreadHandle
  { threadId :: ThreadId,
    threadPool :: ThreadPool,
    threadResult :: TMVar (Either SomeException a),
    threadStartTime :: TMVar UTCTime,
    threadEndTime :: TMVar UTCTime
  }

instance Eq (ThreadHandle a) where
  ThreadHandle {threadId = a} == ThreadHandle {threadId = b} = a == b

instance Hashable (ThreadHandle a) where
  hashWithSalt salt ThreadHandle {threadId = threadId} =
    hashWithSalt salt threadId

instance Show (ThreadHandle a) where
  show ThreadHandle {threadId = threadId} = "ThreadHandle " ++ show threadId

lockPool :: ThreadPool -> IO ()
lockPool ThreadPool {..} = takeMVar lock

unlockPool :: ThreadPool -> IO ()
unlockPool ThreadPool {..} = putMVar lock ()

withLockedPool :: ThreadPool -> Bool -> IO b -> IO b
withLockedPool pool shouldCheckForStarts action = do
  lockPool pool
  result <-
    action `catch` \(e :: SomeException) -> do
      when shouldCheckForStarts $ startIfHaveSpaceImpl pool
      unlockPool pool
      throwIO e
  when shouldCheckForStarts $ startIfHaveSpaceImpl pool
  unlockPool pool
  return result

newThreadPool :: Int -> IO ThreadPool
newThreadPool poolSize = do
  lock <- newMVar ()
  running <- newIORef HM.empty
  queue <- newIORef OM.empty
  childrenQueue <- newIORef HM.empty
  children <- newIORef HM.empty
  return ThreadPool {..}

cleanupChildrenImpl :: ThreadPool -> ThreadId -> IO ()
cleanupChildrenImpl ThreadPool {..} threadId = do
  oldChildren <- readIORef children
  case HM.lookup threadId oldChildren of
    Just childrenSet -> do
      mapM_
        ( \childrenId -> do
            oldChildrenQueue <- readIORef childrenQueue
            let pendingChildren = oldChildrenQueue HM.! childrenId
            let newParentSet = HS.delete threadId $ parents pendingChildren
            if HS.null newParentSet
              then do
                writeIORef childrenQueue $ HM.delete childrenId oldChildrenQueue
                modifyIORef' queue $ \oldQueue ->
                  oldQueue OM.>| (childrenId, pendingThread pendingChildren)
              else do
                writeIORef childrenQueue $
                  HM.insert
                    childrenId
                    pendingChildren {parents = newParentSet}
                    oldChildrenQueue
        )
        $ HS.toList childrenSet
      writeIORef children $ HM.delete threadId oldChildren
    Nothing -> return ()

threadFun :: ThreadPool -> PendingThread a -> MVar () -> IO ()
threadFun pool@ThreadPool {..} PendingThread {..} cancellable =
  mask $ \restore -> do
    putMVar cancellable ()
    let writeResult result = do
          oldRunning <- readIORef running
          writeIORef running $ HM.delete threadId oldRunning
          cleanupChildrenImpl pool threadId
          atomically $ tryPutTMVar threadResult result
    getCurrentTime >>= atomically . tryPutTMVar threadStartTime
    result <-
      restore threadAction `catch` \(e :: SomeException) -> do
        getCurrentTime >>= atomically . tryPutTMVar threadEndTime
        case fromException e of
          Just (CancellingException e1) -> do
            writeResult (Left $ toException e1)
            throwIO e1
          Nothing -> withLockedPool pool True $ do
            writeResult (Left e)
            throwIO e
    withLockedPool pool True $ writeResult (Right result)
    getCurrentTime >>= atomically . tryPutTMVar threadEndTime
    return ()

startIfHaveSpaceImpl :: ThreadPool -> IO ()
startIfHaveSpaceImpl pool@ThreadPool {..} = do
  oldRunning <- readIORef running
  when (HM.size oldRunning < poolSize) $ do
    oldQueue <- readIORef queue
    case OM.elemAt oldQueue 0 of
      Just (_, SomePendingThread pendingThread@PendingThread {..}) -> do
        exception <- atomically $ tryReadTMVar threadResult
        writeIORef queue $ OM.delete threadId oldQueue
        case exception of
          Just _ -> return ()
          Nothing -> do
            cancellable <- newEmptyMVar
            threadAsync <-
              Async.async $ threadFun pool pendingThread cancellable
            writeIORef running $
              HM.insert threadId (Thread threadAsync cancellable) oldRunning
        startIfHaveSpaceImpl pool
      _ -> return ()

taskNotYetFinishedImpl :: ThreadPool -> ThreadId -> IO Bool
taskNotYetFinishedImpl ThreadPool {..} threadId = do
  oldRunning <- readIORef running
  oldQueue <- readIORef queue
  oldChildrenQueue <- readIORef childrenQueue
  return $
    HM.member threadId oldRunning
      || OM.member threadId oldQueue
      || HM.member threadId oldChildrenQueue

newChildThread ::
  forall a.
  (Typeable a) =>
  ThreadPool ->
  [ThreadId] ->
  IO a ->
  IO (ThreadHandle a)
newChildThread threadPool@ThreadPool {..} parents threadAction =
  withLockedPool threadPool True $ do
    threadId <- freshThreadId
    threadResult <- newEmptyTMVarIO :: IO (TMVar (Either SomeException a))
    threadStartTime <- newEmptyTMVarIO :: IO (TMVar UTCTime)
    threadEndTime <- newEmptyTMVarIO :: IO (TMVar UTCTime)
    filteredParents <- filterM (taskNotYetFinishedImpl threadPool) parents
    if null filteredParents
      then do
        oldQueue <- readIORef queue
        writeIORef queue $
          oldQueue
            OM.>| (threadId, SomePendingThread $ PendingThread {..})
      else do
        modifyIORef' childrenQueue $
          HM.insert
            threadId
            SomeChildrenPendingThread
              { pendingThread = SomePendingThread $ PendingThread {..},
                parents = HS.fromList filteredParents
              }
        mapM_
          ( \parent -> do
              oldChildren <- readIORef children
              let oldChildrenOfParent = oldChildren HM.! parent
              modifyIORef' children $
                HM.insert parent $
                  HS.insert threadId oldChildrenOfParent
          )
          filteredParents
    modifyIORef' children $ HM.insert threadId HS.empty
    startIfHaveSpaceImpl threadPool
    return $ ThreadHandle {..}

newThread ::
  forall a. (Typeable a) => ThreadPool -> IO a -> IO (ThreadHandle a)
newThread threadPool = newChildThread threadPool []

cancelWithImpl :: (Exception e) => ThreadPool -> e -> ThreadId -> IO ()
cancelWithImpl pool@ThreadPool {..} e threadId = do
  oldRunning <- readIORef running
  oldQueue <- readIORef queue
  oldChildrenQueue <- readIORef childrenQueue
  case HM.lookup threadId oldRunning of
    Just (Thread threadAsync cancellable) -> do
      takeMVar cancellable
      Async.cancelWith threadAsync $ CancellingException e
      startIfHaveSpaceImpl pool
    Nothing -> case OM.lookup threadId oldQueue of
      Just pending -> do
        cancelSomePendingThread pending
        modifyIORef' queue $ OM.delete threadId
        cleanupChildrenImpl pool threadId
      _ -> case HM.lookup threadId oldChildrenQueue of
        Just (SomeChildrenPendingThread pending _) -> do
          cancelSomePendingThread pending
          modifyIORef' childrenQueue $ HM.delete threadId
          cleanupChildrenImpl pool threadId
        Nothing -> return ()
  where
    cancelSomePendingThread (SomePendingThread PendingThread {..}) = do
      time <- getCurrentTime
      atomically $ do
        tryPutTMVar threadResult (Left $ toException e)
        tryPutTMVar threadStartTime time
        tryPutTMVar threadEndTime time
      return ()

cancelWith :: (Exception e) => e -> ThreadHandle a -> IO ()
cancelWith e ThreadHandle {threadPool = pool@ThreadPool {..}, threadId} =
  withLockedPool pool True $ cancelWithImpl pool e threadId

cancelAllWith :: (Exception e) => ThreadPool -> e -> IO ()
cancelAllWith pool@ThreadPool {..} e = withLockedPool pool True $ do
  oldPending <- readIORef queue
  mapM_
    ( \(SomePendingThread PendingThread {..}) -> do
        time <- getCurrentTime
        atomically $ do
          tryPutTMVar threadResult (Left $ toException e)
          tryPutTMVar threadStartTime time
          tryPutTMVar threadEndTime time
    )
    $ snd <$> OM.assocs oldPending
  oldRunning <- readIORef running
  mapM_
    ( \(Thread threadAsync cancellable) -> do
        takeMVar cancellable
        Async.cancelWith threadAsync $ CancellingException e
    )
    $ HM.elems oldRunning

pollSTM ::
  (Typeable a) => ThreadHandle a -> STM (Maybe (Either SomeException a))
pollSTM ThreadHandle {threadResult} = tryReadTMVar threadResult

poll :: (Typeable a) => ThreadHandle a -> IO (Maybe (Either SomeException a))
poll = atomically . pollSTM

waitCatchSTM :: (Typeable a) => ThreadHandle a -> STM (Either SomeException a)
waitCatchSTM ThreadHandle {threadResult} = readTMVar threadResult

waitCatch :: (Typeable a) => ThreadHandle a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

alterIfPending :: forall a. (Typeable a) => ThreadHandle a -> IO a -> IO ()
alterIfPending
  ThreadHandle {threadId, threadPool = pool@ThreadPool {..}}
  newAction =
    withLockedPool pool False $ do
      oldQueue <- readIORef queue
      case OM.lookup threadId oldQueue of
        Just (SomePendingThread (PendingThread {..} :: PendingThread a1)) -> do
          case eqT @a @a1 of
            Just Refl -> do
              writeIORef queue $
                OM.alter
                  ( const $
                      Just $
                        SomePendingThread $
                          PendingThread {threadAction = newAction, ..}
                  )
                  threadId
                  oldQueue
              startIfHaveSpaceImpl pool
            Nothing -> error "alterIfPending: type mismatch"
        Nothing -> return ()

startTimeSTM :: ThreadHandle a -> STM UTCTime
startTimeSTM ThreadHandle {threadStartTime} = readTMVar threadStartTime

endTimeSTM :: ThreadHandle a -> STM UTCTime
endTimeSTM ThreadHandle {threadEndTime} = readTMVar threadEndTime

elapsedTimeSTM :: ThreadHandle a -> STM NominalDiffTime
elapsedTimeSTM ThreadHandle {threadStartTime, threadEndTime} = do
  startTime <- readTMVar threadStartTime
  endTime <- readTMVar threadEndTime
  return $ diffUTCTime endTime startTime

startTime :: ThreadHandle a -> IO UTCTime
startTime = atomically . startTimeSTM

endTime :: ThreadHandle a -> IO UTCTime
endTime = atomically . endTimeSTM

elapsedTime :: ThreadHandle a -> IO NominalDiffTime
elapsedTime = atomically . elapsedTimeSTM

maybeStartTimeSTM :: ThreadHandle a -> STM (Maybe UTCTime)
maybeStartTimeSTM ThreadHandle {threadStartTime} = tryReadTMVar threadStartTime

maybeEndTimeSTM :: ThreadHandle a -> STM (Maybe UTCTime)
maybeEndTimeSTM ThreadHandle {threadEndTime} = tryReadTMVar threadEndTime

maybeElapsedTimeSTM :: ThreadHandle a -> STM (Maybe NominalDiffTime)
maybeElapsedTimeSTM ThreadHandle {threadStartTime, threadEndTime} = do
  maybeStartTime <- tryReadTMVar threadStartTime
  maybeEndTime <- tryReadTMVar threadEndTime
  case (maybeStartTime, maybeEndTime) of
    (Just startTime, Just endTime) ->
      return $ Just $ diffUTCTime endTime startTime
    _ -> return Nothing

maybeStartTime :: ThreadHandle a -> IO (Maybe UTCTime)
maybeStartTime = atomically . maybeStartTimeSTM

maybeEndTime :: ThreadHandle a -> IO (Maybe UTCTime)
maybeEndTime = atomically . maybeEndTimeSTM

maybeElapsedTime :: ThreadHandle a -> IO (Maybe NominalDiffTime)
maybeElapsedTime = atomically . maybeElapsedTimeSTM
