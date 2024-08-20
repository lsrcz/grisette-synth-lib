{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool
  ( CancellingException,
    ThreadHandle (threadId, threadPool),
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
    freezePool,
    unfreezePool,
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
import Control.Monad (filterM, unless, when)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashPSQ as PSQ
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
    pendingPriority :: Double,
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
    -- | Thread id, priority, pending thread. Smaller number is higher priority.
    queue :: IORef (PSQ.HashPSQ ThreadId Double SomePendingThread),
    childrenQueue :: IORef (HM.HashMap ThreadId SomeChildrenPendingThread),
    children :: IORef (HM.HashMap ThreadId (HS.HashSet ThreadId)),
    frozen :: IORef Bool
  }

printThreadPoolStatus :: ThreadPool -> IO ()
printThreadPoolStatus pool@ThreadPool {..} = do
  lockPool pool
  putStrLn "Running threads:"
  runningThreads <- readIORef running
  mapM_ print $ HM.keys runningThreads
  putStrLn "Pending threads:"
  pendingThreads <- readIORef queue
  mapM_ print $ PSQ.keys pendingThreads
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
withLockedPool pool shouldCheckForStarts action =
  mask $ \restore -> do
    lockPool pool
    result <-
      restore action `catch` \(e :: SomeException) -> do
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
  queue <- newIORef PSQ.empty
  childrenQueue <- newIORef HM.empty
  children <- newIORef HM.empty
  frozen <- newIORef False
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
                  PSQ.insert
                    childrenId
                    (pendingPriority pendingChildren)
                    (pendingThread pendingChildren)
                    oldQueue
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

threadFun ::
  forall a.
  (forall x. IO x -> IO x) ->
  ThreadPool ->
  PendingThread a ->
  MVar () ->
  IO ()
threadFun unmask pool@ThreadPool {..} PendingThread {..} cancellable = do
  putMVar cancellable ()
  let writeResult :: Either SomeException a -> IO ()
      writeResult result = do
        modifyIORef' running $ HM.delete threadId
        cleanupChildrenImpl pool threadId
        atomically $ tryPutTMVar threadResult result
        getCurrentTime >>= atomically . tryPutTMVar threadEndTime
        return ()
  getCurrentTime >>= atomically . tryPutTMVar threadStartTime
  let handler (e :: SomeException) =
        ( do
            getCurrentTime >>= atomically . tryPutTMVar threadEndTime
            case fromException e of
              Just (CancellingException e1) ->
                writeResult (Left $ toException e1)
              Nothing -> withLockedPool pool True $ writeResult (Left e)
        )
          `catch` handler
  unmask
    (threadAction >>= withLockedPool pool True . writeResult . Right)
    `catch` handler

startIfHaveSpaceImpl :: ThreadPool -> IO ()
startIfHaveSpaceImpl pool@ThreadPool {..} = do
  isFrozen <- readIORef frozen
  unless isFrozen $ do
    oldRunning <- readIORef running
    when (HM.size oldRunning < poolSize) $ do
      oldQueue <- readIORef queue
      case PSQ.findMin oldQueue of
        Just (_, _, SomePendingThread pendingThread@PendingThread {..}) -> do
          exception <- atomically $ tryReadTMVar threadResult
          writeIORef queue $ PSQ.delete threadId oldQueue
          case exception of
            Just _ -> return ()
            Nothing -> do
              cancellable <- newEmptyMVar
              threadAsync <-
                Async.asyncWithUnmask $ \unmask ->
                  threadFun unmask pool pendingThread cancellable
              writeIORef running $
                HM.insert threadId (Thread threadAsync cancellable) oldRunning
          startIfHaveSpaceImpl pool
        _ -> return ()

freezePool :: ThreadPool -> IO ()
freezePool pool = withLockedPool pool False $ do
  writeIORef (frozen pool) True

unfreezePool :: ThreadPool -> IO ()
unfreezePool pool = withLockedPool pool True $ do
  writeIORef (frozen pool) False

taskNotYetFinishedImpl :: ThreadPool -> ThreadId -> IO Bool
taskNotYetFinishedImpl ThreadPool {..} threadId = do
  oldRunning <- readIORef running
  oldQueue <- readIORef queue
  oldChildrenQueue <- readIORef childrenQueue
  return $
    HM.member threadId oldRunning
      || PSQ.member threadId oldQueue
      || HM.member threadId oldChildrenQueue

newChildThread ::
  forall a.
  (Typeable a) =>
  ThreadPool ->
  [ThreadId] ->
  Double ->
  IO a ->
  IO (ThreadHandle a)
newChildThread threadPool@ThreadPool {..} parents priority threadAction =
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
          PSQ.insert
            threadId
            priority
            (SomePendingThread $ PendingThread {..})
            oldQueue
      else do
        modifyIORef' childrenQueue $
          HM.insert
            threadId
            SomeChildrenPendingThread
              { pendingThread = SomePendingThread $ PendingThread {..},
                pendingPriority = priority,
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
  forall a. (Typeable a) => ThreadPool -> Double -> IO a -> IO (ThreadHandle a)
newThread threadPool = newChildThread threadPool []

cancelSomePendingThreadImpl :: (Exception e) => e -> SomePendingThread -> IO ()
cancelSomePendingThreadImpl e (SomePendingThread PendingThread {..}) = do
  time <- getCurrentTime
  atomically $ do
    tryPutTMVar threadResult (Left $ toException e)
    tryPutTMVar threadStartTime time
    tryPutTMVar threadEndTime time
  return ()

cancelWith :: (Exception e) => e -> ThreadHandle a -> IO ()
cancelWith e ThreadHandle {threadPool = pool@ThreadPool {..}, threadId} = do
  maybeThreadToCancel <- withLockedPool pool True $ do
    oldRunning <- readIORef running
    oldQueue <- readIORef queue
    oldChildrenQueue <- readIORef childrenQueue
    case HM.lookup threadId oldRunning of
      Just thread -> return $ Just thread
      Nothing -> case PSQ.lookup threadId oldQueue of
        Just (_, pending) -> do
          cancelSomePendingThreadImpl e pending
          modifyIORef' queue $ PSQ.delete threadId
          cleanupChildrenImpl pool threadId
          return Nothing
        _ -> case HM.lookup threadId oldChildrenQueue of
          Just (SomeChildrenPendingThread pending _ _) -> do
            cancelSomePendingThreadImpl e pending
            modifyIORef' childrenQueue $ HM.delete threadId
            cleanupChildrenImpl pool threadId
            return Nothing
          Nothing -> return Nothing
  case maybeThreadToCancel of
    Just (Thread threadAsync cancellable) -> do
      takeMVar cancellable
      Async.cancelWith threadAsync $ CancellingException e
      putMVar cancellable ()
    Nothing -> return ()

cancelAllWith :: (Exception e) => ThreadPool -> e -> IO ()
cancelAllWith pool@ThreadPool {..} e = do
  oldRunning <- withLockedPool pool True $ do
    oldPending <- readIORef queue
    mapM_ (cancelSomePendingThreadImpl e) $
      (\(_, _, v) -> v) <$> PSQ.toList oldPending
    writeIORef queue PSQ.empty
    oldChildrenQueue <- readIORef childrenQueue
    mapM_ (cancelSomePendingThreadImpl e . pendingThread) oldChildrenQueue
    writeIORef childrenQueue HM.empty
    writeIORef children HM.empty
    readIORef running
  mapM_
    ( \(Thread threadAsync cancellable) -> do
        takeMVar cancellable
        Async.cancelWith threadAsync $ CancellingException e
        putMVar cancellable ()
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
      case PSQ.lookup threadId oldQueue of
        Just (prio, SomePendingThread (PendingThread {..} :: PendingThread a1)) -> do
          case eqT @a @a1 of
            Just Refl -> do
              writeIORef queue $
                snd $
                  PSQ.alter
                    ( const
                        ( 0,
                          Just
                            ( prio,
                              SomePendingThread $
                                PendingThread {threadAction = newAction, ..}
                            )
                        )
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
