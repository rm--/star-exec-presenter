module StarExec.STM
  ( startWorker
  , lookupCache
  ) where

import Import

import StarExec.Types
import StarExec.CompetitionResults
import StarExec.CompetitionResults.Type

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Concurrent ( threadDelay )
import Control.Exception.Base
import Control.Monad ( forever, when )

defaultWorkerDelay :: Int
defaultWorkerDelay = 10 * 10^6

lookupCache :: Competition -> Handler (Maybe CompetitionResults)
lookupCache comp = do
  app <- getYesod
  (mCompResults, start) <- lift $ atomically $ do
    crc <- readTVar $ compResultsCache app
    case M.lookup (getMetaData comp) crc of
        Nothing -> do
            return (Nothing, True)
        Just entry -> do
            mCompResults <- readTVar entry
            return (mCompResults, False)
  when start $ startWorker comp
  return mCompResults

startWorker :: Competition -> Handler ()
startWorker comp = do
  app <- getYesod
  mSink <- lift $ atomically $ do
    crc <- readTVar $ compResultsCache app
    case M.lookup (getMetaData comp) crc of
      Nothing -> do
          sink <- newTVar Nothing
          modifyTVar' (compResultsCache app) $ M.insert (getMetaData comp) sink
          return $ Just sink
      Just entry -> do
          return Nothing
  case mSink of
    Nothing -> return ()
    Just sink -> do
      lift $ putStrLn $ "start worker for " ++ ( T.unpack $ getCompetitionName comp )
      forkHandler exceptionHandler runWorker
      where
        runWorker = do
          compResults <- getCompetitionResults comp
          lift $ atomically $ writeTVar sink $ Just compResults
          lift $ threadDelay defaultWorkerDelay
          when (not $ competitionComplete compResults) runWorker

exceptionHandler :: SomeException -> Handler ()
exceptionHandler e = lift $ do
  putStrLn $ "ignoring exception:"
  putStrLn $ show e
