{-# language DoAndIfThenElse #-}
module FCA.Benchmark where

import FCA.Helpers
import Import hiding (get)

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Archive.Zip as Zip
import Presenter.StarExec.Space
import Presenter.DOI
import Presenter.PersistHelper
import System.FilePath
import Presenter.StarExec.Commands
import Data.List (isSuffixOf, stripPrefix)
import Data.Text (append, pack)
import TPDB.Input.Memory
import TPDB.Data
import TPDB.Data.Attributes
import Data.Either

-- perhabs is that the source of strictness:
insertLatestBenchmarks :: HandlerT App IO ()
insertLatestBenchmarks = insertBenchmarkInfoFromTPDB "TPDB-4d76dd84fd49.zip"

-- check database if benchmark  with given id exists
doesNotExistsBenchmarkInDB :: Int -> Handler Bool
doesNotExistsBenchmarkInDB bid = do
  record <- getPersistBenchmarkInfo $ StarExecBenchmarkID bid
  case record of
    Nothing -> return True
    Just _  -> return False

-- create benchmarkinfo instances from given data
createBenchmarkInfo :: Int -> Text -> Text -> Int -> Maybe Bool -> BenchmarkInfo
createBenchmarkInfo bid bName type' numberOfRules leftLinear = BenchmarkInfo bid bName type' numberOfRules leftLinear defaultDate

-- insert benchmark instances from tpdb zip
-- including new attributes for formal concept analysis: number of rules and left linear
insertBenchmarkInfoFromTPDB :: FilePath -> HandlerT App IO ()
insertBenchmarkInfoFromTPDB fPath = do
  Just sp <- liftIO $ getDefaultSpaceXML "TPDB-10.3_XML.zip"
  let benchmarkPathBidRel = invertMap $ spaceToNames sp
  bs <- liftIO $ BSL.readFile fPath
  let xmlEntries = fmap (\e -> do
                               let path = fromJust $ stripPrefix "TPDB-4d76dd84fd49/" $ Zip.eRelativePath e
                               let content = Zip.fromEntry e
                               let benchmarkId = fromJust $ M.lookup (pack path) benchmarkPathBidRel
                               (takeFileName path, path, content, benchmarkId)
                             ) $
                             filter (\ e -> isSuffixOf ".xml" $ Zip.eRelativePath e) $ Zip.zEntries $ Zip.toArchive bs

  nonExistingBenchmarks <- filterM (\(_, _, _, StarExecBenchmarkID bid) -> doesNotExistsBenchmarkInDB $ bid) xmlEntries
  if (not (null nonExistingBenchmarks))
    then do
      $(logInfo) $ pack "Benchmark import started. That can take some minutes."
      tpdbInstances <- mapM liftIO $ map (\(fn, _, content, _) -> get fn content) nonExistingBenchmarks
      -- FIXME: add doi handling
      -- only for trs instances atm
      let trsRules = fmap (rules) . lefts $ rights tpdbInstances
      let numberOfRulesAttrs = fmap length trsRules
      let leftLinearAttrs = fmap (left_linear . compute_attributes) trsRules
      let benchmarkData = zipWith3 (\(t1,t2,t3,t4) r l -> (t1,t2,t3,t4,r,l)) nonExistingBenchmarks numberOfRulesAttrs leftLinearAttrs
      let benchmarkInstances = fmap
                               (\(fn, _, _ , StarExecBenchmarkID bid, numberOfRules, leftLinear)  -> createBenchmarkInfo bid (pack fn) "" numberOfRules (Just leftLinear)) $
                               benchmarkData
      runDB $ insertMany_ benchmarkInstances
      $(logInfo) $ append (pack . show $ length benchmarkInstances) " benchmarks imported."
  else $(logInfo) $ pack "Nothing to import."
