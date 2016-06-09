module FCA.Benchmark where

import FCA.Helpers
import Import

import Control.Monad
-- import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Archive.Zip as Zip
import qualified Text.XML.Cursor as Cursor
import Presenter.StarExec.Space
import Presenter.DOI
import Presenter.PersistHelper
import System.FilePath
import Presenter.StarExec.Commands
import Data.List (isSuffixOf, stripPrefix)
import Data.Text (pack, isInfixOf)

-- module to load benchmark information from latest tpdb archive into the database

-- perhabs is that the source of strictness:
insertLatestBenchmarks = insertBenchmarkInfoFromTPDB "TPDB-4d76dd84fd49.zip"

existsBenchmarkInDB :: Int -> Handler Bool
existsBenchmarkInDB bid = do
  record <- getPersistBenchmarkInfo $ StarExecBenchmarkID bid
  case record of
    Nothing -> return True
    Just _  -> return False

createBenchmarkInfo :: Int -> Text -> Text -> Int -> BenchmarkInfo
createBenchmarkInfo bid name type' numberOfRules = BenchmarkInfo bid name type' numberOfRules defaultDate


getNumberOfRules :: Cursor.Cursor -> Int
getNumberOfRules cursor = length $ Cursor.child cursor >>= Cursor.element "trs" >>= Cursor.child
                         >>= Cursor.element "rules" >>= Cursor.child >>= Cursor.element "rule"

getNumberOfVariables :: Cursor.Cursor -> Int
getNumberOfVariables cursor = length $ Cursor.child cursor >>= Cursor.element "trs" >>= Cursor.child
                        >>= Cursor.element "var" -- >>= Cursor.child >>= Cursor.element "rule"


insertBenchmarkInfoFromTPDB :: FilePath -> HandlerT App IO ()
insertBenchmarkInfoFromTPDB fPath = do
  Just sp <- liftIO $ getDefaultSpaceXML "TPDB-10.3_XML.zip"

  let mm = invertMap $ spaceToNames sp
  bs <- liftIO $ BSL.readFile fPath
  let benchmarkInfos = fmap
                  (\e -> do
                    let path = fromJust $ stripPrefix "TPDB-4d76dd84fd49/" $ Zip.eRelativePath e
                    let content = Zip.fromEntry e
                    let benchmarkId = fromJust $ M.lookup (pack path) mm
                    let cursor = cursorFromDOM content
                    (pack $ takeFileName path, path, benchmarkId, getNumberOfRules cursor, getNumberOfVariables cursor)) $
                  filter (\ e -> isSuffixOf ".xml" $ Zip.eRelativePath e) $ Zip.zEntries $ Zip.toArchive bs
  -- remove this restriction later
  -- let srsEntries = filter (\(_,path,_,_,_) -> "SRS_Relative" `isInfixOf` (pack path)) benchmarkInfos
  let benchmarkInstances = fmap (\(name,_ ,StarExecBenchmarkID bid ,numberOfRules ,_ ) -> createBenchmarkInfo bid name "" numberOfRules) $ take 1  benchmarkInfos

  nonExistingBenchmarks <- filterM (\b -> existsBenchmarkInDB $ benchmarkInfoStarExecId b) benchmarkInstances
  runDB $ insertMany_ nonExistingBenchmarks
