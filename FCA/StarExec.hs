module FCA.StarExec where

import FCA.Basic hiding (concepts)
import FCA.Helpers
import Import
import Presenter.Model.Entities()
import Presenter.PersistHelper

import Control.Monad (guard, unless)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List hiding (isPrefixOf, stripPrefix)
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (append, isPrefixOf, null, pack, stripPrefix, take)

data Attribute =
  ASolverBasename !Text
   | AJobResultInfoSolver !Text
   | AYearSpecificSolverName !Text
   | AJobResultInfoConfiguration !Text
   | ASlowCpuTime !Bool
   | ASolverResult !SolverResult
   | ABenchmarkNumberRules !(Maybe Bool)
  deriving (Eq, Ord, Show)


  -- constant: all job pairs with a response cpu time greater 10 seconds is slow
slowCpuTimeLimit :: (Num Double, Ord Double) => Double
slowCpuTimeLimit = 10

--
fewRulesLimit :: (Num Double, Ord Double) => Double
fewRulesLimit = 10

-- evaluate whether time are slow or not
evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = fmap ((> slowCpuTimeLimit). jobResultInfoCpuTime)

-- evaluate whether number of rules are few or not
evaluateNumberOfRules :: [Maybe Benchmark] -> [Maybe Bool]
evaluateNumberOfRules bms = do
  -- let limit = lowNumberOfRulesLimit bms
  let limit = fewRulesLimit
  fmap (\b ->
      case b of
        Nothing -> Nothing
        Just (StarExecBenchmark bb) -> Just (fromIntegral (benchmarkInfoNumberRules bb) < limit))
        bms

  -- get attribute pairs of given job results
attributePairs :: [[JobResult]] -> Handler [(JobPairID, [Attribute])]
attributePairs jobResults = do
  let starExecResults = fmap getStarExecResults jobResults
  let jobIds = fmap (StarExecJobID . jobResultInfoJobId . head) starExecResults
  competitionYears <- mapM getCompetitionYear jobIds
  benchmarks <- mapM getJobResultBenchmarks starExecResults
  let numberOfRules = map evaluateNumberOfRules benchmarks
  return . concatMap (\(jr, numberOfRules', year) -> collectData (getStarExecResults jr) numberOfRules' year) $ zip3 jobResults numberOfRules competitionYears

-- calculate all possible attribute combination of given attributes
-- Only attribute combination of different attribute constructor are allowed!
attributeGroupCombinations :: [[Attribute]] -> [[Attribute]]
attributeGroupCombinations = foldr (\a b -> (:) <$> a <*> b) [[]]

-- filter all attribute pairs by given attribute groups
filterPairsByAttributes :: [(JobPairID, [Attribute])] -> [[Attribute]] -> Maybe [(JobPairID, [Attribute])]
filterPairsByAttributes pairs attrCombinations = do
  let anyMember = or . (\s -> map (\v -> Set.isSubsetOf (Set.fromList v) s) attrCombinations)
  let filteredJobResults = filter (\(_,attrs) -> anyMember $ Set.fromList attrs) pairs
  case filteredJobResults of
    [] -> Nothing
    _ -> Just filteredJobResults

-- filter all attribute pairs by given set of objects
filterPairsByObjects :: [(JobPairID, [Attribute])] -> Set JobPairID -> [(JobPairID, [Attribute])]
filterPairsByObjects pairs objs = filter (\(obj,_) -> Set.notMember obj objs) pairs

-- unite all attributes of given jobpair attributes
uniteJobPairAttributes :: [(JobPairID, [Attribute])] -> Set Attribute
uniteJobPairAttributes pairs = Set.fromList $ concatMap snd pairs

-- create relation of JobPairID and declared attributes of given data
collectData :: [JobResultInfo] -> [Maybe Bool] -> Text -> [(JobPairID, [Attribute])]
collectData results numOfRules year = zip (fmap (StarExecPairID . jobResultInfoPairId) results) (getAttributeCollection results numOfRules year)

-- get all benchmarks from job results
getJobResultBenchmarks :: [JobResultInfo] -> Handler [Maybe Benchmark]
getJobResultBenchmarks jobResults = do
  benchmarks <- mapM (\jr -> getPersistBenchmarkInfo $ StarExecBenchmarkID $ jobResultInfoBenchmarkId jr) jobResults
  return benchmarks

-- create collection of selected attributes of given data
getAttributeCollection :: [JobResultInfo] -> [Maybe Bool] -> Text -> [[Attribute]]
getAttributeCollection jobResults lowRules year = do
  let solverBasenames = fmap (getSolverBasename . jobResultInfoSolver) jobResults
  let yearSpecificSolverNames = fmap (`T.append` year) solverBasenames
  let jobResultInfoSolvers = fmap jobResultInfoSolver jobResults
  let jobResultInfoConfigurations = fmap
                                    (\(jr,name) -> name `append` (dashPrefix $ jobResultInfoConfiguration jr)) $
                                    zip jobResults yearSpecificSolverNames
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = fmap jobResultInfoResult jobResults
  zipWith7
    (\a b c d e f g -> [
      AJobResultInfoSolver a,
      ASolverBasename b,
      AYearSpecificSolverName c,
      AJobResultInfoConfiguration d,
      ASlowCpuTime e,
      ASolverResult f,
      ABenchmarkNumberRules g
    ])
    jobResultInfoSolvers solverBasenames yearSpecificSolverNames jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults lowRules


-- proper names for attributes in template
properAttrName :: Attribute -> Text
properAttrName at = case at of
 (ASlowCpuTime slow)                  -> if slow then "time > 10s" else "time <= 10s"
 (AJobResultInfoSolver name)          -> name
 (ASolverBasename name)               -> name
 (AYearSpecificSolverName name)       -> name
 (AJobResultInfoConfiguration config) -> config
 (ABenchmarkNumberRules lowRules)       -> case lowRules of
                                          Nothing   -> "No Rules"
                                          Just low  -> if low then "few" else "many"
 (ASolverResult result) -> case result of
                            YES           -> "YES"
                            NO            -> "NO"
                            MAYBE         -> "MAYBE"
                            (BOUNDS b)    -> T.append "BOUNDS " . T.pack $ show b
                            CERTIFIED     -> "CERTIFIED"
                            ERROR         -> "ERROR"
                            (OTHER text)  -> T.append "OTHER " text
stripAttributePrefixes :: Text -> Text
stripAttributePrefixes at
  | "Result " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Result " at
  | "Solver config " `isPrefixOf` at = fromJust $ T.stripPrefix "Solver config " at
  | "Solver basename " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Solver basename " at
  | "SolverYearName " `T.isPrefixOf` at = fromJust $ T.stripPrefix "SolverYearName " at
  | "Solver name " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Solver name " at
  | "CPU " `T.isPrefixOf` at = fromJust $ T.stripPrefix "CPU " at
  | otherwise = at


-- create all attribute combinations from existing attributes without duplicates
attributeCombination :: (Ord at) => Context ob at -> [Set at]
attributeCombination context = do
  let attrs = Map.elems $ fore context
  -- using ordNub to reduce duplicate items and keep order
  ordNub . fmap Set.fromList . concatMap (subsequences . Set.toList) $ ordNub attrs

-- determine all concepts of given context with StarExec attributes
concepts :: (Ord at, Ord ob, Show ob, Show at) => Context ob at -> [Concept ob at]
concepts c = do
  attrs <- attributeCombination c
  let objs = getObjects c attrs
  unless (Set.null objs) . guard $ (attrs == (getAttributes c) objs)
  return (Concept objs attrs)

-- get competition year from JobID
getCompetitionYear :: JobID -> Handler Text
getCompetitionYear jid = do
  jobInfo <- getPersistStarExecJobInfo $ getStarExecId jid
  let year = T.take 4 . jobInfoDate $ fromJust jobInfo
  if T.null year
    then return $ year
    else return $ dashPrefix year

isASolverResult :: Attribute -> Bool
isASolverResult at = case at of
  ASolverResult _ -> True
  _               -> False

isABenchmarkNumberRules :: Attribute -> Bool
isABenchmarkNumberRules at = case at of
  ABenchmarkNumberRules _ -> True
  _                       -> False

isASlowCpuTime :: Attribute -> Bool
isASlowCpuTime at = case at of
  ASlowCpuTime _  -> True
  _               -> False

isAJobResultInfoConfiguration :: Attribute -> Bool
isAJobResultInfoConfiguration at = case at of
  AJobResultInfoConfiguration _  -> True
  _                              -> False

isAYearSpecificSolverName :: Attribute -> Bool
isAYearSpecificSolverName at = case at of
  AYearSpecificSolverName _  -> True
  _                          -> False

-- recursive function to calculate concepts list and reduce attribute pairs
-- by objects of concept at first index of complement list.
-- Each complement list element refers to the index of the current concepts list
reducePairsByComplements :: [(JobPairID, [Attribute])] -> ComplementIds -> [(JobPairID, [Attribute])]
reducePairsByComplements pairs (Ids complIds) = case complIds of
  []   -> pairs
  compl:compls -> case pairs of
    []        -> []
    _ -> do
      let concept = safeGetIndex (concepts $ contextFromList pairs) compl
      case concept of
        Nothing -> reducePairsByComplements pairs $ Ids compls
        Just c  -> reducePairsByComplements (filterPairsByObjects pairs $ obs c) $ Ids compls

-- get index of concept in concept list
conceptElemIndex :: (Eq at, Eq ob) => Concept ob at -> Maybe [Concept ob at] -> Int
conceptElemIndex concept conceptLattice = fromJust . elemIndex concept $ maybeListId conceptLattice

-- get objects of a concept in concept lattice from index
safeGetConceptObjectsFromLattice :: [Concept ob at] -> Int -> Set ob
safeGetConceptObjectsFromLattice concept k = do
  let concept' = safeGetIndex concept k
  case concept' of
    Nothing -> Set.empty
    Just c  -> obs c
