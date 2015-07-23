module Handler.DbTest where

import Import
import Data.Maybe
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA


type JobPairId = Int

data Attribute = 
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
   | ASlowCpuTimeSolverResult Bool SolverResult
 deriving (Eq, Ord, Show)

data JobPairAttributes = JobPairAttributes
  { benchmarkId :: Int
  , solverName  :: Text
  , slowCpuTime :: Bool
  , solverResult :: SolverResult
  } deriving (Show)


-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10


getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  -- let objAttrRel = createObjectAttributeRelation $ getStarExecResults jobResults
  -- let attrObjRel = createAttributeObjectReleation objAttrRel
  -- let concepts = createConcepts objAttrRel attrObjRel
  let concepts' = concepts context

  defaultLayout [whamlet|
    <h1>Concepts
    <ul>
    $forall (o,a) <- concepts'
      <li> #{show o} : #{show a}

    <h1>Objects
    <ul>
    $forall k <- objects context
      <li> #{show k}
      #{show $ getAttributes context $ Set.fromList [k]}

    <h1>Attributes
    <ul>
    $forall k <- attributes context
      <li> #{show k}
      #{show $ getObjects context $ Set.fromList [k]}

    <h1>ContextData
    <ul>
    $forall obj <- contextData
      <li> #{show obj}

    <h1>Original records
    <ul>
    $forall jobResult <- getStarExecResults jobResults
      <li> #{show jobResult}
    |]

collectData :: [JobResultInfo] -> [(JobPairId, [Attribute])]
collectData results = do
  let jobResultInfoPairIds = map jobResultInfoPairId results
  let attrs = getAttributeCollection results
  zip jobResultInfoPairIds attrs


getAttributeCollection :: [JobResultInfo] -> [[Attribute]]
getAttributeCollection jobResults = do
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let jobResultInfoConfigurations = map jobResultInfoConfiguration jobResults
  -- let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  zipWith4 (\a b c d -> [AJobResultInfoSolver a, AJobResultInfoConfiguration b, ASlowCpuTime c, ASolverResult d])
    jobResultInfoSolvers jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults



createConcepts :: Map JobPairId JobPairAttributes -> Map Attribute [JobPairId] -> [([JobPairId], [Attribute])]
createConcepts objAttrRel attrObjRel = do
  let objects = (take 2000000 $ subsequences $ Map.keys objAttrRel :: [[JobPairId]])
  -- let objects = (subsequences $ Map.keys objAttrRel :: [[JobPairId]])
  let attributes = getCommonAttributes objects objAttrRel
  let calculatedObjects = getCommonObjects attributes attrObjRel
  getEqualObjects objects calculatedObjects attributes


getEqualObjects :: [[JobPairId]] -> [[JobPairId]] -> [[Attribute]] -> [([JobPairId],[Attribute])]
getEqualObjects [] _ _ = []
getEqualObjects _ [] _ = []
getEqualObjects _ _ [] = []
getEqualObjects (o:objs) (d:derivedObjs) (a:attrs)
  | o == d = (o,a) : getEqualObjects objs derivedObjs attrs
  | otherwise = getEqualObjects objs derivedObjs attrs


getCommonAttributes :: [[JobPairId]] -> Map JobPairId JobPairAttributes -> [[Attribute]]
getCommonAttributes jobPairIds objAttrRel = do
    map (\jobs ->
      getCommonAttribute $ map (\job -> fromJust $ Map.lookup job objAttrRel) jobs)
      jobPairIds


getCommonObjects :: [[Attribute]] -> Map Attribute [JobPairId] -> [[JobPairId]]
getCommonObjects attributes attrObjRel = do
    map (\attrs ->
      concat $ map (\attr -> fromJust $ Map.lookup attr attrObjRel) attrs)
      attributes

getCommonAttribute :: [JobPairAttributes] -> [Attribute]
getCommonAttribute attributes = do
    let commonAttr = (getSlowCpuTimeAttribute attributes) ++ (getSolverResultAttribute attributes)
    if elem (ASlowCpuTime True) commonAttr && elem (ASolverResult MAYBE) commonAttr
      then [ASlowCpuTimeSolverResult True MAYBE]
      else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult MAYBE) commonAttr
        then [ASlowCpuTimeSolverResult False MAYBE]
        else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult YES) commonAttr
          then [ASlowCpuTimeSolverResult False YES]
          else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult YES) commonAttr
            then [ASlowCpuTimeSolverResult False YES]
            else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult NO) commonAttr
              then [ASlowCpuTimeSolverResult False NO]
              else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult NO) commonAttr
                then [ASlowCpuTimeSolverResult False NO]
                  else commonAttr


getSlowCpuTimeAttribute :: [JobPairAttributes] -> [Attribute]
getSlowCpuTimeAttribute attributes = do
  if all (\a -> slowCpuTime a == True) attributes
    then [ASlowCpuTime True]
    else if all (\a -> slowCpuTime a == False) attributes
      then [ASlowCpuTime False]
      else []

getSolverResultAttribute :: [JobPairAttributes] -> [Attribute]
getSolverResultAttribute attributes = do
  if all (\a -> solverResult a == MAYBE) attributes
    then [ASolverResult MAYBE]
    else if all (\a -> solverResult a == YES) attributes
      then [ASolverResult YES]
      else if all (\a -> solverResult a == NO) attributes
        then [ASolverResult NO]
        else []


createAttributeObjectReleation :: Map JobPairId JobPairAttributes -> Map Attribute [JobPairId]
createAttributeObjectReleation objAttrRel = do
  let objAttrs = Map.toList objAttrRel
  -- very ugly and to explicit!
  let cpuTimes = map (\(a,b) -> (a, slowCpuTime b)) objAttrs
  let slowCpuTimes = map (\(a,_) -> a) $ filter (\(_,b) -> b == True) cpuTimes
  let fastCpuTimes = map (\(a,_) -> a) $ filter (\(_,b) -> b == False) cpuTimes
  
  let solverResults = map (\(a,b) -> (a, solverResult b)) objAttrs
  let maybeSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == MAYBE) solverResults
  let yesSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == YES) solverResults
  let noSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == NO) solverResults
  -- let otherSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == OTHER) solverResults
  let attrsObjRel = Map.insert (ASlowCpuTime False) slowCpuTimes Map.empty
  let attrsObjRel' = Map.insert (ASlowCpuTime True) fastCpuTimes attrsObjRel

  -- only helper keys so the union does not have to be calculated again and again
  let attrsObjRel'' = Map.insert (ASlowCpuTimeSolverResult True MAYBE) (listIntersect slowCpuTimes maybeSolverResults) attrsObjRel'
  let attrsObjRel''' = Map.insert (ASlowCpuTimeSolverResult False MAYBE) (listIntersect fastCpuTimes maybeSolverResults) attrsObjRel''

  let attrsObjRel4 = Map.insert (ASlowCpuTimeSolverResult True YES) (listIntersect slowCpuTimes yesSolverResults) attrsObjRel'''
  let attrsObjRel5 = Map.insert (ASlowCpuTimeSolverResult False YES) (listIntersect fastCpuTimes yesSolverResults) attrsObjRel4

  let attrsObjRel6 = Map.insert (ASlowCpuTimeSolverResult True NO) (listIntersect slowCpuTimes noSolverResults) attrsObjRel5
  let attrsObjRel7 = Map.insert (ASlowCpuTimeSolverResult False NO) (listIntersect fastCpuTimes noSolverResults) attrsObjRel6

  let attrsObjRel8 = Map.insert (ASolverResult YES) yesSolverResults attrsObjRel7
  let attrsObjRel9 = Map.insert (ASolverResult NO) noSolverResults attrsObjRel8
  Map.insert (ASolverResult MAYBE) maybeSolverResults attrsObjRel9


evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)


-- https://github.com/nh2/haskell-ordnub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
-- https://github.com/nh2/haskell-ordnub
listIntersect :: (Ord a) => [a] -> [a] -> [a]
listIntersect a b = filter (`Set.member` bSet) a
  where
    bSet = Set.fromList b

updateMap :: (Ord k, Num a) => k -> a -> Map k a -> Map k a
updateMap = Map.insertWith (+)