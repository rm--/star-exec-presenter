module Handler.Concepts where

import FCA.Basic hiding (concepts)
import qualified FCA.Basic as FCA
import FCA.StarExec
import FCA.DotGraph (renderConceptSVG)
import FCA.Helpers
import Import
import Presenter.StarExec.JobData
import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import Presenter.Utils.WidgetTable

import Control.Monad
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
import Yesod.Form.Bootstrap3
import Data.List (sortOn)


data AttributeChoices = AttributeChoices
  { chosenSolver :: [Attribute]
  , chosenResults :: Maybe [Attribute]
  , chosenCpu :: Maybe [Attribute]
  , chosenConfig :: Maybe [Attribute]
  , chosenRules :: Maybe [Attribute]
  , chosenLeftlinears :: Maybe [Attribute]
  }
  deriving (Eq, Show)


-- route with multiselect to choose attributes of JobID
getConceptsR :: ConceptId -> ComplementIds -> JobIds -> Handler Html
getConceptsR cid compls@(Ids complIds) jids@(JobIds ids) = do
  qJobs <- queryManyJobs ids
  attributePairs' <- attributePairs $ fmap (snd . queryResult) qJobs
  ((result, widget), enctype) <- ((runFormGet . renderBootstrap3 BootstrapBasicForm) . attributeForm ) .
    attrOptions $ uniteJobPairAttributes attributePairs'
  let mc = case result of
        FormMissing    -> Nothing
        FormFailure _  -> Nothing
        FormSuccess ca -> do
          let chosenAttributes = filter (not . null) .
                                   (++) [chosenSolver ca] $
                                   map (\f -> maybeListId .f $ ca) [chosenResults, chosenCpu, chosenConfig, chosenRules, chosenLeftlinears]
          case filterPairsByAttributes attributePairs' $ attributeGroupCombinations chosenAttributes of
            Nothing -> Nothing
            Just pairs ->
              let ctx = contextFromList . reducePairsByComplements pairs $ Ids complIds
                  lat = FCA.lattice $ FCA.einpack ctx
              in  Just ( map (auspack . fst) lat , FCA.implications lat )

  let concepts' :: Maybe [Concept JobPairID Attribute]
      concepts' = fmap fst mc
      implications = fmap snd mc
  let reducedConcepts = reduceConceptsToProperSubsets concepts' cid

  nodeURLs <- mapM
             (\c -> getConceptURL (conceptElemIndex c concepts') compls ids) $
             maybeListId reducedConcepts
  complURLs <- mapM
              (\c -> getConceptURL cid (Ids (complIds `mappend` [conceptElemIndex c concepts'])) ids) $
              maybeListId reducedConcepts

  svgContent <- renderConceptSVG (maybeListId reducedConcepts) nodeURLs complURLs

  let currObjects = maybe
                    Set.empty
                    (\ c -> safeGetConceptObjectsFromLattice c cid)
                    concepts'

  let filteredJobResults = fmap
                          ((wrapResults .
                            filter
                            (\jr -> Set.member (getPairID jr) currObjects)
                            . getStarExecResults)
                            . snd . queryResult)
                          qJobs
  tab <- getManyJobCells filteredJobResults

  --actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL 0 compls ids
  currURL <- getConceptURL cid compls ids
  resetComplURL <- getConceptURL 0 (Ids []) ids
  defaultLayout $ do
    when (any (\q' -> queryStatus q' /= Latest) qJobs ) insertWidgetMetaRefresh
    toWidget $(luciusFile "templates/solver_result.lucius")
    toWidget $(juliusFile "templates/concepts.julius")
    setTitle "concepts"
    -- case implications of
    --   Nothing -> return ()
    --   Just imps -> do
    --     let fst3 (x,y,z) = x
    --         att_size conc = Set.size $ FCA.ats $ auspack conc
    --         obj_size conc = Set.size $ FCA.obs $ auspack conc
    --         imps' = sortOn (att_size . fst3) $ filter ((>0) . att_size . fst3 ) imps
    --     [whamlet|$forall (conc,at,atts) <- imps'
    --          <pre>
    --             for #{show $ obj_size conc} objects with attributes #{show $ FCA.ats $ auspack conc},
    --             attribute #{show at}
    --             implies attributes #{show atts}
    --     |]
    $(widgetFile "concepts")
    unless (null currObjects) $ displayConcept jids tab


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions = AttributeChoices
  <$> areq (multiSelectFieldList . fromJust $ M.lookup "SolverYearName" formOptions) (bfsFormControl MsgSolverNames "SolverNames") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Solver config" formOptions) (bfsFormControl MsgSolverConfigs "SolverConfigs") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Result" formOptions) (bfsFormControl MsgResults "Results") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "CPU" formOptions) (bfsFormControl MsgCPUTimes "CPUTimes") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Number Rules" formOptions) (bfsFormControl MsgNumberRules "NumberRules") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Left Linears" formOptions) (bfsFormControl MsgLeftLinears "LeftLinears") Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary center-block",
      bsValue="choose",
      bsAttrs=[]} :: BootstrapSubmit Text)

bfsFormControl :: RenderMessage master msg => msg -> Text -> FieldSettings master
bfsFormControl msg label = (bfs msg) {fsName = Just label, fsAttrs = [("class", "form-control")]}

-- create attribute form field options
attrOptions :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptions attrs = do
  M.fromList $ fmap
               (\(fieldName, atrPred) -> (fieldName, filter (\(_, atr) ->  atrPred atr) . fmap (\atr -> (properAttrName atr, atr)) $ Set.toList attrs))
               [("Result",isASolverResult)
               ,("CPU",isASlowCpuTime)
               ,("Solver config",isAJobResultInfoConfiguration)
               ,("Number Rules",isABenchmarkNumberRules)
               ,("Left Linears",isABenchmarkLeftLinear)
               ,("SolverYearName",isAYearSpecificSolverName)]

getConceptURL :: ConceptId -> ComplementIds -> [JobID] -> Handler Text
getConceptURL cid compls jids = do
  rq <- getRequest
  renderer <- getUrlRenderParams
  return . renderer (ConceptsR cid compls $ JobIds jids) $ reqGetParams rq
