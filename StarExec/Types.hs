{-# LANGUAGE DeriveGeneric #-}

module StarExec.Types where

import Prelude
import Yesod
import qualified Data.Text as T
import Data.Time.Clock
import Text.Blaze
import Text.Blaze.Internal
import Network.HTTP.Conduit
import GHC.Generics

type Email = T.Text
type Password = T.Text
type Name = T.Text
type Description = T.Text
type Rank = Int
type Score = Int
type Seconds = Double
type PostProcId = Int

data Login = Login Email Password deriving (Show, Read, Eq)

data SessionData = SessionData
  { cookieData :: CookieJar
  , date :: UTCTime
  } deriving (Show, Read)

{-
-}
type Cookies = [Cookie]

{-
-}
type StarExecConnection = (Request, Manager, CookieJar)

{-
-}
data JobStatus = Complete | Incomplete | Started
    deriving (Show, Read, Eq)
derivePersistField "JobStatus"

instance ToMarkup JobStatus where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show

{-
-}
--data StarExecPrimType = Solver | Benchmark | Job | User | Space
--    deriving (Eq, Show, Read)

{-
-}
data StarExecListType = Solvers | Benchmarks | Jobs | Users | Spaces
    deriving (Eq, Show, Read)

getColumns :: StarExecListType -> Int
getColumns Solvers = 2
getColumns Benchmarks = 2
getColumns Users = 3
getColumns Jobs = 6
getColumns Spaces = 2

{-
aaData: [[,…], [,…]]
iTotalDisplayRecords: 2
iTotalRecords: 2
sEcho: 1
-}
data ListPrimResult = ListPrimResult
  { aaData :: ![[T.Text]]
  , iTotalDisplayRecords :: Int
  , iTotalRecords :: Int
  , sEcho :: Int
  } deriving (Show, Generic)

instance FromJSON ListPrimResult

data SpaceInfo = SpaceInfo
  { spaceId :: Int
  , spaceParentId :: Maybe Int
  , spaceName :: Name
  , spaceDescription :: Description
  } deriving (Show, Eq)

-- | this is (some of) the data in the xml file returned by download-space-XML
-- (for the moment, only benchmarks, ignoring permissions and solvers)
data Space = Space 
   { spId :: Int
   , spName :: Name
   , children :: [Space]
   , benchmarks :: [ Int ]
   } deriving ( Show, Eq )

families :: Space -> [ (Name, [Int]) ]
families s = 
    let path ns = T.intercalate "/" ns
        walk s = 
            let here = benchmarks s
                below = children s >>= walk 
            in    map ( \ (p,b) -> (spName s : p, b) )
                $ if Prelude.null here then below else ([], here) : below
    in  map ( \ (p,b) -> (path p, b) ) $ walk s

all_in_hierarchy :: Space -> [Int]
all_in_hierarchy s =
    benchmarks s ++ (children s >>= all_in_hierarchy)


-- | this is for managing registrations (which are in the source) FIXME
data Year = Y2014 | Y2015 | E
  deriving (Show, Eq, Read)

instance PathPiece Year where
  toPathPiece year = T.pack $ show year
  fromPathPiece t = case reads (T.unpack t) of
    [(y, "")] -> return y
    _ -> Nothing


{-
-}
data SolverResult = YES (Maybe Int) | NO | CERTIFIED | MAYBE | ERROR | OTHER
   deriving (Show, Read, Eq)
derivePersistField "SolverResult"

instance ToMarkup SolverResult where
   toMarkup = string . show
   preEscapedToMarkup = preEscapedString . show

{-
-}
data JobResultStatus = JobResultComplete | JobResultRunning | JobResultEnqueued | JobResultPending | JobResultUndetermined
  deriving (Show, Read, Eq)
derivePersistField "JobResultStatus"

{-
-}
data ErrorID = LoginError | Unkown
    deriving (Eq, Show, Read)

instance PathPiece ErrorID where
    toPathPiece = toPathPiece . show
    fromPathPiece e = do
        err <- fromPathPiece e
        return $ read err

{-
-}
newtype JobIds = JobIds [Int]
  deriving (Show, Eq, Read)

getids :: JobIds -> [Int]
getids (JobIds ids) = ids

instance PathMultiPiece JobIds where
  toPathMultiPiece (JobIds ints) = toPathMultiPiece $ map show $ ints
  fromPathMultiPiece (i:is) = do
    int <- fromPathPiece i
    (JobIds ints) <- case is of
                          [] -> return $ JobIds []
                          _ -> fromPathMultiPiece is
    return $ JobIds (int:ints)
  fromPathMultiPiece _ = Nothing

data Scoring = Standard | Complexity
  deriving (Show, Read, Eq)

{-
  solver sorted by YES/CERTIFIED/NO, maybe with scoring -> SolverResult
-}
data Category = Category
  { getCategoryName :: Name
  , getCategoryScoring :: Scoring
  , getPostProcId :: PostProcId
  , getJobIds :: [Int]
  } deriving (Show, Read, Eq)

--getCategoryName :: Category -> Name
--getCategoryName (Category name _ _ _) = name

--getCategoryScoring :: Category -> Scoring
--getCategoryScoring (Category _ scoring _ _) = scoring

--getPostProcId :: Category -> PostProcId
--getPostProcId (Category _ _ pid _) = pid

--getJobIds :: Category -> [Int]
--getJobIds (Category _ _ _ jis) = jis

{-
  solver by rank in the categories
-}
data MetaCategory = MetaCategory
  { getMetaCategoryName :: Name
  , getCategories :: [Category]
  } deriving (Show, Read, Eq)
derivePersistField "MetaCategory"

--getMetaCategoryName :: MetaCategory -> Name
--getMetaCategoryName (MetaCategory name _) = name

--getCategories :: MetaCategory -> [Category]
--getCategories (MetaCategory _ cs) = cs

data CompetitionMeta = CompetitionMeta
  { getMetaName :: Name
  , getMetaDescription :: Description
  } deriving (Eq, Ord, Read, Show)

{-
-}
data Competition = Competition
  { getMetaData :: CompetitionMeta
  , getMetaCategories :: [MetaCategory]
  } deriving (Show, Read, Eq)
derivePersistField "Competition"

getCompetitionName :: Competition -> Name
getCompetitionName = getMetaName . getMetaData

getCompetitionDescription :: Competition -> Description
getCompetitionDescription = getMetaDescription . getMetaData

--getMetaCategories :: Competition -> [MetaCategory]
--getMetaCategories (Competition _ ms) = ms

instance PathPiece Competition where
  toPathPiece comp = T.pack $ show comp
  fromPathPiece t = case reads (T.unpack t) of
      [(c, "")] -> return c
      _ -> Nothing

{-
  data-type for concurrent work
-}
data QueryStatus k = Pending (Key k) | Latest
--  deriving (Eq, Show)

data QueryResult k a = QueryResult
  { queryStatus :: QueryStatus k
  , queryResult :: a
  }

data SEQuery =
  GetJobInfo Int
  | GetSolverInfo Int
  | GetBenchmarkInfo Int
  | GetJobPair Int
  | GetJobResults Int
  | GetJob Int
  | GetPostProc Int
  deriving (Eq, Read, Show)
derivePersistField "SEQuery"

fromDiffTime :: NominalDiffTime -> Seconds
fromDiffTime = fromRational . toRational

diffTime :: UTCTime -> UTCTime -> Seconds
diffTime t1 t2 = fromDiffTime $ diffUTCTime t1 t2
