module FCA.Helpers where

import Import

import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text as T (append, pack, replace, unpack)
import Text.Regex.TDFA

-- https://github.com/nh2/haskell-ordnub#dont-use-nub
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- remove dash from given text
removeDash :: Text -> Text
removeDash = T.replace "-" ""

-- prefix given text with dash
dashPrefix :: Text -> Text
dashPrefix t = (T.pack "-") `T.append` t

-- trim after letters to retain solver basename
getSolverBasename :: Text -> Text
getSolverBasename = T.pack . matchBasename . T.unpack . removeDash

-- match basename from given solvername
matchBasename :: String -> String
matchBasename n = (n =~ ("[a-zA-Z]*" :: String) :: String)

maybeListId :: Maybe [a] -> [a]
maybeListId = fromMaybe []

safeGetIndex :: [a] -> Int -> Maybe a
safeGetIndex xs k = case drop k xs of
                    x:_ -> Just x
                    [] -> Nothing

invertMap :: (Ord a, Ord b) => Map a b -> Map b a
invertMap m = M.fromList . map (\(k,v) -> (v,k)) $ M.assocs m

zipWith8 :: (a->b->c->d->e->f->g->h->i) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (i:is)
                   =  z a b c d e f g i : zipWith8 z as bs cs ds es fs gs is
zipWith8 _ _ _ _ _ _ _ _ _ = []
