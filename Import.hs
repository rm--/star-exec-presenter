module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Presenter.Model      as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

for :: Functor f =>  f a -> (a -> b) -> f b
for = flip fmap
