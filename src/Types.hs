{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types where

import           Brick.Forms   (Form)
import           Lens.Micro.TH
import           RIO


-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data Haradict = Haradict
  { haradictQuery       :: !(Maybe Text)
  , haradictQueryForm   :: !(Form Text () String)

  , haradictResults     :: ![(Int, [Int])]
  -- ^ The ElixirFM 'Clips' value.
  , haradictResultIndex :: !Int
  -- ^ idx `elem` [0, length results)

  , haradictException   :: !(Maybe HaradictException)
  , haradictViewState   :: !HaradictView
  }

data HaradictView =
  Query
  | Results
  | ErrorView

data HaradictException =
  EmptyQuery
  | EmptyResults
  | NotArabicQuery
  | UnknownError


data WordInfo = WordInfo
  { wordInfoRoot :: String
  , wordInfoWord :: String
  , wordInfoTags :: String
  , wordInfoMorphs :: String
  , wordInfoMeanings :: [String]
  } deriving Show

makeFields ''Haradict
makeFields ''Options
makeFields ''WordInfo
