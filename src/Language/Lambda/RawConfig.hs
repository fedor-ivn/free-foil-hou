{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.RawConfig (
  -- * Raw config types
  RawConfig (..),
  RawProblem (..),
  RawSolution (..),

  -- * TOML parsing
  configCodec,
  problemCodec,
  solutionCodec,
  stringsCodec,

  -- * Parsing functions
  parseRawConfig,
  parseRawProblem,
  parseSolution,
  formatTomlErrors,
) where

import Data.Text (Text)
import qualified Data.Text as TIO
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import qualified Toml

import Language.Lambda.Impl (MetavarBinders)

-- Raw types (from TOML)
data RawConfig = RawConfig
  { rawConfigLanguage :: Text
  , rawConfigFragment :: Text
  , rawConfigProblems :: [RawProblem]
  }
  deriving (Show, Generic)

data RawProblem = RawProblem
  { rawProblemMetavars :: [String]
  , rawProblemConstraints :: [String]
  , rawProblemSolutions :: [RawSolution]
  }
  deriving (Show, Generic)

data RawSolution = RawSolution
  { rawSolutionName :: Text
  , rawSolutionMetavars :: Maybe [String] -- New field for solution-specific metavars
  , rawSolutionSubstitutions :: [String]
  }
  deriving (Show, Generic)

-- TomlCodecs
configCodec :: TomlCodec RawConfig
configCodec =
  RawConfig
    <$> Toml.text "language" .= rawConfigLanguage
    <*> Toml.text "fragment" .= rawConfigFragment
    <*> Toml.list problemCodec "problems" .= rawConfigProblems

problemCodec :: TomlCodec RawProblem
problemCodec =
  RawProblem
    <$> stringsCodec "metavars"
    <*> stringsCodec "constraints"
    <*> Toml.list solutionCodec "solutions" .= rawProblemSolutions

stringsCodec :: Toml.Key -> Toml.Codec object [String]
stringsCodec field =
  map TIO.unpack
    <$> Toml.arrayOf Toml._Text field .= error "no encode needed"

solutionCodec :: TomlCodec RawSolution
solutionCodec =
  RawSolution
    <$> Toml.text "name" .= rawSolutionName
    <*> Toml.dioptional (stringsCodec "metavars") .= rawSolutionMetavars -- Make it optional
    <*> stringsCodec "substitutions" .= rawSolutionSubstitutions

-- Helper function for error formatting
formatTomlErrors :: Toml.TomlDecodeError -> String
formatTomlErrors errs =
  unlines $
    "Configuration errors:" : [(\err -> "  - " ++ show err) errs]

-- | These functions need to be defined at the module that combines both
-- | raw config and processed config. Here we define their type signatures
-- | for reference.
parseRawConfig :: RawConfig -> Either String config
parseRawConfig = undefined

parseRawProblem :: RawProblem -> Either String problem
parseRawProblem = undefined

parseSolution :: MetavarBinders -> RawSolution -> Either String solution
parseSolution = undefined
