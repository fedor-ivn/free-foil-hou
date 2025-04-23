{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.RawConfig (
  decodeConfigFile,
)
where

import qualified Data.Map as Map
import Data.SOAS (MetaAbs (..), MetaSubst (..), MetaSubsts (..))
import qualified Data.Text as TIO
import Language.Lambda.Config (
  CanonicalConfig,
  Config (..),
  IsCanonicalConstraint (..),
  IsCanonicalMetavarBinders (..),
  IsCanonicalSubstitutions (..),
  Problem (..),
  Result,
  Solution (..),
  fromUnificationConstraint,
  toCanonicalConfig,
  toUnificationConstraint,
 )
import Language.Lambda.Impl (
  MetavarBinders,
  fromMetaTerm,
  fromTerm,
  toIdents,
  toMetaSubst,
 )
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw
import qualified Language.Lambda.Syntax.Print as Raw
import Toml (TomlCodec, (.=))
import qualified Toml

newtype RawBinders = RawBinders [String]
newtype RawSubstitutions = RawSubstitutions [String]
newtype RawConstraint = RawConstraint String

type RawConfig = Config RawBinders RawConstraint RawSolution

type RawProblem = Problem RawBinders RawConstraint RawSolution

type RawSolution = Solution RawBinders RawSubstitutions

-- TomlCodecs
configCodec :: TomlCodec RawConfig
configCodec =
  Config
    <$> Toml.list problemCodec "problems" .= configProblems

problemCodec :: TomlCodec RawProblem
problemCodec =
  (Problem . RawBinders <$> stringsCodec "metavars")
    <*> (map RawConstraint <$> stringsCodec "constraints")
    <*> Toml.list solutionCodec "solutions" .= problemSolutions

stringsCodec :: Toml.Key -> Toml.Codec object [String]
stringsCodec field =
  map TIO.unpack
    <$> Toml.arrayOf Toml._Text field .= error "no encode needed"

solutionCodec :: TomlCodec RawSolution
solutionCodec =
  Solution
    <$> Toml.dioptional (Toml.text "name") .= solutionName
    <*> (RawBinders <$> stringsCodec "metavars")
      .= solutionMetavarBinders
    <*> (RawSubstitutions <$> stringsCodec "substitutions")
      .= solutionSubstitutions

decodeConfigFile :: FilePath -> IO (Result CanonicalConfig)
decodeConfigFile =
  let convert = either (Left . show) toCanonicalConfig
   in fmap convert . Toml.decodeFileEither configCodec

instance IsCanonicalMetavarBinders RawBinders where
  toCanonicalMetavarBinders :: RawBinders -> Result MetavarBinders
  toCanonicalMetavarBinders (RawBinders rawMetavarBinders) = do
    metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
    pure (Map.fromList metavarBinders)
   where
    parseMetavarBinder input =
      let
        tokens = Raw.resolveLayout False (Raw.myLexer input)
        toMetavarBinder (Raw.AMetavarBinder metavar args returnType) =
          (metavar, (args, returnType))
       in
        fmap toMetavarBinder (Raw.pMetavarBinder tokens)
  fromCanonicalMetavarBinders metavarBinders =
    let
      toRawMetavarBinder (metavar, (args, returnType)) =
        Raw.AMetavarBinder metavar args returnType
      rawMetavarBinders = map toRawMetavarBinder (Map.toList metavarBinders)
     in
      pure $ RawBinders (fmap Raw.printTree rawMetavarBinders)

instance IsCanonicalSubstitutions RawSubstitutions where
  toCanonicalSubstitution metavarBinders (RawSubstitutions rawSubsts) = do
    substs <- traverse parse' rawSubsts
    pure (MetaSubsts substs)
   where
    parse' input = do
      let tokens = Raw.resolveLayout False (Raw.myLexer input)
      raw <- Raw.pMetaSubst tokens
      case toMetaSubst metavarBinders raw of
        Just subst -> pure subst
        Nothing -> Left "type error"

  fromCanonicalSubstitution _ (MetaSubsts substs) = do
    let rawSubsts = map (Raw.printTree . fromMetaSubst) substs
    let rawSubstitutions = map Raw.printTree rawSubsts
    pure $ RawSubstitutions rawSubstitutions
   where
    fromMetaSubst (MetaSubst (metavar, MetaAbs binderList term)) =
      let term' = Raw.AScopedTerm $ fromTerm $ fromMetaTerm term
          idents = toIdents binderList
       in Raw.AMetaSubst metavar idents term'

instance IsCanonicalConstraint RawConstraint where
  toCanonicalConstraint metavarBinders (RawConstraint input) = do
    let tokens = Raw.resolveLayout False (Raw.myLexer input)
    raw <- Raw.pUnificationConstraint tokens
    case toUnificationConstraint metavarBinders raw of
      Just uc -> Right uc
      Nothing -> Left "type error"
  fromCanonicalConstraint _ =
    Right
      . RawConstraint
      . Raw.printTree
      . fromUnificationConstraint
