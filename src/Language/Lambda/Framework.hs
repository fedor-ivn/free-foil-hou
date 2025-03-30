{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Framework for testing and validating pattern matching and unification.
module Language.Lambda.Framework (
  -- * Types
  RawConfig (..),
  RawProblem (..),
  RawSolution (..),
  ValidationError (..),

  -- * Parsing
  parseMetavarBinders,
  matchUnificationConstraint,
  isSolvedUnificationConstraint,
  parseRawProblem,
  parseRawConfig,

  -- * Validation
  validateProblem,
  validateSolution,
  solveUnificationConstraint,
  isSolved,

  -- * TOML configuration
  configCodec,
  problemCodec,
  solutionCodec,
  stringsCodec,
  parseConfigAndValidate,
  printInvalidSolutionsWithConstraint,
  formatTomlErrors,

  -- * Main entry point
  main,
  IsCanonicalTerm (..),
) where

import Control.Monad (forM_)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.SOAS
import Data.Text (Text)
import qualified Data.Text as TIO
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import qualified Toml

import qualified Control.Monad.Foil.Internal as Foil
import Control.Monad.Free.Foil (alphaEquiv)
import Language.Lambda.Config (
  Config (..),
  Problem (..),
  Solution (..),
  UnificationConstraint (..),
  toUnificationConstraint,
 )
import Language.Lambda.Impl (
  MetaSubst',
  MetaSubsts',
  MetaTerm,
  MetavarBinder,
  MetavarBinders,
  matchMetaAbs,
  nfMetaTerm,
  toMetaSubst,
 )
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw

-- ** Test framework implementation

type CanonicalTerm n = MetaTerm Raw.MetavarIdent n Raw.Type

class IsCanonicalTerm a where
  toTerm :: a -> CanonicalTerm n
  fromTerm :: CanonicalTerm n -> a

-- ** Parsing functions
parseMetavarBinder :: String -> Either String MetavarBinder
parseMetavarBinder input = fmap toMetavarBinder (Raw.pMetavarBinder tokens)
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)
  toMetavarBinder (Raw.AMetavarBinder metavar args returnType) =
    (metavar, (args, returnType))

-- | Parse a list of metavariable binder strings
parseMetavarBinders :: [String] -> Either String MetavarBinders
parseMetavarBinders rawMetavarBinders = do
  metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
  pure $ Map.fromList metavarBinders

parseMetaSubst :: MetavarBinders -> String -> Either String MetaSubst'
parseMetaSubst metavarBinders input = do
  let tokens = Raw.resolveLayout False (Raw.myLexer input)
  raw <- Raw.pMetaSubst tokens
  case toMetaSubst metavarBinders raw of
    Just subst -> pure subst
    Nothing -> trace "here" $ Left "type error"

parseUnificationConstraint :: MetavarBinders -> String -> Either String UnificationConstraint
parseUnificationConstraint metavarBinders input = do
  let tokens = Raw.resolveLayout False (Raw.myLexer input)
  raw <- Raw.pUnificationConstraint tokens
  case toUnificationConstraint metavarBinders raw of
    Just uc -> trace (show uc) $ pure uc
    Nothing -> Left "type error"

-- ** Matching and solving functions

-- | Match a unification constraint with given metavariable binders
matchUnificationConstraint
  :: [String]
  -> String
  -> Either String [MetaSubsts']
matchUnificationConstraint
  rawMetavarBinders
  rawUnificationConstraint = do
    metavarBindersMap <- parseMetavarBinders rawMetavarBinders
    (UnificationConstraint binders binderTypes lhs rhs) <-
      parseUnificationConstraint
        metavarBindersMap
        rawUnificationConstraint
    let scope = nameBinderListToScope binders
        substs = match scope metavarBindersMap binderTypes lhs rhs
    pure substs

-- | Apply substitutions to both sides of a unification constraint
solveUnificationConstraint
  :: MetaSubsts'
  -> UnificationConstraint
  -> UnificationConstraint
solveUnificationConstraint
  substs
  (UnificationConstraint binders binderTypes lhs rhs) =
    let scope = nameBinderListToScope binders
        solve = nfMetaTerm scope . applyMetaSubsts scope substs
     in UnificationConstraint
          binders
          binderTypes
          (solve lhs)
          (solve rhs)

nameBinderListToScope
  :: (Foil.Distinct n)
  => Foil.NameBinderList Foil.VoidS n
  -> Foil.Scope n
nameBinderListToScope = extendScopeWithNameBinderList Foil.emptyScope

extendScopeWithNameBinderList
  :: ( Foil.Distinct n
     , Foil.Distinct l
     )
  => Foil.Scope n
  -> Foil.NameBinderList n l
  -> Foil.Scope l
extendScopeWithNameBinderList scope Foil.NameBinderListEmpty = scope
extendScopeWithNameBinderList scope (Foil.NameBinderListCons x xs) =
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      let scope' = Foil.extendScope x scope
       in extendScopeWithNameBinderList scope' xs

-- | Check if a unification constraint is solved by specific substitutions
isSolvedUnificationConstraint
  :: [String]
  -> String
  -> [String]
  -> Either String Bool
isSolvedUnificationConstraint rawMetavarBinders rawUnificationConstraint rawMetaSubsts = do
  metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
  let metavarBindersMap = Map.fromList metavarBinders
  (UnificationConstraint binders _ lhs rhs) <-
    trace "parsed unification constraint" $
      parseUnificationConstraint metavarBindersMap rawUnificationConstraint
  metaSubsts <- traverse (parseMetaSubst metavarBindersMap) rawMetaSubsts
  let
    scope = nameBinderListToScope binders
    solve = nfMetaTerm scope . applyMetaSubsts scope (MetaSubsts metaSubsts)
  pure $
    alphaEquiv
      scope
      (trace (show (solve lhs)) (solve lhs))
      (trace (show (solve rhs)) (solve rhs))

-- | Check if a constraint is solved (left-hand side equals right-hand side)
isSolved :: UnificationConstraint -> Bool
isSolved (UnificationConstraint binders _binderTypes lhs rhs) =
  let scope = nameBinderListToScope binders
   in alphaEquiv scope lhs rhs

-- Data types
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

validateProblem :: Problem -> Either String ([(Solution, [UnificationConstraint])], [Solution])
validateProblem (Problem{..}) = do
  let results = map (validateSolution problemConstraints) problemSolutions
  pure (partitionEithers results)

parseRawProblem :: RawProblem -> Either String Problem
parseRawProblem RawProblem{..} = do
  metavarBindersList <- traverse parseMetavarBinder rawProblemMetavars
  let metavarBinders = Map.fromList metavarBindersList
  constraints <- traverse (parseUnificationConstraint metavarBinders) rawProblemConstraints
  solutions <- traverse (parseSolution metavarBinders) rawProblemSolutions
  pure (Problem metavarBinders constraints solutions)
 where
  parseSolution problemMetavarBinders (RawSolution{..}) = do
    -- Parse any solution-specific metavariables
    let metavars = fromMaybe [] rawSolutionMetavars
    solutionMetavarBindersList <- traverse parseMetavarBinder metavars
    let solutionMetavarBinders = Map.fromList solutionMetavarBindersList

    -- Merge problem metavariables with solution metavariables
    -- Solution metavars take precedence if there are duplicates
    let mergedMetavarBinders = Map.union solutionMetavarBinders problemMetavarBinders

    -- Parse substitutions using the merged metavariable context
    parsedSubsts <-
      traverse
        (parseMetaSubst mergedMetavarBinders)
        rawSolutionSubstitutions

    pure (Solution rawSolutionName solutionMetavarBinders (MetaSubsts parsedSubsts))

parseRawConfig :: RawConfig -> Either String Config
parseRawConfig RawConfig{..} = do
  problems <- traverse parseRawProblem rawConfigProblems
  pure (Config rawConfigLanguage rawConfigFragment problems)

validateSolution
  :: [UnificationConstraint]
  -> Solution
  -> Either (Solution, [UnificationConstraint]) Solution
validateSolution constraints solution@Solution{..} =
  -- Merge problem metavars with solution-specific metavars
  let solvedConstraints =
        map (solveUnificationConstraint solutionSubstitutions) constraints
   in if all isSolved solvedConstraints
        then Right solution
        else Left (solution, solvedConstraints)

printInvalidSolutionsWithConstraint :: (Foldable t, Show a) => (Solution, t a) -> IO ()
printInvalidSolutionsWithConstraint (solution, constraints) = do
  putStrLn $ replicate 25 '-' <> "\n"
  putStrLn $ "Solution: " <> TIO.unpack (solutionName solution)
  putStrLn ""
  putStrLn "Substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) (getMetaSubsts (solutionSubstitutions solution))
  putStrLn ""
  putStrLn "Constraints with applied substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) constraints
  putStrLn ""
  putStrLn $ replicate 25 '-' <> "\n"

-- Main function to parse and print the configuration
data ValidationError
  = ConfigError Toml.TomlDecodeError
  | ProblemError String RawProblem
  deriving (Show)

parseConfigAndValidate :: IO ()
parseConfigAndValidate = do
  configResult <- Toml.decodeFileEither configCodec "config.toml"
  case configResult of
    Left errs -> do
      putStrLn "\n=== Configuration Errors ==="
      putStr $ formatTomlErrors errs
    Right cfg -> case parseRawConfig cfg of
      Left err -> putStrLn err
      Right Config{..} ->
        mapM_ processValidation configProblems
 where
  processValidation problem =
    case validateProblem problem of
      Left err -> printError err
      Right (invalid, valid) -> do
        printValidSolutions valid
        printInvalidSolutions invalid

  printValidSolutions valid = do
    putStrLn "\n=== Validated solutions ==="
    mapM_ (putStrLn . ("✓ " ++) . show . solutionName) valid

  printInvalidSolutions invalid = do
    putStrLn "\n=== Invalid solutions ==="
    forM_ invalid $ \(solution, constraints) -> do
      putStrLn $ "✗ " ++ show (solutionName solution)
      mapM_ (putStrLn . ("  - " ++) . show) constraints

  printError err = putStrLn $ "\n=== Error ===" ++ "\n" ++ show err

-- Add helper for error formatting
formatTomlErrors :: [Toml.TomlDecodeError] -> String
formatTomlErrors errs =
  unlines $
    "Configuration errors:" : map (\err -> "  - " ++ show err) errs

-- | Main function for testing and debugging the framework
main :: IO ()
-- main = parseConfigAndValidate

-- main = mainDebug
main = mainMatchDebug

mainMatchDebug :: IO ()
mainMatchDebug = do
  let rawMetavarBinders = ["M : [t] t -> t", "H1 : [t] t -> t", "H2 : [t] t -> t"]
  let subst = ["F[a, x] ↦ a H[a, x]"]
      Right metavarBinders = parseMetavarBinders rawMetavarBinders
      Right lhs = parseMetaSubst metavarBinders "M[x] ↦ H1[x]"
      Right rhs = parseMetaSubst metavarBinders "M[x] ↦ H2[x]"
      (_, lhsAbs) = getMetaSubst lhs
      (_, rhsAbs) = getMetaSubst rhs
  print $ matchMetaAbs metavarBinders lhsAbs rhsAbs

mainDebug :: IO ()
mainDebug = do
  let rawMetavarBinder = "N : [t -> t] t -> t"
  let metavarBinders = ["X : [t, t] t"]
      constraint = "∀ m: t, n: t. X[m, n] = n"
      substs = ["X [x, y] ↦ y"]
  print $ matchUnificationConstraint metavarBinders constraint

-- print $ isSolvedUnificationConstraint [rawMetavarBinder] rawConstraint [rawSubst]

-- $setup
-- >>> import Language.Lambda.Impl

-- | Test cases
-- >>> metavarBinders = ["X : [t, t] t"]
-- >>> constraint     = "∀ m: t, n: t. X[m, n] = n"
-- >>> substs         = ["X [x, y] ↦ y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint substs
-- Right [[X [x0, x1] ↦ x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t] t"]
-- >>> constraint     = "∀ f: t -> t, g: t. M[f, g] = f g"
-- >>> subst          = ["M [x, y] ↦ x y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0 x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t] t"]
-- >>> constraint     = "∀ f : t -> t, g : t. M[f, g] = g"
-- >>> subst          = ["M [x, y] ↦ y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t -> t] t -> t"]
-- >>> constraint     = "∀ . M[λf:t. f, λg:t. g] = λy:t. y"
-- >>> subst          = ["M [x, y] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0],[M [x0, x1] ↦ x1],[M [x0, x1] ↦ λ x2 : t . x2]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t -> t -> t] t -> t"]
-- >>> constraint     = "∀ . M[λx:t. x, λy:t. λy:t. y] = λa:t. a"
-- >>> subst          = ["M [x, y] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0],[M [x0, x1] ↦ λ x2 : t . x2]]
-- Right True
-- >>> metavarBinders = ["M : [] t -> t"]
-- >>> constraint     = "∀ . M[] = λx:t.x"
-- >>> subst          = ["M [] ↦ λ x:t.x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [] ↦ λ x0 : t . x0]]
-- Right True
-- >>> metavarBinders = []
-- >>> constraint     = "∀ . λx:t.x = λy:t.y"
-- >>> subst          = []
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[]]
-- Right True
-- >>> metavarBinders = []
-- >>> constraint     = "∀ . λx:t.λx:t.x = λy:t.y"
-- >>> subst          = []  -- no substitution can solve this constraint
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right []
-- Right False
-- >>> metavarBinders = ["M : [t -> t] t -> t"]
-- >>> constraint     = "∀ . λy:t.M[λx:t.x] = λy:t.λx:t.x"
-- >>> subst          = ["M [x] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0] ↦ x0],[M [x0] ↦ λ x1 : t . x1]]
-- Right True

-- | Constraint couldn't be solved by matching
-- >>> metavarBinders = ["N : [t] t -> t"]
-- >>> constraint     = "∀ x : t, y : t. N[x] y = x"
-- >>> subst          = ["N [x] ↦ λz:t.x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right []
-- Right True

-- >>> metavarBinders = ["F: [t -> t, t] t", "X: [t -> t] t", "Y: [] t"]
-- >>> constraint     = "∀ a: t -> t. F[a, X[a]] = a Y[]"
-- >>> subst          = ["F[a, x] ↦ a H[a, x]"]
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right []
-- Left "type error"
