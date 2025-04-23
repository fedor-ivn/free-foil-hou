{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Framework for testing and validating pattern matching and unification.
module Language.Lambda.Framework (
  -- * Types

  -- * Parsing
  parseMetavarBinders,
  matchUnificationConstraint,
  isSolvedUnificationConstraint,
  parseMetaSubst,

  -- * Validation
  validateProblem,
  validateSolution,
  solveUnificationConstraint,
  isSolved,

  -- * TOML configuration
  parseConfigAndValidate,
  printInvalidSolutionsWithConstraint,

  -- * Instances
  IsCanonicalMetavarBinders (..),
  IsCanonicalConstraint (..),
  IsCanonicalSubstitutions (..),
  SolutionComparison (..),
  solveAndCompareToReferenceSolutionsWith,

  -- * Main entry point
  main,
  solveByMatchingAndCompareToReferenceSolutionsWith,
) where

import Control.Monad (forM_)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.SOAS
import qualified Data.Text as TIO
import Debug.Trace (trace)

import qualified Control.Monad.Foil.Internal as Foil
import Control.Monad.Free.Foil (alphaEquiv)
import Language.Lambda.Config (
  CanonicalConstraint (..),
  CanonicalProblem,
  CanonicalSolution,
  Config (..),
  IsCanonicalConstraint (..),
  IsCanonicalMetavarBinders (..),
  IsCanonicalSubstitutions (..),
  Problem (..),
  Result,
  Solution (..),
  toCanonicalSolution,
  toUnificationConstraint,
 )
import Language.Lambda.Impl (
  MetaSubst',
  MetaSubsts',
  MetavarBinder,
  MetavarBinders,
  matchMetaAbs,
  nfMetaTerm,
  toMetaSubst,
 )
import Language.Lambda.RawConfig (decodeConfigFile)
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw

-- ** Test framework implementation

-- | Solve a unification problem and compare the results to reference solutions
--   using a user-implemented algorithm. The function takes a problem definition
--   and a solving function, and returns a list of comparisons between the
--   solutions found by the algorithm and the reference solutions provided in
--   the problem definition.
solveAndCompareToReferenceSolutionsWith
  :: ( IsCanonicalConstraint c
     , IsCanonicalSubstitutions s
     , IsCanonicalMetavarBinders b
     )
  => CanonicalProblem
  -- ^ The problem definition with constraints to solve
  -> (b -> [c] -> Result [Solution b s])
  -- ^ User's algorithm function
  -> Result [(CanonicalSolution, SolutionComparison)]
solveAndCompareToReferenceSolutionsWith problem solve = do
  solutions <- solveWith solve problem
  mapM findBestComparison solutions
 where
  originalBinders = problemMetavarBinders problem
  referenceSolutions = problemSolutions problem
  compare' = flip (compareSolutions originalBinders)
  findBestComparison solution = do
    let comparisons = map (compare' solution) referenceSolutions
    case comparisons of
      [] -> Left "No reference solutions"
      (x : xs) -> Right (solution, foldr max x xs)

-- >>> foldr (fmap . max) (Left "No reference solutions") [Right 1, Right 2]
-- Left "No reference solutions"

solveByMatchingAndCompareToReferenceSolutionsWith
  :: CanonicalProblem -> Result [(CanonicalSolution, SolutionComparison)]
solveByMatchingAndCompareToReferenceSolutionsWith problem =
  solveAndCompareToReferenceSolutionsWith problem solve
 where
  solve :: MetavarBinders -> [CanonicalConstraint] -> Result [CanonicalSolution]
  solve metavarBinders [CanonicalConstraint binders binderTypes lhs rhs] =
    let scope = Foil.extendScopePattern binders Foil.emptyScope
        substs = match scope metavarBinders binderTypes lhs rhs
        solutions = map (Solution Nothing metavarBinders) substs
     in Right solutions
  solve _ _ = Left "Invalid number of constraints"

solveWith
  :: forall b c s
   . ( IsCanonicalConstraint c
     , IsCanonicalSubstitutions s
     , IsCanonicalMetavarBinders b
     )
  => (b -> [c] -> Result [Solution b s])
  -- ^ User's algorithm function
  -> CanonicalProblem
  -- ^ The problem definition with constraints to solve
  -> Result [CanonicalSolution]
  -- ^ Results for each constraint
solveWith solve Problem{..} = do
  binders' <-
    fromCanonicalMetavarBinders problemMetavarBinders
  constraint' <-
    traverse (fromCanonicalConstraint problemMetavarBinders) problemConstraints
  solutions <- solve binders' constraint'
  traverse (toCanonicalSolution problemMetavarBinders) solutions

data SolutionComparison
  = Incomparable
  | RightMoreGeneral
  | LeftMoreGeneral
  | Equivalent
  deriving (Show, Eq, Ord)

-- >>> Right originalBinders = Language.Lambda.Framework.parseMetavarBinders ["M : [t] t -> t -> t"]
-- >>> Right lhsBinders = parseMetavarBinders ["N : [t] t -> t"]
-- >>> Right lhsSubst = parseMetaSubst (originalBinders <> lhsBinders) "M[x] ↦ λy:t.N[x]"
-- >>> Right rhsSubst = parseMetaSubst originalBinders "M[x] ↦ λy:t.λz:t.x"
-- >>> lhsSubsts = MetaSubsts [lhsSubst]
-- >>> rhsSubsts = MetaSubsts [rhsSubst]
-- >>> compareSolutions originalBinders (Solution Nothing lhsBinders lhsSubsts) (Solution Nothing Map.empty rhsSubsts)
-- LeftMoreGeneral
-- >>> Right originalBinders = parseMetavarBinders ["M : [t] t -> t -> t"]
-- >>> Right lhsBinders = parseMetavarBinders ["N : [t] t -> t"]
-- >>> Right rhsBinders = parseMetavarBinders ["A : [t] t -> t"]
-- >>> Right lhsSubst = parseMetaSubst (originalBinders <> lhsBinders) "M[x] ↦ λy:t.N[x]"
-- >>> Right rhsSubst = parseMetaSubst (originalBinders <> rhsBinders) "M[x] ↦ λy:t.A[x]"
-- >>> lhsSubsts = MetaSubsts [lhsSubst]
-- >>> rhsSubsts = MetaSubsts [rhsSubst]
-- >>> compareSolutions originalBinders (Solution Nothing lhsBinders lhsSubsts) (Solution Nothing rhsBinders rhsSubsts)
-- Equivalent
compareSolutions
  :: MetavarBinders
  -> CanonicalSolution
  -> CanonicalSolution
  -> SolutionComparison
compareSolutions originalBinders left right =
  let leftIsMoreGeneral =
        isMoreGeneralSubstitution originalBinders left right
      rightIsMoreGeneral =
        isMoreGeneralSubstitution originalBinders right left
   in case (leftIsMoreGeneral, rightIsMoreGeneral) of
        (True, True) -> Equivalent
        (True, False) -> LeftMoreGeneral
        (False, True) -> RightMoreGeneral
        (False, False) -> Incomparable

isMoreGeneralSubstitution
  :: MetavarBinders
  -> CanonicalSolution
  -> CanonicalSolution
  -> Bool
isMoreGeneralSubstitution originalBinders left right =
  flip
    all
    (Map.toList originalBinders)
    $ \(metavar, (argTypes, _)) ->
      let maybeSubsts = do
            leftAbs <- lookup metavar leftSubsts
            rightAbs <- lookup metavar rightSubsts
            let substs = matchMetaAbs argTypes allBinders leftAbs rightAbs
            listToMaybe substs
       in isJust maybeSubsts
 where
  allBinders =
    originalBinders
      <> solutionMetavarBinders left
      <> solutionMetavarBinders right
  leftSubsts = map metaSubst $ metaSubsts $ solutionSubstitutions left
  rightSubsts = map metaSubst $ metaSubsts $ solutionSubstitutions right

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

parseUnificationConstraint :: MetavarBinders -> String -> Either String CanonicalConstraint
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
    (CanonicalConstraint binders binderTypes lhs rhs) <-
      parseUnificationConstraint
        metavarBindersMap
        rawUnificationConstraint
    let scope = nameBinderListToScope binders
        substs = match scope metavarBindersMap binderTypes lhs rhs
    pure substs

-- | Apply substitutions to both sides of a unification constraint
solveUnificationConstraint
  :: MetaSubsts'
  -> CanonicalConstraint
  -> CanonicalConstraint
solveUnificationConstraint
  substs
  (CanonicalConstraint binders binderTypes lhs rhs) =
    let scope = nameBinderListToScope binders
        solve = nfMetaTerm scope . applyMetaSubsts scope substs
     in CanonicalConstraint
          binders
          binderTypes
          (solve lhs)
          (solve rhs)

nameBinderListToScope
  :: Foil.NameBinderList Foil.VoidS n
  -> Foil.Scope n
nameBinderListToScope = flip Foil.extendScopePattern Foil.emptyScope

-- | Check if a unification constraint is solved by specific substitutions
isSolvedUnificationConstraint
  :: [String]
  -> String
  -> [String]
  -> Either String Bool
isSolvedUnificationConstraint rawMetavarBinders rawUnificationConstraint rawMetaSubsts = do
  metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
  let metavarBindersMap = Map.fromList metavarBinders
  (CanonicalConstraint binders _ lhs rhs) <-
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
isSolved :: CanonicalConstraint -> Bool
isSolved (CanonicalConstraint binders _binderTypes lhs rhs) =
  let scope = nameBinderListToScope binders
   in alphaEquiv scope lhs rhs

validateProblem :: CanonicalProblem -> Either String ([(CanonicalSolution, [CanonicalConstraint])], [CanonicalSolution])
validateProblem (Problem{..}) = do
  let results = map (validateSolution problemConstraints) problemSolutions
  pure (partitionEithers results)

validateSolution
  :: [CanonicalConstraint]
  -> CanonicalSolution
  -> Either (CanonicalSolution, [CanonicalConstraint]) CanonicalSolution
validateSolution constraints solution@Solution{..} =
  -- Merge problem metavars with solution-specific metavars
  let solvedConstraints =
        map (solveUnificationConstraint solutionSubstitutions) constraints
   in if all isSolved solvedConstraints
        then Right solution
        else Left (solution, solvedConstraints)

printInvalidSolutionsWithConstraint :: (Foldable t, Show a) => (CanonicalSolution, t a) -> IO ()
printInvalidSolutionsWithConstraint (solution, constraints) = do
  putStrLn $ replicate 25 '-' <> "\n"
  putStrLn $ "Solution: " <> TIO.unpack (fromMaybe "<no name>" $ solutionName solution)
  putStrLn ""
  putStrLn "Substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) (metaSubsts (solutionSubstitutions solution))
  putStrLn ""
  putStrLn "Constraints with applied substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) constraints
  putStrLn ""
  putStrLn $ replicate 25 '-' <> "\n"

parseConfigAndValidate :: IO ()
parseConfigAndValidate = do
  configResult <- decodeConfigFile "config.toml"
  case configResult of
    Left errs -> do
      putStrLn "\n=== Configuration Errors ==="
      putStr errs
    Right cfg ->
      mapM_ processValidation (configProblems cfg)
 where
  processValidation problem =
    case validateProblem problem of
      Left err -> printError err
      Right (invalid, valid) -> do
        printValidSolutions valid
        printInvalidSolutions invalid

  printValidSolutions valid = do
    putStrLn "\n=== Validated solutions ==="
    mapM_ (putStrLn . ("✓ " ++) . TIO.unpack . fromMaybe "<no name>" . solutionName) valid

  printInvalidSolutions invalid = do
    putStrLn "\n=== Invalid solutions ==="
    forM_ invalid $ \(solution, constraints) -> do
      putStrLn $ "✗ " ++ show (solutionName solution)
      mapM_ (putStrLn . ("  - " ++) . show) constraints

  printError err = putStrLn $ "\n=== Error ===" ++ "\n" ++ show err

-- | Main function for testing and debugging the framework
main :: IO ()
main = mainFoo

-- main = parseConfigAndValidate
-- main = mainDebug
-- main = mainMatchDebug

mainFoo :: IO ()
mainFoo = do
  config <- decodeConfigFile "problems/matching.toml"
  case config of
    Left err -> print err
    Right cfg -> do
      forM_ (zip [1 ..] (configProblems cfg)) $ \(i, problem) -> do
        putStrLn $ "Problem #" ++ show i
        case solveByMatchingAndCompareToReferenceSolutionsWith problem of
          Left err -> print err
          Right result ->
            forM_ result $ \(solution, comparison) -> do
              putStrLn $ "Solution: " ++ show solution
              putStrLn $ "Comparison: " ++ show comparison

mainMatchDebug :: IO ()
mainMatchDebug = either print print result
 where
  result = do
    let rawMetavarBinders = ["M : [t] t -> t", "H1 : [t] t -> t", "H2 : [t] t -> t"]
    metavarBinders <- parseMetavarBinders rawMetavarBinders
    let res = Map.lookup "M" metavarBinders
    (argTypes, _) <- maybe (Left "not found") Right res
    lhs <- parseMetaSubst metavarBinders "M[x] ↦ H1[x]"
    rhs <- parseMetaSubst metavarBinders "M[x] ↦ H2[x]"
    let (_, lhsAbs) = metaSubst lhs
        (_, rhsAbs) = metaSubst rhs
    pure $ matchMetaAbs argTypes metavarBinders lhsAbs rhsAbs

-- mainDebug :: IO ()
-- mainDebug = do
--   let rawMetavarBinder = "N : [t -> t] t -> t"
--   let metavarBinders = ["X : [t, t] t"]
--       constraint = "∀ m: t, n: t. X[m, n] = n"
--       substs = ["X [x, y] ↦ y"]
--   print $ matchUnificationConstraint metavarBinders constraint

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
-- Left "type error"
