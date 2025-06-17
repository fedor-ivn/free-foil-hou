{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Lambda.FCUSpec where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import Control.Monad.Foil qualified as Foil
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.List (intercalate)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import qualified Language.Lambda.FCU.FreeFoil.Syntax as FCU

import Data.SOAS (MetaAbs (..), MetaSubst (..), MetaSubsts (..), AnnBinder(..), TypedSOAS(..))
import Language.Lambda.Config (
  CanonicalConstraint (..),
  CanonicalProblem,
  CanonicalSolution,
  Config (..),
  Problem (..),
  Result,
  Solution (..),
 )
import qualified Language.Lambda.Framework as Framework
import Language.Lambda.Impl
import Language.Lambda.RawConfig (decodeConfigFile)
import qualified Language.Lambda.Syntax.Abs as Raw

handleErr :: (Show e) => Either e a -> IO a
handleErr = either (\err -> print err >> exitFailure) pure

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (FCU.Substitution typ metavar binder sig)
  where
  show (FCU.Substitution meta parameters body) =
    show meta <> "[" <> intercalate ", " parameters' <> "] ↦ " <> show body
   where
    parameters' = fmap (\name -> "x" <> show name) (Foil.namesOfPattern parameters)

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (FCU.Substitutions typ metavar binder sig)
  where
  show (FCU.Substitutions []) = "{ }"
  show (FCU.Substitutions substitutions) = "{ " <> intercalate ", " (show <$> substitutions) <> " }"

instance FCU.MetavarFreshable Raw.MetavarIdent where
  newMetavarId (Raw.MetavarIdent name) = Raw.MetavarIdent (name ++ "'")

instance FCU.TypedUnifiablePattern Raw.Type (AnnBinder Raw.Type FoilPattern) where
  typedUnifyPatterns
    (AnnBinder (FoilAPattern left) leftType)
    (AnnBinder (FoilAPattern right) rightType)
      | leftType == rightType = case Foil.unifyNameBinders left right of
          Foil.NotUnifiable -> FCU.NotUnifiable'
          Foil.RenameBothBinders unifiedBinder leftRenaming rightRenaming ->
            FCU.RenameBothBinders' unifiedBinder (Foil.addNameBinders unifiedBinder [leftType]) leftRenaming rightRenaming
          Foil.RenameLeftNameBinder unifiedBinder leftRenaming ->
            FCU.RenameBothBinders' unifiedBinder (Foil.addNameBinders unifiedBinder [leftType]) leftRenaming id
          Foil.RenameRightNameBinder unifiedBinder rightRenaming ->
            FCU.RenameBothBinders' unifiedBinder (Foil.addNameBinders unifiedBinder [leftType]) id rightRenaming
          Foil.SameNameBinders unifiedBinder ->
            FCU.RenameBothBinders' unifiedBinder (Foil.addNameBinders unifiedBinder [leftType]) id id
  typedUnifyPatterns _ _ = FCU.NotUnifiable'

data FCUMetavariables = FCUMetavariables
  deriving (Show)

solveFCU ::
  CanonicalProblem -> Result [(CanonicalSolution, Framework.SolutionComparison)]
solveFCU problem =
  Framework.solveAndCompareToReferenceSolutionsWith problem solver
  where
    solver :: FCUMetavariables -> 
              [FCU.Constraint Raw.Type Raw.MetavarIdent FoilPattern TermSig] -> 
              Result [Solution FCUMetavariables (FCU.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig)]
    solver _metas constraints =
      let result = unsafePerformIO $ try @SomeException $ evaluate $
            foldl
              (\currentSubs constraint -> FCU.unify (currentSubs, constraint))
              (FCU.Substitutions []) -- Initial empty substitutions
              constraints
      in
      Right $ case result of
        Left err -> unsafePerformIO (print err) `seq` []
        Right finalSubstitutions -> unsafePerformIO (print finalSubstitutions) `seq` [toFrameworkSolution finalSubstitutions]

toFrameworkSolution ::
  FCU.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig ->
  Solution
    FCUMetavariables
    (FCU.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig)
toFrameworkSolution substitutions =
  Solution Nothing FCUMetavariables substitutions

instance Framework.IsCanonicalMetavarBinders FCUMetavariables where
  toCanonicalMetavarBinders _ = Right Map.empty
  fromCanonicalMetavarBinders _ = Right FCUMetavariables

instance Framework.IsCanonicalConstraint (FCU.Constraint Raw.Type Raw.MetavarIdent FoilPattern TermSig) where
  toCanonicalConstraint _ (FCU.Constraint forall_ forallTypes lhs rhs) =
    Right (CanonicalConstraint forall_ forallTypes lhs rhs)

  fromCanonicalConstraint _ (CanonicalConstraint forall_ forallTypes lhs rhs) =
    Right (FCU.Constraint forall_ forallTypes lhs rhs)

instance Framework.IsCanonicalSubstitutions (FCU.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig) where
  toCanonicalSubstitution _ (FCU.Substitutions substs) = Right (MetaSubsts substs')
   where
    substs' = fmap (\(FCU.Substitution k params body) -> MetaSubst (k, MetaAbs params body)) substs

forFile :: FilePath -> Spec
forFile path = do
  Config{configProblems} <- runIO $ handleErr =<< decodeConfigFile path
  let [problem] = configProblems
  result <- runIO $ handleErr (solveFCU problem)
  forM_ (zip [1 ..] result) $ \(j, (_, comparison)) -> do
    it ("solution #" <> show j) $ do
      comparison `shouldBe` Framework.Equivalent

spec :: Spec
spec = do
  describe "a = a" $ forFile "problems/fcu/problem-0.toml"
  describe "f a b = f a b" $ forFile "problems/fcu/problem-1.toml"
  describe "X a = f a b" $ forFile "problems/fcu/problem-2.toml"
  describe "X x = x (Y y x)" $ forFile "problems/fcu/problem-3.toml"
  describe "X x y z = X z y x" $ forFile "problems/fcu/problem-4.toml"
  describe "X x y z l = Y z y x" $ forFile "problems/fcu/problem-5.toml"
