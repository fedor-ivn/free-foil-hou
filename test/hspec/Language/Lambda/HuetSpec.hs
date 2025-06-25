{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Lambda.HuetSpec where

import Control.Monad (forM_)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Test.Hspec

import Data.SOAS (MetaAbs (..), MetaSubst (..), MetaSubsts (..))
import Data.Text (unpack)
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
import qualified Language.Lambda.Huet as Huet
import Language.Lambda.Impl
import Language.Lambda.RawConfig (decodeConfigFile)
import qualified Language.Lambda.Syntax.Abs as Raw

handleErr :: (Show e) => Either e a -> IO a
handleErr = either (\err -> print err >> exitFailure) pure

solve :: CanonicalProblem -> Result [(CanonicalSolution, Framework.SolutionComparison)]
solve problem = Framework.solveAndCompareToReferenceSolutionsWith problem $ \metas constraints ->
  Right (toFrameworkSolution <$> Huet.solve [Huet.Problem metas constraints])

toFrameworkSolution
  :: Huet.Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig
  -> Solution
      (Huet.Metavariables Raw.Type Raw.MetavarIdent)
      (Huet.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig)
toFrameworkSolution (Huet.Solution metas _constraints substitutions) =
  Solution Nothing metas substitutions

instance Framework.IsCanonicalMetavarBinders (Huet.Metavariables Raw.Type Raw.MetavarIdent) where
  toCanonicalMetavarBinders (Huet.Metavariables (Huet.MetaTypes metas) _) =
    Right (Map.fromList binders)
   where
    binders = fmap (\(k, Huet.MetaType argumentTypes returnType) -> (k, (argumentTypes, returnType))) metas

  fromCanonicalMetavarBinders binders =
    Right (Huet.Metavariables (Huet.MetaTypes metas) (Huet.freshMetavariables Huet.someMetas))
   where
    metas = fmap (\(k, (argumentTypes, returnType)) -> (k, Huet.MetaType argumentTypes returnType)) (Map.assocs binders)

instance Framework.IsCanonicalConstraint (Huet.Constraint Raw.Type Raw.MetavarIdent FoilPattern TermSig) where
  toCanonicalConstraint _ (Huet.Constraint forall_ forallTypes lhs rhs) =
    Right (CanonicalConstraint forall_ forallTypes lhs rhs)

  fromCanonicalConstraint _ (CanonicalConstraint forall_ forallTypes lhs rhs) =
    Right (Huet.Constraint forall_ forallTypes lhs rhs)

instance Framework.IsCanonicalSubstitutions (Huet.Substitutions Raw.Type Raw.MetavarIdent FoilPattern TermSig) where
  toCanonicalSubstitution _ (Huet.Substitutions substs) = Right (MetaSubsts substs')
   where
    substs' = fmap (\(Huet.Substitution k parameters rhs) -> MetaSubst (k, MetaAbs parameters rhs)) substs

forFile path = do
  Config{configProblems} <- runIO $ handleErr =<< decodeConfigFile path
  forM_ (zip [1 ..] configProblems) $ \(i, problem) -> do
    result <- runIO $ handleErr (solve problem)
    describe ("problem #" <> show i) $ do
      forM_ (zip [1 ..] result) $ \(j, (_, comparison)) -> do
        it ("solution #" <> show j) $ do
          comparison `shouldBe` Framework.Equivalent

spec :: Spec
spec = do
  describe "F[a, b] X[a, b] = a b" $ forFile "problems/huet/problem-1.toml"
  describe "F[a, b, c] X[a, b, c] = a b c" $ forFile "problems/huet/problem-2.toml"
  describe "F[X[a, b], a, b] = a b" $ forFile "problems/huet/problem-3.toml"
  describe "F[a, b] X[a, b] b = a b" $ forFile "problems/huet/problem-4.toml"
  describe "F[] = λx. λ y. y x" $ forFile "problems/huet/problem-5.toml"
  describe "Matching" $ forFile "problems/matching.toml"
