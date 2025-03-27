{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Language.Lambda.ImplSpec where

import Control.Monad (forM_)
import Data.Either (isRight)
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Test.Hspec
import qualified Toml

import Data.SOAS (MetaSubsts (..))
import Language.Lambda.Impl as Impl

handleErr :: (Show e) => Either e a -> IO a
handleErr = either (\err -> print err >> exitFailure) pure

spec :: Spec
spec = do
  Impl.Config{..} <- runIO $ do
    configResult <- Toml.decodeFileEither configCodec "config.toml" >>= handleErr
    handleErr (Impl.parseRawConfig configResult)

  let title = Text.unpack (configLanguage <> " (fragment: " <> configFragment <> ")")
  describe title $
    forM_ (zip [1 ..] configProblems) $ \(i, Impl.Problem{..}) -> do
      describe ("problem #" <> show i) $ do
        forM_ problemSolutions $ \solution@Impl.Solution{..} -> do
          it (Text.unpack solutionName) $ do
            Impl.validateSolution problemConstraints solution `shouldSatisfy` isRight

  describe "moreGeneralThan (substitution comparison)" $ do
    -- Helper function to set up the test case
    let parseSubsts metavarBinders strs = do
          substs <- traverse (parseMetaSubst metavarBinders) strs
          return (MetaSubsts substs)
        testMoreGeneralThan rawMetavarBinder rawLhsSubst rawRhsSubst expectedResult = do
          Right metavarBinders <- pure $ parseMetavarBinders rawMetavarBinder
          Right lhsSubsts <- pure $ parseSubsts metavarBinders rawLhsSubst
          Right rhsSubsts <- pure $ parseSubsts metavarBinders rawRhsSubst
          let result = moreGeneralThan metavarBinders lhsSubsts rhsSubsts
          result `shouldBe` expectedResult

    context "when comparing simple substitutions" $ do
      it "should recognize that M[x] ↦ λy:t.N[x] is more general than M[x] ↦ λy:t.λz:t.x" $ do
        testMoreGeneralThan
          ["M : [t] t -> t -> t", "N : [t] t -> t"]
          ["M[x] ↦ λy:t.N[x]"]
          ["M[x] ↦ λy:t.λz:t.x"]
          True

      it "should recognize that M[x] ↦ λy:t.λz:t.x is not more general than M[x] ↦ λy:t.N[x]" $ do
        testMoreGeneralThan
          ["M : [t] t -> t -> t", "N : [t] t -> t"]
          ["M[x] ↦ λy:t.λz:t.x"]
          ["M[x] ↦ λy:t.N[x]"]
          False

    context "with identity and empty substitutions" $ do
      it "any substitution should be more general than itself" $ do
        testMoreGeneralThan
          ["M : [t] t"]
          ["M[x] ↦ x"]
          ["M[x] ↦ x"]
          True

      it "a non-empty substitution should not be more general than an empty one" $ do
        Right metavarBinders <- pure $ parseMetavarBinders ["M : [] t -> t"]
        Right lhsSubsts <- pure $ parseSubsts metavarBinders ["M[] ↦ λy:t.y"]
        let rhsSubsts = MetaSubsts []
        moreGeneralThan metavarBinders lhsSubsts rhsSubsts `shouldBe` False

    context "with multiple metavariables" $ do
      it "should correctly compare substitutions with multiple metavariables" $ do
        testMoreGeneralThan
          ["M : [t] t", "F : [t -> t, t] t"]
          ["M[x] ↦ x", "F[f, y] ↦ f y"]
          ["M[x] ↦ x"]
          False
