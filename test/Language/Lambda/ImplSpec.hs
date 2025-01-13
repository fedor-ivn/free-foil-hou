{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Lambda.ImplSpec where

import Data.Either (isRight)
import Control.Monad (forM_)
import Test.Hspec
import qualified Data.Text as Text
import qualified Toml
import System.Exit (exitFailure)

import Language.Lambda.Impl as Impl

spec :: Spec
spec = do
  Impl.Config{..} <- runIO $ do
    configResult <- Toml.decodeFileEither configCodec "config.toml"
    case configResult of
      Left err -> do
        print err
        exitFailure
      Right config -> return config
  
  let title = Text.unpack (configLanguage <> " (fragment: " <> configFragment <> ")")
  describe title $
    forM_ (zip [1..] configProblems) $ \(i, Impl.Problem{..}) -> do
      describe ("problem #" <> show i) $ do
        forM_ problemSolutions $ \solution@Impl.Solution{..} -> do
          it (Text.unpack solutionName) $ do
            Impl.validateSolution problemConstraints solution `shouldSatisfy` isRight