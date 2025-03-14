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


