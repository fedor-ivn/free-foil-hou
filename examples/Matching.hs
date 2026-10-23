-- | Run second-order matching through the typed lambda-calculus adapter.
-- The projection example specialises the repeated-argument example of
-- Fedor Ivanov and Nikolai Kudasov in 'Data.SOAS'.
module Main (main) where

import Control.Monad (forM_)
import Language.Lambda.Framework (matchUnificationConstraint)
import System.Exit (die)

main :: IO ()
main = forM_ problems $ \(declarations, constraint) -> do
  putStrLn constraint
  solutions <- either die pure (matchUnificationConstraint declarations constraint)
  case solutions of
    [] -> putStrLn "No solutions"
    _ -> forM_ solutions print
  putStrLn ""
 where
  problems =
    [ (["M : [t, t] t"], "∀ x: t. M[x, x] = x")
    , (["M : [] t"], "∀ x: t. M[] = x")
    , ([], "∀ x: t. x = x")
    ]
