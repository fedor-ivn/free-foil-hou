-- | Run second-order matching through the typed lambda-calculus adapter.
module Main (main) where

import Control.Monad (forM_)
import Language.Lambda.Framework (matchUnificationConstraint)
import System.Exit (die)

main :: IO ()
main = forM_ (simpleProblems ++ nestedProblems) $ \(declarations, constraint) -> do
  putStrLn constraint
  solutions <- either die pure (matchUnificationConstraint declarations constraint)
  case solutions of
    [] -> putStrLn "No solutions"
    _ -> forM_ solutions print
  putStrLn ""
 where
  simpleProblems =
    [ (["M : [t, t] t"], "∀ x: t. M[x, x] = x")
    , (["M : [] t"], "∀ x: t. M[] = x")
    , ([], "∀ x: t. x = x")
    ]
  nestedProblems =
    [ ( ["M : [t -> t, t] t", "N : [t -> t, t] t"]
      , "∀ p: t -> t -> t, f: t -> t, x: t. "
          ++ "λu:t. p (M[f, N[f, x]]) (N[f, u]) = "
          ++ "λv:t. p (f x) (" ++ target ++ ")"
      )
    | target <- ["v", "f v", "f (f v)"]
    ]
