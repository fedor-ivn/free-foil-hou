{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Discharge where

import           Language.Lambda.FCU.Terms (Id, Term (..))

-- | Discharge a term by replacing all terms t in s with variables z.
discharge :: [(Term, Id)] -> Term -> Term
discharge th t =
  case lookup t th of
    Just y -> O y
    Nothing ->
      case t of
        (x :.: t1) -> x :.: discharge th t1
        (f :@ x)   -> discharge th f :@ discharge th x
        _          -> t
-- >>> discharge [("Cons" :@ "x" :@ "y", "z")] ("Cons" :@ "x" :@ "y")
-- z
-- >>> discharge [("Fst" :@ "x", "z")] ("Cons" :@ ("Fst" :@ "x") :@ "w")
-- Cons (z) (w)
-- >>> discharge [("x", "z1"), ("y", "z2")] ("Cons" :@ "x" :@ ("x" :.: "y"))
-- Cons (z1) (λx . (z2))
