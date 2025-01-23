{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.FCUImplSW.Discharge where

import Language.Lambda.FCU.FCUImplSW.Terms (Id, Term (..))

-- | Discharge a term by replacing all terms t in s with variables z.
discharge :: [(Term, Id)] -> Term -> Term
discharge th t = case lookup t th of
  Just y -> O y
  Nothing -> case t of
    (x :.: t1) -> x :.: discharge th t1
    (t1 :@ t2) -> discharge th t1 :@ discharge th t2
    t -> t

-- >>> discharge [("Cons" :@ "x" :@ "y", "z")] ("Cons" :@ "x" :@ "y")
-- z

-- >>> discharge [("Fst" :@ "x", "z")] ("Cons" :@ ("Fst" :@ "x") :@ "w")
-- (Cons z) (w)
