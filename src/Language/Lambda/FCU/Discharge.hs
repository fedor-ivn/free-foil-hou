{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Language.Lambda.FCU.Discharge where

import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw

-- | Discharge a term by replacing all terms t in s with variables z.
discharge :: [(Raw.Term, Raw.Id)] -> Raw.Term -> Raw.Term
discharge th t = case lookup t th of
  Just y -> Raw.OTerm y
  Nothing -> case t of
    Raw.AbsTerm x (Raw.ScopedTerm t1) -> Raw.AbsTerm x (Raw.ScopedTerm (discharge th t1))
    Raw.AppTerm t1 t2 -> Raw.AppTerm (discharge th t1) (discharge th t2)
    t -> t

-- >>> discharge [("Cons x y", "z")] "Cons x y"
-- OTerm (Id "z")

-- >>> discharge [("Fst x", "z")] "Cons ( Fst x ) w"
-- AppTerm (CTerm (ConstructorId "Cons")) (AppTerm (OTerm (Id "z")) (OTerm (Id "w")))
