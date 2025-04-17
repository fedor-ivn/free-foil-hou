{-# LANGUAGE ImportQualifiedPost #-}


module Language.Lambda.FCU.Strip where

import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw

-- | Strip a term into a head and a list of arguments.
strip :: Raw.Term -> (Raw.Term, [Raw.Term])
strip (Raw.AppTerm t1 t2) =
  let (h, rest) = strip t1
   in (h, rest ++ [t2])
strip t = (t, [])

-- (WTerm (MetavarId "X"),[AppTerm (OTerm (Id "y")) (OTerm (Id "z"))])
-- (WTerm (MetavarId "X"),[AppTerm (OTerm (Id "y")) (OTerm (Id "z"))])

--- >>> strip ("Cons x ( y ( λ z . z ) )")
-- (CTerm (ConstructorId "Cons"),[AppTerm (OTerm (Id "x")) (AppTerm (OTerm (Id "y")) (AbsTerm (PatternVar (Id "z")) (ScopedTerm (OTerm (Id "z")))))])

-- | Combine a head term with arguments to reconstruct the original term
unstrip :: (Raw.Term, [Raw.Term]) -> Raw.Term
unstrip (hd, args) = case args of
  [] -> hd
  _ -> Raw.AppTerm hd (foldl1 Raw.AppTerm args)

-- >>> unstrip ("X", ["y", "z"])
-- AppTerm (WTerm (MetavarId "X")) (AppTerm (OTerm (Id "y")) (OTerm (Id "z")))

-- >>> "X y z" :: Raw.Term
-- AppTerm (WTerm (MetavarId "X")) (AppTerm (OTerm (Id "y")) (OTerm (Id "z")))

-- >>> unstrip ("Cons", ["x", "y (λ z . z )"])
-- AppTerm (CTerm (ConstructorId "Cons")) (AppTerm (OTerm (Id "x")) (AppTerm (OTerm (Id "y")) (AbsTerm (PatternVar (Id "z")) (ScopedTerm (OTerm (Id "z"))))))
