{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Language.Lambda.FCU.Strip where

import Language.Lambda.FCU.Terms
import qualified Language.Lambda.FCU.FCUSyntax.Abs as Raw

-- | Strip a term into a head and a list of arguments.
strip :: Raw.Term -> (Raw.Term, [Raw.Term])
strip (Raw.AppTerm t1 t2) =
  let (h, rest) = strip t1
   in (h, rest ++ [t2])
strip t = (t, [])

--- >>> strip ("X :@ y :@ z")
-- (X,[(y z)])

--- >>> strip ("Cons :@ x :@ ( y :@ ( z :.: z ) )")
-- (Cons,[(x (y λz . z))])

-- | Combine a head term with arguments to reconstruct the original term
unstrip :: (Raw.Term, [Raw.Term]) -> Raw.Term
unstrip (head, args) = case args of
    [] -> head
    _ -> Raw.AppTerm head (foldl1 Raw.AppTerm args)

-- >>> unstrip ("X", ["y", "z"])
-- (X (y z))

-- >>> "X :@ y :@ z" :: Raw.Term
-- (X (y z))

-- >>> unstrip ("Cons", ["x", "y :@ ( z :.: z )"])
-- (Cons (x (y λz . z)))
