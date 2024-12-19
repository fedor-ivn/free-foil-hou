{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Strip where

import           Language.Lambda.FCU.Terms (Id, Term (..))

-- | Strip a term into a head and a list of arguments.
strip :: Term -> (Term, [Term])
strip (t1 :@ t2) =
  let (h, rest) = strip t1
   in (h, rest ++ [t2])
strip t = (t, [])

--- >>> strip ("X" :@ "y" :@ "z")
-- (X,[y,z])

--- >>> strip ("Cons" :@ "x" :@ ("y" :@ ("z" :.: "z")))
-- (Cons,[x,y λz . (z)])


-- | Combine a head term with arguments to reconstruct the original term
unstrip :: (Term, [Term]) -> Term
unstrip (head, args) = foldl (:@) head args

-- >>> unstrip ("X", ["y", "z"])
-- (W "X" :@ O "y") :@ O "z"

-- >>> ("X" :@ "y" :@ "z")
-- (W "X" :@ O "y") :@ O "z"

-- >>> unstrip ("Cons", ["x", "y" :@ ("z" :.: "z")])
-- (Constructor "Cons" :@ O "x") :@ (O "y" :@ ("z" :.: O "z"))
