{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Prune where

import Data.List (elemIndex)
import Language.Lambda.FCU.Covers (coverExists)
import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm)
import Language.Lambda.FCU.Strip (strip)
import Language.Lambda.FCU.Substitutions (Substitutions (..), devar, mkvars)
import Language.Lambda.FCU.Terms (Id, Term (..), newMetaVarId, subset)

abst :: ([Id], Term) -> Term
abst ([], t) = t
abst (x : xs, t) = x :.: abst (xs, t)

-- >>> abst (["x", "y"], "z")
-- λx . (λy . (z))

-- >>> abst (mkvars ["x" :@ "y", "z" :.: "z"], "z" :@ "w")
-- λz1 . (λz2 . (z w))

hnf :: ([Id], [Char], [Id]) -> Term
hnf (vars, base, args) =
  foldr
    (:.:)
    (foldl (\acc x -> acc :@ O x) (Constructor base) args)
    vars

-- >>> hnf (["z1", "z2"], "X", ["z1", "z2", "z3"])
-- λz1 . (λz2 . (((X z1) z2) z3))

-- >>> hnf (mkvars ["x" :@ "y", "z" :.: "z"], "X", mkvars ["x" :@ "y", "z" :.: "z"])
-- λz1 . (λz2 . ((X z1) z2))

foldlN :: (([(Id, Term)], Term) -> [(Id, Term)]) -> ([(Id, Term)], [Term]) -> [(Id, Term)]
foldlN _ (rho, []) = rho
foldlN f (rho, t : ts) = foldlN f (f (rho, t), ts)

-- | Select the variables in the first list that are in the second list
eqsel :: [Id] -> [Term] -> [Term] -> [Id]
eqsel vsm tn sm =
  [v | (v, s) <- zip vsm sm, s `elem` tn]

-- >>> eqsel ["z1", "z2", "z3"] ["x", "y", "z"] ["x", "y", "z"]
-- ["z1","z2","z3"]

-- >>> eqsel ["z1", "z2"] ["x", "y"] ["x", "w"]
-- ["z1"]

-- >>> eqsel ["z1", "z2"] ["x", "y"] ["z", "x"]
-- ["z2"]

prune :: [Term] -> ([(Id, Term)], Term) -> [(Id, Term)]
prune tn (rho, u) = case strip (devar rho u) of
  (_, []) -> rho
  (x :.: t', _) -> prune (O x : tn) (rho, t')
  (Constructor _, rr) -> foldlN (prune tn) (rho, rr)
  (O x, rr) ->
    if O x `elem` tn
      then foldlN (prune tn) (rho, rr)
      else error (show (O x) ++ " not in " ++ show tn)
  (W _W, sm) ->
    if sm `subset` tn -- all sm appear in lhs
      then rho
      else
        let vsm = mkvars sm
         in (_W, hnf (vsm, newMetaVarId _W, eqsel vsm tn sm)) : rho
  _ -> error "Prune: unexpected case"

-- >>> prune ["x", "y"] ([], "Cons" :@ "x" :@ "y")
-- []

-- >>> prune ["x", "q", "y"] ([], "X" :@ ("Snd" :@ "x") :@ "q")
-- [("X",λz1 . (λz2 . (X' z2)))]

-- >>> prune ["y"] ([("X", "z1" :.: ("Cons" :@ "x"))], "Cons" :@ "x")
-- [("X",λz1 . (Cons x))]

-- >>> prune ["a", "b1", "c"] ([], "X" :@ "a" :@ "b2" :@ "c")
-- [("X",λz1 . (λz2 . (λz3 . ((X' z1) (z3)))))]

-- >>> prune ["l1", "l2"] ([], "Snd" :@ "l3" :@ "l1")
-- []
