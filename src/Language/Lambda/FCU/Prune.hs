{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Prune where

import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.Strip (strip)
import Language.Lambda.FCU.Substitutions (Substitutions (..), combineSubstitutions, devar, mkvars)
import Language.Lambda.FCU.Terms (newMetaVarId, subset)

abst :: ([Raw.Id], Raw.Term) -> Raw.Term
abst ([], t) = t
abst (x : xs, t) = Raw.AbsTerm (Raw.PatternVar x) (Raw.ScopedTerm (abst (xs, t)))

-- >>> abst (["x", "y"], "z")
-- AbsTerm (PatternVar (Id "x")) (ScopedTerm (AbsTerm (PatternVar (Id "y")) (ScopedTerm (OTerm (Id "z")))))

-- >>> abst (mkvars ["x y", "λ z . z"], "z w")
-- AbsTerm (PatternVar (Id "z1")) (ScopedTerm (AbsTerm (PatternVar (Id "z2")) (ScopedTerm (AppTerm (OTerm (Id "z")) (OTerm (Id "w"))))))

foldlN :: ((Substitutions, Raw.Term) -> Substitutions) -> (Substitutions, [Raw.Term]) -> Substitutions
foldlN _ (rho, []) = rho
foldlN f (rho, t : ts) = foldlN f (f (rho, t), ts)

hnf :: ([Raw.Id], Raw.Term, [Raw.Id]) -> Raw.Term
hnf (vars, base, args) =
  foldr
    (\p t -> Raw.AbsTerm p (Raw.ScopedTerm t))
    (foldl (\acc x -> Raw.AppTerm acc (Raw.OTerm x)) base args)
    (map Raw.PatternVar vars)

-- >>> hnf (["z1", "z2"], "X", ["z1", "z2", "z3"])
-- AbsTerm (PatternVar (Id "z1")) (ScopedTerm (AbsTerm (PatternVar (Id "z2")) (ScopedTerm (AppTerm (AppTerm (AppTerm (WTerm (MetavarId "X")) (OTerm (Id "z1"))) (OTerm (Id "z2"))) (OTerm (Id "z3"))))))

-- >>> hnf (mkvars ["x y", "λ z . z"], "X", mkvars ["x y", "λ z . z"])
-- AbsTerm (PatternVar (Id "z1")) (ScopedTerm (AbsTerm (PatternVar (Id "z2")) (ScopedTerm (AppTerm (AppTerm (WTerm (MetavarId "X")) (OTerm (Id "z1"))) (OTerm (Id "z2"))))))

-- | Select the variables in the first list that are in the second list
eqsel :: [Raw.Id] -> [Raw.Term] -> [Raw.Term] -> [Raw.Id]
eqsel vsm tn sm =
  [v | (v, s) <- zip vsm sm, s `elem` tn]

-- >>> eqsel ["z1", "z2", "z3"] ["x", "y", "z"] ["x", "y", "z"]
-- [Id "z1",Id "z2",Id "z3"]

-- >>> eqsel ["z1", "z2"] ["x", "y"] ["x", "w"]
-- [Id "z1"]

-- >>> eqsel ["z1", "z2"] ["x", "y"] ["z", "x"]
-- [Id "z2"]

prune :: [Raw.Term] -> (Substitutions, Raw.Term) -> Substitutions
prune tn (rho, u) = case strip (devar rho u) of
  (_, []) -> rho
  (Raw.AbsTerm (Raw.PatternVar x) (Raw.ScopedTerm t'), _) -> prune (Raw.OTerm x : tn) (rho, t')
  (Raw.CTerm _, rr) -> foldlN (prune tn) (rho, rr)
  (Raw.OTerm x, rr) ->
    if Raw.OTerm x `elem` tn
      then foldlN (prune tn) (rho, rr)
      else error (show (Raw.OTerm x) ++ " not in " ++ show tn)
  (Raw.WTerm (Raw.MetavarId _W), sm) ->
    if sm `subset` tn -- all sm appear in lhs
      then rho
      else
        let vsm = mkvars sm
            newSubst = Substitutions [(_W, hnf (vsm, Raw.WTerm (newMetaVarId (Raw.MetavarId _W)), eqsel vsm tn sm))]
         in combineSubstitutions newSubst rho
  _ -> error "Prune: unexpected case"

-- >>> prune ["x", "y"] (Substitutions [], "Cons x y")
-- []

-- >>> prune ["x", "q", "y"] (Substitutions [], "( X ( Snd x ) ) q")
-- [("X" |-> AbsTerm (PatternVar (Id "z1")) (ScopedTerm (AbsTerm (PatternVar (Id "z2")) (ScopedTerm (AppTerm (WTerm (MetavarId "X'")) (OTerm (Id "z2")))))))]

-- >>> prune ["y"] (Substitutions [("X", "λ z1 . ( Cons x ) ")], "Cons x")
-- [("X" |-> AbsTerm (PatternVar (Id "z1")) (ScopedTerm (AppTerm (CTerm (ConstructorId "Cons")) (OTerm (Id "x")))))]

-- >>> prune ["a", "b1", "c"] (Substitutions [], "X a b2 c")
-- [("X" |-> AbsTerm (PatternVar (Id "z1")) (ScopedTerm (WTerm (MetavarId "X'"))))]

-- >>> prune ["l1", "l2"] (Substitutions [], "( Snd l3 ) l1")
-- []
