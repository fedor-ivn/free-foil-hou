{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification (unify) where

import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm)
import Language.Lambda.FCU.Substitutions (Substitutions (..))
import Language.Lambda.FCU.Terms (Id, Term (..))

------- Unification ----- bvs (th (s,t)) = Q for all, (subs, S)
unify :: [Id] -> (Substitutions, (Term, Term)) -> Maybe Substitutions
unify _ (th, (O x, O y)) = unifyIdent x y th
unify bvs (th, (x :.: s', y :.: t')) = unifyAbstraction x y s' t' bvs th
unify bvs (th, (f :@ x, g :@ y)) = case (f, g) of
  (W _, W _) -> unifyFlexFlex f g (toRTerm x) (toRTerm y) bvs th
  (W _, _) -> unifyFlexRigid bvs (f, toRTerm x, g, y, th)
  (_, W _) -> unifyFlexRigid bvs (g, toRTerm y, f, x, th)
  _ -> unifyFunction f g x y bvs th
unify _ _ = Nothing

-- Helper function to unify identical vars
unifyIdent :: Id -> Id -> Substitutions -> Maybe Substitutions
unifyIdent x y th = if x == y then Just th else Nothing

-- Helper function to unify abstractions
unifyAbstraction :: Id -> Id -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyAbstraction x y s' t' bvs th =
  if x == y
    then unify (bvs ++ [x]) (th, (s', t'))
    else Nothing

-- Helper function to unify functions
unifyFunction :: Term -> Term -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyFunction f g x y bvs th =
  if f == g
    then unify bvs (th, (x, y))
    else Nothing

-- Helper function to unify flexible and rigid terms
unifyFlexRigid :: [Id] -> (Term, RTerm, Term, Term, Substitutions) -> Maybe Substitutions
unifyFlexRigid bvs (_F, ym, f, x, th)
  | occ _F th x = error "Occurances check failed"
  | otherwise = error "unifyFlexRigid not implemented"

occ :: Term -> Substitutions -> Term -> Bool
occ _ _ _ = error "occ not implemented"

-- Helper function to unify flexible and flexible terms
unifyFlexFlex :: Term -> Term -> RTerm -> RTerm -> [Id] -> Substitutions -> Maybe Substitutions
unifyFlexFlex _ _ _ _ _ _ = error "unifyFlexFlex not implemented"

-- >>> unify [] (Substitutions [], ("x", "x"))
-- Just []

-- >>> unify [] (Substitutions [], ("x", "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "z" :.: "z"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "x" :.: "y"))
-- Just []

-- >>> unify [] (Substitutions [], ("Fst" :@ "x", "Fst" :@ "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("Cons" :@ "x" :@ "y", "Cons" :@ "x" :@ "y"))
-- Just []
