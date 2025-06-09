{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification where

import Language.Lambda.FCU.Discharge (discharge)
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.Prune ( abst, hnf, eqsel, prune, selectzrk )
import Language.Lambda.FCU.Restrictions
  ( argumentRestriction,
    globalRestriction,
    localRestriction,
  )
import Language.Lambda.FCU.Strip (strip, unstrip)
import Language.Lambda.FCU.Substitutions (Substitutions (Substitutions), combineSubstitutions, devar, mkvars, rename)
import Language.Lambda.FCU.Terms (newMetaVarId, permutate)

-- >>> unify [] (Substitutions [], ("x", "x"))
-- []

-- >>> unify [] (Substitutions [], ("x", "y"))
-- OTerm (Id "x") and OTerm (Id "y") are not unifiable

-- >>> unify [] (Substitutions [], ("λ l1 . ( λ x . l1 )", "λ l2 . ( λ x . l2)" ))
-- []

-- >>> unify [] (Substitutions [], ("( Cons y ) z", "( Cons y ) z"))
-- []

-- >>> unify [] (Substitutions [], ("( Cons y ) z", "( Cons y ) w"))
-- OTerm (Id "z") and OTerm (Id "w") are not unifiable

-- >>> unify [] (Substitutions [], ("X y", "Cons x"))
-- [("X" |-> λz1 . (Cons x))]

-- >>> unify [] (Substitutions [], ("X y", "(Cons x) y"))
-- [("X" |-> λz1 . ((Cons x) z1))]

-- >>> unify [] (Substitutions [], ("(X y) x", "(Cons x) ((Cons z) x)"))
-- [("X" |-> λz1 . λz2 . ((Cons z2) ((Cons z) z2)))]

-- >>> unify [] (Substitutions [], ("λ l1 . (λ l2 . ((X (Fst l1)) (Fst (Snd l2))))", "λ l1 . (λ l2 . (Snd ((Y (Fst l2)) (Fst l1))))"))
-- [("Y" |-> λz1 . λz2 . (Y' z2)) ("X" |-> λz1 . λz2 . (Snd (Y' z1)))]

-- >>> unify [] (Substitutions [], ("(X a) c", "((X a) b) c"))
-- Different argument lists lengths in (4) rule

-- >>> unify [] (Substitutions [], ("((X a) b1) c", "((X a) b2) c"))
-- [("X" |-> λz1 . λz2 . λz3 . ((X' z1) z3))]

-- >>> unify [] (Substitutions [], ("((X c) b) a", "((X a) b) c"))
-- [("X" |-> λz1 . λz2 . λz3 . (X' z2))]

-- >>> unify [] (Substitutions [], ("((X a) b1) c", "((Y c) b2) a"))
-- Global restriction fail at flexflex case

-- >>> unify [] (Substitutions [], ("((X a) b1) c", "((Y (Cons c)) b2) a"))
-- Global restriction fail at flexflex case

unify :: [(Char, Raw.Id)] -> (Substitutions, (Raw.Term, Raw.Term)) -> Substitutions
unify bvs (th, (s, t)) = case (devar th s, devar th t) of
  (Raw.AbsTerm (Raw.PatternVar x) (Raw.ScopedTerm sId), Raw.AbsTerm (Raw.PatternVar x') (Raw.ScopedTerm tId)) ->
    let renamed = if x == x' then tId else rename x' x tId
        newBvs = ('B', x) : bvs
     in unify newBvs (th, (sId, renamed))
  (s', t') -> cases bvs (th, (s', t'))

cases :: [(Char, Raw.Id)] -> (Substitutions, (Raw.Term, Raw.Term)) -> Substitutions
cases bvs (th, (s, t)) = case (strip s, strip t) of
  ((Raw.WTerm _F, sn), (Raw.WTerm _G, zm)) -> caseFlexFlex bvs (_F, sn, _G, zm, th)
  ((Raw.WTerm _F, sn), (Raw.CTerm _, _)) -> caseFlexRigid bvs (_F, sn, t, th)
  ((Raw.CTerm _, _), (Raw.WTerm _F, sn)) -> caseFlexRigid bvs (_F, sn, s, th)
  ((a, []), (b, [])) -> if a == b then th else error (show a ++ " and " ++ show b ++ " are not unifiable")
  ((a, sn), (b, tm)) -> caseRigidRigid bvs (a, sn, b, tm, th)

caseRigidRigid :: [(Char, Raw.Id)] -> (Raw.Term, [Raw.Term], Raw.Term, [Raw.Term], Substitutions) -> Substitutions
caseRigidRigid bvs (a, sn, b, tm, th) = case (a, b) of
  (Raw.OTerm x, Raw.OTerm y) -> applicableCase (x, y)
  (Raw.CTerm (Raw.ConstructorId x), Raw.CTerm (Raw.ConstructorId y)) -> applicableCase (Raw.Id x, Raw.Id y)
  _ -> error (show a ++ " and " ++ show b ++ " are not unifiable")
  where
    applicableCase :: (Raw.Id, Raw.Id) -> Substitutions
    applicableCase (x, y) =
      if x == y && length sn == length tm
        then foldl (\th' (s, t) -> unify bvs (th', (s, t))) th (zip sn tm)
        else error "Different function heads or argument lists lengths in (2) rule"

caseFlexRigid :: [(Char, Raw.Id)] -> (Raw.MetavarId, [Raw.Term], Raw.Term, Substitutions) -> Substitutions
caseFlexRigid _ (Raw.MetavarId _F, tn, s, rho) -- s is rigid
  | not (argumentRestriction tn) = error "Argument restriction fail at flexrigid case"
  | not (localRestriction tn) = error "Local restriction fail at flexrigid case"
  | otherwise = combineSubstitutions (combineSubstitutions rho pruningResult) newMetavarSubs
  where
    zn = mkvars tn
    pruningResult = prune tn (rho, s)
    newMetavarSubs = Substitutions [(_F, abst (zn, discharge (zip tn zn) (devar pruningResult s)))]

caseFlexFlex :: [(Char, Raw.Id)] -> (Raw.MetavarId, [Raw.Term], Raw.MetavarId, [Raw.Term], Substitutions) -> Substitutions
caseFlexFlex bvs (_F, sn, _G, tm, th)
  | not (argumentRestriction sn) = error "Argument restriction fail at flexflex case"
  | not (argumentRestriction tm) = error "Argument restriction fail at flexflex case"
  | not (localRestriction sn) = error "Local restriction fail at flexflex case"
  | not (localRestriction tm) = error "Local restriction fail at flexflex case"
  | _F == _G = caseFlexFlexSame bvs (_F, sn, tm, th)
  | otherwise = caseFlexFlexDiff bvs (_F, _G, sn, tm, th)

caseFlexFlexSame :: [(Char, Raw.Id)] -> (Raw.MetavarId, [Raw.Term], [Raw.Term], Substitutions) -> Substitutions
caseFlexFlexSame _ (Raw.MetavarId _F, sn, tn, th)
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | sn == tn = error "Same argument lists in (4) rule"
  | otherwise = combineSubstitutions th newMetavarSubs
  where
    vsm = mkvars sn
    newMetavarSubs = Substitutions [(_F, hnf (vsm, Raw.WTerm (newMetaVarId (Raw.MetavarId _F)), selectzrk vsm tn sn))]

caseFlexFlexDiff :: [(Char, Raw.Id)] -> (Raw.MetavarId, Raw.MetavarId, [Raw.Term], [Raw.Term], Substitutions) -> Substitutions
caseFlexFlexDiff _ (_F, _G, sn, tm, th)
  | not (globalRestriction sn tm) = error "Global restriction fail at flexflex case"
  | otherwise = combineSubstitutions (combineSubstitutions th pruningResultLeft) (combineSubstitutions pruningResultRight metavarSubs)
  where
    t = unstrip (Raw.WTerm _G, tm)
    pruningResultLeft = prune sn (th, t)
    s = unstrip (Raw.WTerm _F, sn)
    pruningResultRight = prune tm (th, s)
    tmnew = strip (devar pruningResultLeft t)
    snnew = strip (devar pruningResultRight s)
    vsm = mkvars (snd tmnew)
    Raw.MetavarId newMetavar = newMetaVarId _G
    metavarSubs = Substitutions [(newMetavar, hnf (vsm, Raw.WTerm (newMetaVarId _F), permutate vsm (snd tmnew) (snd snnew)))]
