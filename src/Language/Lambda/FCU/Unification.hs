{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Language.Lambda.FCU.Unification where

import GHC.Generics qualified as Raw
import Language.Lambda.FCU.Discharge (discharge)
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.Prune
import Language.Lambda.FCU.Prune (abst, prune)
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
-- x and y are not unifiable

-- >>> unify [] (Substitutions [], ("l1 :.: ( x :.: l1 )", "l2 :.: ( x :.: l2)" ))
-- []

-- >>> unify [] (Substitutions [], ("( Cons :@ y ) :@ z", "( Cons :@ y ) :@ z"))
-- []

-- >>> unify [] (Substitutions [], ("( Cons :@ y ) :@ z", "( Cons :@ y ) :@ w"))
-- z and w are not unifiable

-- >>> unify [] (Substitutions [], ("X :@ y", "Cons :@ x"))
-- [("X" |-> λz1 . (Cons x))]


unify :: [(Char, Raw.Id)] -> (Substitutions, (Raw.Term, Raw.Term)) -> Substitutions
unify bvs (th, (s, t)) = case (devar th s, devar th t) of
  (Raw.AbsTerm x s, Raw.AbsTerm x' t) ->
    let renamed = if x == x' then t else rename x' x t
        newBvs = ('B', x) : bvs
     in unify newBvs (th, (s, renamed))
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
  (Raw.OTerm x, Raw.OTerm y) -> applicableCase bvs (x, y, sn, tm, th)
  (Raw.CTerm (Raw.ConstructorId x), Raw.CTerm (Raw.ConstructorId y)) -> applicableCase bvs (Raw.Id x, Raw.Id y, sn, tm, th)
  _ -> error (show a ++ " and " ++ show b ++ " are not unifiable")
  where
    applicableCase :: [(Char, Raw.Id)] -> (Raw.Id, Raw.Id, [Raw.Term], [Raw.Term], Substitutions) -> Substitutions
    applicableCase bvs (x, y, sn, tm, th) =
      if x == y && length sn == length tm
        then foldl (\th' (s, t) -> unify bvs (th', (s, t))) th (zip sn tm)
        else error "Different function heads or argument lists lengths in (2) rule"

caseFlexRigid :: [(Char, Raw.Id)] -> (Raw.MetavarId, [Raw.Term], Raw.Term, Substitutions) -> Substitutions
caseFlexRigid bvs ((Raw.MetavarId _F), tn, s, rho) -- s is rigid
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
caseFlexFlexSame bvs ((Raw.MetavarId _F), sn, tn, th)
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | sn == tn = error "Same argument lists in (4) rule"
  | otherwise = combineSubstitutions th newMetavarSubs
  where
    vsm = mkvars sn
    newMetavarSubs = Substitutions [(_F, hnf (vsm, _F ++ "'", eqsel vsm tn sn))]

caseFlexFlexDiff :: [(Char, Raw.Id)] -> (Raw.MetavarId, Raw.MetavarId, [Raw.Term], [Raw.Term], Substitutions) -> Substitutions
caseFlexFlexDiff bvs (_F, _G, sn, tm, th)
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
    metavarSubs = Substitutions [(newMetaVarId _G, hnf (vsm, newMetaVarId _F, permutate vsm (snd tmnew) (snd snnew)))]
