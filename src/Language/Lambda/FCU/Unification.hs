{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification
  ( unify,
  )
where

import Language.Lambda.FCU.Discharge (discharge)
import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm)
import Language.Lambda.FCU.Restrictions (argumentRestriction, localRestriction)
import Language.Lambda.FCU.Substitutions (devar, rename, mkvars)
import Language.Lambda.FCU.Terms (Id, Term (..), subset)
import Language.Lambda.FCU.Strip (strip)
import Language.Lambda.FCU.Prune

------- Unification ----- bvs (th (s,t)) = Q, (theta, S)
unify :: [(Char, Id)] -> ([(Id, Term)], (Term, Term)) -> [(Id, Term)]
unify bvs (th, (s, t)) = case (devar th s, devar th t) of
  (x :.: s, x' :.: t) ->
    let renamed = if x == x' then t else rename x' x t
        newBvs = ('B', x) : bvs
     in unify newBvs (th, (s, renamed))
  (s', t') -> cases bvs (th, (s', t'))

-- >>> unify [] ([], ("x", "x"))
-- []

-- >>> unify [] ([], ("x", "y"))
-- x and y are not unifiable

-- >>> unify [] ([], ("l1" :.: ("x" :.: "l1"), "l2" :.: ("x" :.: "l2")))
-- []

-- >>> unify [] ([], ("Cons" :@ "y" :@ "z", "Cons" :@ "y" :@ "z"))
-- []

-- >>> unify [] ([], ("Cons" :@ "y" :@ "z", "Cons" :@ "y" :@ "w"))
-- z and w are not unifiable

-- >>> unify [] ([], ("X" :@ "y", "Cons" :@ "x"))
-- [("X",λz1 . (Cons x))]

-- >>> unify [] ([], ("X" :@ "y", "Cons" :@ "x" :@ "y"))
-- [("X",λz1 . ((Cons x) (z1)))]

-- >>> unify [] ([], ("X" :@ "y" :@ "x", "Cons" :@ "x" :@ ("Cons" :@ "z" :@ "x")))
-- [("X",λz1 . (λz2 . ((Cons z2) ((Cons z) (z2)))))]

-- Example from the paper
-- >>> unify [] ([], ("l1" :.: ("l2" :.: ("X" :@ ("Fst" :@ "l1") :@ ("Fst" :@ ("Snd" :@ "l2")))), "l1" :.: ("l2" :.: ("Snd" :@ (("Y" :@ ("Fst" :@ "l2")) :@ ("Fst" :@ "l1"))))))
-- [("Y",λz1 . (λz2 . (Y' z2))),("X",λz1 . (λz2 . (Snd Y' z1)))]

cases :: [(Char, Id)] -> ([(Id, Term)], (Term, Term)) -> [(Id, Term)]
cases bvs (th, (s, t)) = case (strip s, strip t) of
  ((W _F, sn), (W _G, zm)) -> caseFlexFlex bvs (_F, sn, _G, zm, th)
  ((W _F, sn), (Constructor _, _)) -> caseFlexRigid bvs (_F, sn, t, th)
  ((Constructor _, _), (W _F, sn)) -> caseFlexRigid bvs (_F, sn, s, th)
  ((a, []), (b, [])) -> if a == b then th else error (show a ++ " and " ++ show b ++ " are not unifiable")
  ((a, sn), (b, tm)) -> caseRigidRigid bvs (a, sn, b, tm, th)

caseFlexRigid :: [(Char, Id)] -> (Id, [Term], Term, [(Id, Term)]) -> [(Id, Term)]
caseFlexRigid bvs (_F, tn, s, rho) -- s is rigid
  | not (argumentRestriction tn) = error "Argument restriction fail at flexrigid case"
  | not (localRestriction tn) = error "Local restriction fail at flexrigid case"
  | otherwise =
      let zn = mkvars tn
          pruningResult = prune tn (rho, s)
          pruned = devar pruningResult s
          discharged = discharge (zip tn zn) pruned
          theta = (_F, abst (zn, discharged))
          substitutions = pruningResult ++ [theta] ++ rho
       in substitutions

caseRigidRigid :: [(Char, Id)] -> (Term, [Term], Term, [Term], [(Id, Term)]) -> [(Id, Term)]
caseRigidRigid bvs (a, sn, b, tm, th) = case (a, b) of
  (O x, O y) -> applicableCase bvs (x, y, sn, tm, th)
  (Constructor x, Constructor y) -> applicableCase bvs (x, y, sn, tm, th)
  _ -> error (show a ++ " and " ++ show b ++ " are not unifiable")
  where
    applicableCase :: [(Char, Id)] -> (Id, Id, [Term], [Term], [(Id, Term)]) -> [(Id, Term)]
    applicableCase bvs (x, y, sn, tm, th) =
      if x == y && length sn == length tm
        then foldl (\th' (s, t) -> unify bvs (th', (s, t))) th (zip sn tm)
        else error "Different function heads or argument lists lengths in (2) rule"

caseFlexFlex :: [(Char, Id)] -> (Id, [Term], Id, [Term], [(Id, Term)]) -> [(Id, Term)]
caseFlexFlex bvs (_F, sn, _G, tm, th)
  | not (argumentRestriction sn) = error "Argument restriction fail at flexflex case"
  | not (argumentRestriction tm) = error "Argument restriction fail at flexflex case"
  | not (localRestriction sn) = error "Local restriction fail at flexflex case"
  | not (localRestriction tm) = error "Local restriction fail at flexflex case"
  | _F == _G = caseFlexFlexSame bvs (_F, sn, tm, th)
  | otherwise = error "Not implemented yet"


caseFlexFlexSame :: [(Char, Id)] -> (Id, [Term], [Term], [(Id, Term)]) -> [(Id, Term)]
caseFlexFlexSame bvs (_F, sn, tn, th)
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | sn == tn = error "Same argument lists in (4) rule"
  | otherwise = error "Not implemented yet"
