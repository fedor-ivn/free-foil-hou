{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification
  ( unify,
  )
where

import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm)
import Language.Lambda.FCU.Terms (Id, Term (..))

betaReduceOnce :: Term -> Term
betaReduceOnce ((x :.: t1) :@ t2) = betaReduceOnce (devar [(x, t2)] t1)
betaReduceOnce (t1 :@ t2) = betaReduceOnce t1 :@ betaReduceOnce t2
betaReduceOnce (x :.: t) = x :.: betaReduceOnce t
betaReduceOnce t = t

-- >>> betaReduceOnce ("x" :.: "y" :@ "z")
-- y

-- >>> betaReduceOnce ("x" :.: "y" :@ "z" :@ "w")
-- y w

-- >>> betaReduceOnce (("x" :.: "x" :@ "z") :@ "w")
-- z w

-- λz1 . (λz2 . (Snd ((λz1 . (λz2 . (Y' z2))) (Fst l2)) (z1)))

betaReduce :: Term -> Term
betaReduce t = case betaReduceOnce t of
  t' -> if t == t' then t else betaReduce t'

-- >>> betaReduce ("z1" :.: ("z2" :.: ("Snd" :@ (("k1" :.: ("k2" :.: ("Y" :@ "k2" ))) :@ ("Fst" :@ "l2") :@ "z1"))))
-- λz1 . (λz2 . (Snd Y z1))

applySubstitution :: [(Id, Term)] -> Term -> Term
applySubstitution th (O x) = case lookup x th of
  Just t -> applySubstitution th t
  Nothing -> O x
applySubstitution th (W x) = case lookup x th of
  Just t -> applySubstitution th t
  Nothing -> W x
applySubstitution th (s :@ t) = applySubstitution th s :@ applySubstitution th t
applySubstitution th (x :.: t) = x :.: applySubstitution th t
applySubstitution _ t@(Constructor _) = t

devar :: [(Id, Term)] -> Term -> Term
devar th s = betaReduce (applySubstitution th s)

-- >>> devar [("x", "Y")] (O "x")
-- Y

-- >>> devar [("x", "Y")] (O "z")
-- z

-- >>> devar [("X", "Y" :@ "z")] ("X" :@ "z")
-- (Y z) z

strip :: Term -> (Term, [Term])
strip (t1 :@ t2) =
  let (h, rest) = strip t1
   in (h, rest ++ [t2])
strip t = (t, [])

--- >>> strip ("X" :@ "y" :@ "z")
-- (X,[y,z])

--- >>> strip ("Cons" :@ "x" :@ ("y" :@ ("z" :.: "z")))
-- (Cons,[x,y λz . (z)])

rename :: Id -> Id -> Term -> Term
rename x x' (O y) = if x == y then O x' else O y
rename x x' (s :@ t) = rename x x' s :@ rename x x' t
rename x x' (y :.: t) = y :.: rename x x' t
rename _ _ t = t

-- >>> rename "x" "y" ("x" :@ "z")
-- y (z)

-- >>> rename "x" "y" ("x" :.: "z")
-- λx . (z)

occ :: Id -> [(Id, Term)] -> Term -> Bool
occ _ _ _ = False

mkvars :: [Term] -> [Id]
mkvars sn = ["z" ++ show i | i <- [1 .. length sn]]

-- >>> mkvars ["x", "y", "z"]
-- ["z1","z2","z3"]

-- >>> mkvars ["x" :@ "y", "z" :.: "z"]
-- ["z1","z2"]

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

discharge :: [(Term, Id)] -> Term -> Term
discharge th t = case lookup t th of
  Just y -> O y
  Nothing -> case t of
    (x :.: t1) -> x :.: discharge th t1
    (t1 :@ t2) -> discharge th t1 :@ discharge th t2
    t -> t

-- >>> discharge [("Cons" :@ "x" :@ "y", "z")] ("Cons" :@ "x" :@ "y")
-- z

-- >>> discharge [("Fst" :@ "x", "z")] ("Cons" :@ ("Fst" :@ "x") :@ "w")
-- (Cons (z)) (w)

foldlN :: (([(Id, Term)], Term) -> [(Id, Term)]) -> ([(Id, Term)], [Term]) -> [(Id, Term)]
foldlN _ (rho, []) = rho
foldlN f (rho, t : ts) = foldlN f (f (rho, t), ts)

subset :: [Term] -> [Term] -> Bool
subset sm tn = all (`elem` tn) sm

-- >>> subset ["x", "y"] ["x", "y", "z"]
-- True

-- >>> subset ["x", "y", "z"] ["x", "y"]
-- False

eqsel :: [Id] -> [Term] -> [Term] -> [Id]
eqsel vsm tn sm =
    [v | (v, t) <- zip vsm tn, t `notElem` sm]

-- >>> eqsel ["z1", "z2"] ["x", "y", "z"] ["x", "y", "z"]  
-- []

-- >>> eqsel ["z1", "z2"] ["x", "y"] ["x", "w"]
-- ["z2"]

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
         in (_W, hnf (vsm, _W ++ "'", eqsel vsm tn sm)) : rho
  _ -> error "Prune: unexpected case"

-- >>> prune ["x", "y"] ([], "Cons" :@ "x" :@ "y")
-- []

-- >>> prune ["x", "q", "y"] ([], "X" :@ ("Snd" :@ "x") :@ "q")
-- [("X",λz1 . (λz2 . (X' z1)))]

-- >>> prune ["y"] ([("X", "z1" :.: ("Cons" :@ "x"))], "Cons" :@ "x")
-- [("X",λz1 . (Cons x))]

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
  | occ _F rho s = error "Restriction fail at flexrigid case"
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
caseFlexFlex bvs (_F, sn, _G, tm, th) = error "FlexFlex case not implemented"
