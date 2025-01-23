{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.FCUImplSW.Substitutions
  ( Substitutions (..),
    ppSubstitutions,
    devar,
    devarOne,
    mkvars,
    rename,
  )
where

import Language.Lambda.FCU.FCUImplSW.Terms (Id, Term (..))

newtype Substitutions
  = Substitutions [(Id, Term)]
  deriving (Eq)

instance Show Substitutions where
  show :: Substitutions -> String
  show = ppSubstitutions

ppSubstitutions :: Substitutions -> String
ppSubstitutions (Substitutions subs) =
  "[" ++ unwords ["(" ++ x ++ " |-> " ++ show y ++ ")" | (x, y) <- subs] ++ "]"

-- >>>(Substitutions [("x", "Y")])
-- [(x -> Y)]
-- >>> (Substitutions [("x", "Y"), ("z", "Z")])
-- [(x -> Y) (z -> Z)]
-- theta S -> new S
devarOne :: (Id, Term) -> Term -> Term
devarOne (from, to) term =
  case term of
    W x ->
      if x == from
        then to
        else W x
    O x ->
      if x == from
        then to
        else O x
    Constructor x -> Constructor x
    f :@ x -> devarOne (from, to) f :@ devarOne (from, to) x
    x :.: y -> x :.: devarOne (from, to) y

-- >>> devarOne ("x", W "y") (O "x")
-- y
-- >>> devarOne ("x", W "y") (O "z")
-- z
-- >>> devarOne ("X", "Y" :@ "z") ("X" :@ "z")
-- Y (z) (z)
-- >>> devarOne ("X", "Y" :@ "z") ("z" :@ "X")
-- z (Y (z))
-- >>> devarOne ("x", "z") ("Cons" :@ "x" :@ "y")
-- Cons (z) (y)
-- >>> devarOne ("x", "z") ("x" :.: "y")
-- λx . (y)

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

-- >>> devar [("X", "Y" :@ "z")] ("z" :@ "X")
-- z Y z

rename :: Id -> Id -> Term -> Term
rename x x' (O y) = if x == y then O x' else O y
rename x x' (s :@ t) = rename x x' s :@ rename x x' t
rename x x' (y :.: t) = y :.: rename x x' t
rename _ _ t = t

-- >>> rename "x" "y" ("x" :@ "z")
-- y (z)

-- >>> rename "x" "y" ("x" :.: "z")
-- λx . (z)

mkvars :: [Term] -> [Id]
mkvars sn = ["z" ++ show i | i <- [1 .. length sn]]

-- >>> mkvars ["x", "y", "z"]
-- ["z1","z2","z3"]

-- >>> mkvars ["x" :@ "y", "z" :.: "z"]
-- ["z1","z2"]
