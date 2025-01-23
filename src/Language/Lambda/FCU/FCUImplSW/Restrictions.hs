{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.FCUImplSW.Restrictions where

import Language.Lambda.FCU.FCUImplSW.RTerms (RTerm (..), isRTerm)
import Language.Lambda.FCU.FCUImplSW.Substitutions (Substitutions)
import Language.Lambda.FCU.FCUImplSW.Terms (Id, Term (..))

-- >>> argumentRestriction ["X" :@ "a" :@ "b" :@ "c", "X" :@ "a" :@ "b" :@ "c"]
-- False

-- >>> argumentRestriction ["Cons" :@ "c", "a"]
-- True

argumentRestriction :: [Term] -> Bool
argumentRestriction tn = and [isRTerm t | t <- tn]

-- >>> localRestriction ["X" :@ "a" :@ "b" :@ "c", "X" :@ "a" :@ "b" :@ "c"]
-- False

-- >>> localRestriction ["X" :@ "a" :@ "c", "a"]
-- False

-- >>> localRestriction ["X" :@ "a" :@ "b1" :@ "c", "X" :@ "a" :@ "b2" :@ "c"]
-- True

localRestriction :: [Term] -> Bool
localRestriction tn =
  and
    [ not (isSubset t1 t2) && not (isSubset t2 t1)
    | (t1, i1) <- zip tn [0 ..],
      (t2, i2) <- zip tn [0 ..],
      i1 < i2
    ]
  where
    isSubset :: Term -> Term -> Bool
    isSubset t1 t2 = t1 == t2 || checkSubterm t1 t2

    checkSubterm :: Term -> Term -> Bool
    checkSubterm sub (t1 :@ t2) = checkSubterm sub t1 || checkSubterm sub t2
    checkSubterm sub (x :.: t) = checkSubterm sub t
    checkSubterm sub t = sub == t

-- >>> globalRestriction ["X" :@ "a" :@ "b" :@ "c"] ["X" :@ "a" :@ "b" :@ "c"]
-- True

-- >>> globalRestriction ["X" :@ "a" :@ "c"] ["a"]
-- False

-- >>> globalRestriction ["X" :@ "a" :@ "b1" :@ "c"] ["X" :@ "a" :@ "b2" :@ "c"]
-- True

-- >>> globalRestriction ["a", "b1", "c"] ["c", "b2", "a"]
-- True

globalRestriction :: [Term] -> [Term] -> Bool
globalRestriction sn tm =
  and
    [ not (isStrictSubset t1 t2) && not (isStrictSubset t2 t1)
    | t1 <- sn,
      t2 <- tm
    ]
  where
    isStrictSubset :: Term -> Term -> Bool
    isStrictSubset t1 t2 = checkSubterm t1 t2

    checkSubterm :: Term -> Term -> Bool
    checkSubterm sub (t1 :@ t2) = checkSubterm sub t1 || checkSubterm sub t2
    checkSubterm sub (x :.: t) = checkSubterm sub t
    checkSubterm sub t = sub == t
