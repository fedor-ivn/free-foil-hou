{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Language.Lambda.FCU.Restrictions where

import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.Terms

isRTerm :: Raw.Term -> Bool
isRTerm (Raw.OTerm _) = True
isRTerm (Raw.AppTerm (Raw.CTerm _) t) = isRTerm t
isRTerm _ = False

-- >>> argumentRestriction ["X :@ a :@ b :@ c", "X :@ a :@ b :@ c"]
-- False

-- >>> argumentRestriction ["Cons :@ c", "a"]
-- True

argumentRestriction :: [Raw.Term] -> Bool
argumentRestriction tn = and [isRTerm t | t <- tn]


-- >>> localRestriction ["X :@ a :@ b :@ c", "X :@ a :@ b :@ c"]
-- False

-- >>> localRestriction ["X :@ a :@ c", "a"]
-- False

-- >>> localRestriction ["X :@ a :@ b1 :@ c", "X :@ a :@ b2 :@ c"]
-- True

localRestriction :: [Raw.Term] -> Bool
localRestriction tn =
  and
    [ not (isSubset t1 t2) && not (isSubset t2 t1)
    | (t1, i1) <- zip tn [0 ..],
      (t2, i2) <- zip tn [0 ..],
      i1 < i2
    ]
  where
    isSubset :: Raw.Term -> Raw.Term -> Bool
    isSubset t1 t2 = t1 == t2 || checkSubterm t1 t2

    checkSubterm :: Raw.Term -> Raw.Term -> Bool
    checkSubterm sub (Raw.AppTerm t1 t2) = checkSubterm sub t1 || checkSubterm sub t2
    checkSubterm sub (Raw.AbsTerm x t) = checkSubterm sub t
    checkSubterm sub t = sub == t

-- >>> globalRestriction ["Cons :@ a :@ b :@ c"] ["Cons :@ a :@ b :@ c"]
-- True

-- >>> globalRestriction ["Cons :@ a :@ c"] ["a"]
-- False

-- >>> globalRestriction ["Cons :@ a :@ b1 :@ c"] ["Cons :@ a :@ b2 :@ c"]
-- True

-- >>> globalRestriction ["a", "b1", "c"] ["c", "b2", "a"]
-- False

globalRestriction :: [Raw.Term] -> [Raw.Term] -> Bool
globalRestriction sn tm =
  and
    [ not (isStrictSubset t1 t2) && not (isStrictSubset t2 t1)
    | t1 <- sn,
      t2 <- tm
    ]
  where
    isStrictSubset :: Raw.Term -> Raw.Term -> Bool
    isStrictSubset t1 t2 = checkSubterm t1 t2

    checkSubterm :: Raw.Term -> Raw.Term -> Bool
    checkSubterm sub (Raw.AppTerm t1 t2) = checkSubterm sub t1 || checkSubterm sub t2
    checkSubterm sub (Raw.AbsTerm x t) = checkSubterm sub t
    checkSubterm sub t = sub == t

