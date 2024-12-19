{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Restrictions where

import Language.Lambda.FCU.RTerms (RTerm (..), isRTerm)
import Language.Lambda.FCU.Substitutions (Substitutions)
import Language.Lambda.FCU.Terms (Id, Term (..))

argumentRestriction :: [Term] -> Bool
argumentRestriction tn = and [isRTerm t | t <- tn]

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
    isSubset (t1 :@ t2) (s1 :@ s2) = isSubset t1 s1 && isSubset t2 s2
    isSubset (x :.: t) (y :.: s) = x == y && isSubset t s
    isSubset x y = x == y

globalRestriction :: [Term] -> [Term] -> Bool
globalRestriction sn tm = True -- TODO
