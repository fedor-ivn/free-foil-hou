{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Lambda.FCU.Substitutions where

import Language.Lambda.FCU.Terms (Term (..))

newtype Substitutions = Substitutions [(Term, Term)]

-- theta S -> new S
replaceTerm :: (Term, Term) -> Term -> Term
replaceTerm (from, to) term = case term of
  W x -> if W x == from then to else W x
  O x -> if O x == from then to else O x
  Constructor x -> Constructor x
  f :@ x -> replaceTerm (from, to) f :@ replaceTerm (from, to) x
  x :.: y -> x :.: replaceTerm (from, to) y

applySubstitutions :: Substitutions -> Term -> Term
applySubstitutions (Substitutions subs) term = foldr replaceTerm term subs

-- >>> applySubstitutions (Substitutions [(O "x", W "y")]) (O "x")
-- y

-- >>> applySubstitutions (Substitutions [(O "x", W "y")]) (O "z")
-- z

-- >>> applySubstitutions (Substitutions [("X", "Y" :@ "z"), ("z", "fst" :@ "a")]) ("X" :@ "z")
-- Y (z) (fst (a))
