{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lambda.FCU.Substitutions (
  Substitutions(..),
  ppSubstitutions,
  replaceTerm,
  applySubstitutions
) where

import Language.Lambda.FCU.Terms (Id, Term (..))

newtype Substitutions = Substitutions [(Id, Term)]
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
replaceTerm :: (Id, Term) -> Term -> Term
replaceTerm (from, to) term = case term of
  W x -> if x == from then to else W x
  O x -> if x == from then to else O x
  Constructor x -> Constructor x
  f :@ x -> replaceTerm (from, to) f :@ replaceTerm (from, to) x
  x :.: y -> x :.: replaceTerm (from, to) y

applySubstitutions :: Substitutions -> Term -> Term
applySubstitutions (Substitutions subs) term = foldr replaceTerm term subs

-- >>> applySubstitutions (Substitutions [("x", W "y")]) (O "x")
-- y

-- >>> applySubstitutions (Substitutions [("x", W "y")]) (O "z")
-- z

-- >>> applySubstitutions (Substitutions [("X", "Y" :@ "z"), ("z", "fst" :@ "a")]) ("X" :@ "z")
-- Y (z) (fst (a))
