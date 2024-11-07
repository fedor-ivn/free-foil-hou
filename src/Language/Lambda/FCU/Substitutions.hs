{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Substitutions
  ( Substitutions(..)
  , ppSubstitutions
  , devar
  , devarOne
  ) where

import           Language.Lambda.FCU.Terms (Id, Term (..))

newtype Substitutions =
  Substitutions [(Id, Term)]
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
devar :: [(Id, Term)] -> Term -> Term
devar subs term = foldr devarOne term subs
-- >>> devar [("x", W "y")] (O "x")
-- y
-- >>> devar [("x", W "y")] (O "z")
-- z
-- >>> devar [("X", "Y" :@ "z"), ("z", "fst" :@ "a")] ("X" :@ "z")
-- Y (z) (fst (a))
