{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Terms where

import Data.Char (isUpper)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.String (IsString (..))

-- $setup
-- >>> :set -XOverloadedStrings

type Id = String

------- Metavars - Bound vars - Functions -- Application - Abstraction
data Term
  = W Id
  | O Id
  | Constructor Id
  | Term :@ Term
  | Id :.: Term
  deriving (Eq)

instance IsString Term where
  fromString :: String -> Term
  fromString s@(c : _)
    | isUpper c && length s == 1 = W s
    | isUpper c = Constructor s
    | otherwise = O s
  fromString _ = error "empty atoms are not allowed"

instance Show Term where
  show :: Term -> String
  show = ppTerm

ppTerm :: Term -> String
ppTerm (W x) = x
ppTerm (O x) = x
ppTerm (Constructor x) = x
ppTerm (x :.: y) = "λ" ++ x ++ " . (" ++ ppTerm y ++ ")"
ppTerm (f :@ x) = case f of
  _ :.: _ -> "(" ++ ppTerm f ++ ") " ++ "(" ++ ppTerm x ++ ")"
  _ :@ _ -> "(" ++ ppTerm f ++ ") " ++ "(" ++ ppTerm x ++ ")"
  _ -> ppTerm f ++ " " ++ ppTerm x

-- >>> "x" :: Term
-- x
-- >>> "X" :: Term
-- X
-- >>> "Cons" :: Term
-- Cons
-- >>> "x" :.: ("Cons" :@ "x" :@ "y") :: Term
-- λx . ((Cons x) y)

isMeta :: Term -> Bool
isMeta (W _) = True
isMeta _ = False

-- >>> isMeta "x"
-- False
-- >>> isMeta "X"
-- True
-- >>> isMeta "Cons"
-- False
-- >>> isMeta ("x" :.: ("Cons" :@ "x" :@ "y"))
-- False

subset :: [Term] -> [Term] -> Bool
subset sm tn = all (`elem` tn) sm

-- >>> subset ["x", "y"] ["x", "y", "z"]
-- True

-- >>> subset ["x", "y", "z"] ["x", "y"]
-- False

-- | Apply a permutation to two list of arguments
permutate :: [Id] -> [Term] -> [Term] -> [Id]
permutate zs as bs = [zs !! i | b <- bs, i <- maybeToList $ elemIndex b as]

-- >>> permutate ["z1", "z2"] ["a", "b", "c"] ["b", "a"]
-- ["z2","z1"]

-- >>> permutate ["z1", "z2", "z3"] ["a", "b", "c"] ["b", "a", "c"]
-- ["z2","z1","z3"]

-- >>> permutate ["z1", "z2", "z3", "z4"] ["a", "b", "c"] ["b", "a", "d", "c"]
-- ["z2","z1","z3"]

newMetaVarId :: Id -> Id
newMetaVarId = (++ "'")

-- >>> newMetaVarId "X"
-- "X'"
