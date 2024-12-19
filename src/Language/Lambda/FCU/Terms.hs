{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Terms where

import Data.Char (isUpper)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)
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

matchTermLists :: [Id] -> [Term] -> [Term] -> [Id]
matchTermLists vsm tn sm =
  [v | (v, t) <- zip vsm tn, t `elem` sm]

-- | Untested
permutate :: [String] -> [String] -> [String] -> [String]
permutate zs as bs = [zs !! i | b <- bs, let i = fromMaybe 0 $ elemIndex b as]

-- >>> permutate ["z1", "z2", "z3"] ["a", "b", "c"] ["b", "a"]
-- ["z2","z1"]

applyTerms :: Term -> [Term] -> Term
applyTerms = foldl (:@)
