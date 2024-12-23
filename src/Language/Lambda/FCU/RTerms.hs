{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.RTerms (RTerm (..), toRTerm, toTerm, isRTerm) where

import Language.Lambda.FCU.Terms (Id, Term (..))

data RTerm = RO Id | RConstructor Id | RApp RTerm RTerm
  deriving (Eq, Show)

-- >>> toRTerm ("Cons" :@ "x" :@ "y")
-- RApp (RApp (RConstructor "Cons") (RO "x")) (RO "y")

-- >>> toRTerm ("Cons" :@ "x" :@ "y" :@ "z")
-- RApp (RApp (RApp (RConstructor "Cons") (RO "x")) (RO "y")) (RO "z")

-- >>> toRTerm ("Cons" :@ "x" :@ ("Cons" :@ "y"))
-- RApp (RApp (RConstructor "Cons") (RO "x")) (RApp (RConstructor "Cons") (RO "y"))

-- >>> toRTerm ("Cons" :@ "X" :@ "Y")
-- Metavars are not allowed in Restricted Terms

-- >>> toRTerm ("Cons" :@ "x" :@ "Fst")
-- Constructor terms should have > 0 arguments

-- | Convert a Term to a Restricted Term
toRTerm :: Term -> RTerm
toRTerm (O x) = RO x
toRTerm (Constructor x :@ y) = RApp (RConstructor x) (toRTerm y)
toRTerm (x :@ y) = RApp (toRTerm x) (toRTerm y)
toRTerm (W _) = error "Metavars are not allowed in Restricted Terms"
toRTerm (Constructor _) = error "Constructor terms should have > 0 arguments"
toRTerm (_ :.: _) = error "Abstraction is not allowed in Restricted Terms"

-- >>> isRTerm ("Cons" :@ "x" :@ "y")
-- True

-- >>> isRTerm "X"
-- False

-- >>> isRTerm ("Cons" :@ "x" :@ "y" :@ "X")
-- False

-- >>> isRTerm ("l1" :.: "x" :@ "y")
-- False

-- | Check if a Term is a Restricted Term
isRTerm :: Term -> Bool
isRTerm (O _) = True
isRTerm (Constructor _ :@ y) = isRTerm y
isRTerm (x :@ y) = isRTerm x && isRTerm y
isRTerm _ = False

-- >>> toTerm (RApp (RApp (RConstructor "Cons") (RO "x")) (RO "y"))
-- Cons (x) (y)

-- | Convert a Restricted Term to a Term
toTerm :: RTerm -> Term
toTerm (RO x) = O x
toTerm (RConstructor x) = Constructor x
toTerm (RApp f b) = toTerm f :@ toTerm b
