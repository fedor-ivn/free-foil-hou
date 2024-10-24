{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.RTerms (RTerm (..), toRTerm, toTerm) where

import Language.Lambda.FCU.Terms (Id, Term (..))

data RTerm = RO Id | RConstructor Id | RApp RTerm RTerm
  deriving (Eq, Show)

toRTerm :: Term -> RTerm
toRTerm (O x) = RO x
toRTerm (Constructor x :@ y) = RApp (RConstructor x) (toRTerm y)
toRTerm (x :@ y) = RApp (toRTerm x) (toRTerm y)
toRTerm (W _) = error "Metavars are not allowed in Restricted Terms"
toRTerm (Constructor _) = error "Constructor terms should have > 0 arguments"
toRTerm (_ :.: _) = error "Abstraction is not allowed in Restricted Terms"

toTerm :: RTerm -> Term
toTerm (RO x) = O x
toTerm (RConstructor x) = Constructor x
toTerm (RApp f b) = toTerm f :@ toTerm b

-- >>> toRTerm ("Cons" :@ "x" :@ "y")
-- RApp (RApp (RConstructor "Cons") (RO "x")) (RO "y")
