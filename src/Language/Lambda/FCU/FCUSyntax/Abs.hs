-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Language.Lambda.FCU.FCUSyntax.Abs where

newtype Id = Id String
  deriving (Eq, Ord, Show, Read)

newtype MetavarId = MetavarId String
  deriving (Eq, Ord, Show, Read)

newtype ConstructorId = ConstructorId String
  deriving (Eq, Ord, Show, Read)

data Term
    = WTerm MetavarId
    | OTerm Id
    | CTerm ConstructorId
    | AppTerm Term Term
    | AbsTerm Id Term
  deriving (Eq, Ord, Show, Read)

