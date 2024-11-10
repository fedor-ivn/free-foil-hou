-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Syntax.

module Language.Lambda.Syntax.Abs where

import Prelude (String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Program = AProgram [Command]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Command = CommandCompute Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Term
    = Lam Pattern Type ScopedTerm
    | Let Pattern Term ScopedTerm
    | App Term Term
    | Var VarIdent
    | MetaVar MetaVarIdent [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ScopedTerm = AScopedTerm Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Pattern = APattern VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Binder = ABinder VarIdent Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data MetaSubst = AMetaSubst MetaVarIdent [Binder] ScopedTerm
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data UnificationConstraint
    = AUnificationConstraint [Binder] ScopedTerm ScopedTerm
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Type = Fun Type Type | Base VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype VarIdent = VarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype MetaVarIdent = MetaVarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

