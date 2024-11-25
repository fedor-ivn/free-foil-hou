{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Lambda.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pCommand
  , pListCommand
  , pTerm
  , pTerm1
  , pTerm2
  , pListTerm
  , pScopedTerm
  , pPattern
  , pBinder
  , pListBinder
  , pMetaSubst
  , pUnificationConstraint
  , pListVarIdent
  , pType
  , pType1
  ) where

import Prelude

import qualified Language.Lambda.Syntax.Abs
import Language.Lambda.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn19 (Language.Lambda.Syntax.Abs.VarIdent)
        | HappyAbsSyn20 (Language.Lambda.Syntax.Abs.MetaVarIdent)
        | HappyAbsSyn21 (Language.Lambda.Syntax.Abs.Program)
        | HappyAbsSyn22 (Language.Lambda.Syntax.Abs.Command)
        | HappyAbsSyn23 ([Language.Lambda.Syntax.Abs.Command])
        | HappyAbsSyn24 (Language.Lambda.Syntax.Abs.Term)
        | HappyAbsSyn27 ([Language.Lambda.Syntax.Abs.Term])
        | HappyAbsSyn28 (Language.Lambda.Syntax.Abs.ScopedTerm)
        | HappyAbsSyn29 (Language.Lambda.Syntax.Abs.Pattern)
        | HappyAbsSyn30 (Language.Lambda.Syntax.Abs.Binder)
        | HappyAbsSyn31 ([Language.Lambda.Syntax.Abs.Binder])
        | HappyAbsSyn32 (Language.Lambda.Syntax.Abs.MetaSubst)
        | HappyAbsSyn33 (Language.Lambda.Syntax.Abs.UnificationConstraint)
        | HappyAbsSyn34 ([Language.Lambda.Syntax.Abs.VarIdent])
        | HappyAbsSyn35 (Language.Lambda.Syntax.Abs.Type)

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x02\x60\x06\x00\x00\x00\x00\x01\x00\x03\x00\x00\x00\x80\x00\x80\x01\x00\x00\x00\x40\x00\xcc\x00\x00\x00\x00\x20\x00\x66\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x40\x00\x40\x00\x00\x00\x00\x20\x00\x20\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\xc0\x0c\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x30\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x60\x06\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x20\x00\x66\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x80\x00\x80\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\xc0\x0c\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x33\x00\x00\x00\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x08\x80\x19\x00\x00\x00\x00\x04\xc0\x0c\x00\x00\x00\x00\x02\x60\x06\x00\x00\x00\x00\x01\x30\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pBinder","%start_pListBinder","%start_pMetaSubst","%start_pUnificationConstraint","%start_pListVarIdent","%start_pType","%start_pType1","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","Binder","ListBinder","MetaSubst","UnificationConstraint","ListVarIdent","Type","Type1","'('","')'","','","'->'","'.'","':'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","'\8704'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st               Prelude.* 55
        bit_end   = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf6\xff\xff\xff\xf6\xff\xff\xff\xf6\xff\xff\xff\x08\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\xf2\xff\xff\xff\xf2\xff\xff\xff\xf2\xff\xff\xff\xf8\xff\xff\xff\xfe\xff\xff\xff\xf7\xff\xff\xff\x25\x00\x00\x00\x25\x00\x00\x00\xf7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x25\x00\x00\x00\x12\x00\x00\x00\x26\x00\x00\x00\x2a\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x37\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x50\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x08\x00\x00\x00\x46\x00\x00\x00\x46\x00\x00\x00\x5e\x00\x00\x00\x52\x00\x00\x00\x52\x00\x00\x00\x01\x00\x00\x00\x52\x00\x00\x00\x5f\x00\x00\x00\x54\x00\x00\x00\x08\x00\x00\x00\x54\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x69\x00\x00\x00\x68\x00\x00\x00\x6f\x00\x00\x00\x08\x00\x00\x00\x64\x00\x00\x00\x25\x00\x00\x00\x64\x00\x00\x00\x74\x00\x00\x00\x6a\x00\x00\x00\x25\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x85\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x45\x00\x00\x00\x8c\x00\x00\x00\x42\x00\x00\x00\xa6\x00\x00\x00\xbc\x00\x00\x00\x05\x00\x00\x00\x8b\x00\x00\x00\x4f\x00\x00\x00\x27\x00\x00\x00\x41\x00\x00\x00\x2e\x00\x00\x00\x03\x00\x00\x00\x80\x00\x00\x00\x28\x00\x00\x00\x0b\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x00\x00\x32\x00\x00\x00\x0f\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x00\x00\x00\x6d\x00\x00\x00\x77\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xeb\xff\xff\xff\x00\x00\x00\x00\xeb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xff\xff\xd1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\xff\xff\xd5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\xff\xff\x00\x00\x00\x00\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xff\xff\x00\x00\x00\x00\xe4\xff\xff\xff\x00\x00\x00\x00\xde\xff\xff\xff\xe7\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xff\xff\xec\xff\xff\xff\xeb\xff\xff\xff\xe6\xff\xff\xff\xe1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\xdb\xff\xff\xff\x00\x00\x00\x00\xdb\xff\xff\xff\x00\x00\x00\x00\xd6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xff\xff\xd3\xff\xff\xff\xd4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\xff\xff\xd9\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xe8\xff\xff\xff\xd8\xff\xff\xff\xd7\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x0b\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x13\x00\x00\x00\x11\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0a\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x13\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x07\x00\x00\x00\x13\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0b\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x08\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0a\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x05\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0c\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x06\x00\x00\x00\x07\x00\x00\x00\xff\xff\xff\xff\x07\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x35\x00\x00\x00\x2b\x00\x00\x00\x12\x00\x00\x00\x1b\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x2b\x00\x00\x00\x12\x00\x00\x00\x2b\x00\x00\x00\x1e\x00\x00\x00\x12\x00\x00\x00\x2f\x00\x00\x00\x12\x00\x00\x00\x1b\x00\x00\x00\x12\x00\x00\x00\x1c\x00\x00\x00\x12\x00\x00\x00\x12\x00\x00\x00\x1e\x00\x00\x00\xff\xff\xff\xff\x2c\x00\x00\x00\x2d\x00\x00\x00\x12\x00\x00\x00\x1e\x00\x00\x00\x12\x00\x00\x00\x1e\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x46\x00\x00\x00\x16\x00\x00\x00\x4c\x00\x00\x00\x16\x00\x00\x00\x48\x00\x00\x00\x16\x00\x00\x00\x12\x00\x00\x00\x12\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x22\x00\x00\x00\x17\x00\x00\x00\x17\x00\x00\x00\x46\x00\x00\x00\xff\xff\xff\xff\x12\x00\x00\x00\x45\x00\x00\x00\x1e\x00\x00\x00\xff\xff\xff\xff\x1e\x00\x00\x00\x23\x00\x00\x00\x1e\x00\x00\x00\x54\x00\x00\x00\x16\x00\x00\x00\x13\x00\x00\x00\x12\x00\x00\x00\x18\x00\x00\x00\x49\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x1f\x00\x00\x00\x43\x00\x00\x00\x1f\x00\x00\x00\x4d\x00\x00\x00\x1e\x00\x00\x00\x43\x00\x00\x00\x1e\x00\x00\x00\x42\x00\x00\x00\x22\x00\x00\x00\x22\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x36\x00\x00\x00\x32\x00\x00\x00\x37\x00\x00\x00\x1f\x00\x00\x00\x4b\x00\x00\x00\x21\x00\x00\x00\x3d\x00\x00\x00\x3c\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x32\x00\x00\x00\x53\x00\x00\x00\x41\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x12\x00\x00\x00\x29\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\xff\xff\xff\xff\x40\x00\x00\x00\xff\xff\xff\xff\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x3c\x00\x00\x00\x58\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\xff\xff\xff\xff\x3a\x00\x00\x00\xff\xff\xff\xff\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x35\x00\x00\x00\x60\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x52\x00\x00\x00\x51\x00\x00\x00\x50\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x12\x00\x00\x00\x5f\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x4b\x00\x00\x00\x48\x00\x00\x00\x12\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x58\x00\x00\x00\x5e\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x57\x00\x00\x00\x5d\x00\x00\x00\x5b\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x5a\x00\x00\x00\x19\x00\x00\x00\x35\x00\x00\x00\x2d\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x2e\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x52\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x4e\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x31\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x3e\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x38\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x55\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (16, 47) [
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47)
        ]

happy_n_terms = 20 :: Prelude.Int
happy_n_nonterms = 18 :: Prelude.Int

happyReduce_16 = happySpecReduce_1  0# happyReduction_16
happyReduction_16 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
         =  HappyAbsSyn19
                 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
        )
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  1# happyReduction_17
happyReduction_17 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
         =  HappyAbsSyn20
                 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
        )
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  2# happyReduction_18
happyReduction_18 (HappyAbsSyn23  happy_var_1)
         =  HappyAbsSyn21
                 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
        )
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  3# happyReduction_19
happyReduction_19 (HappyAbsSyn24  happy_var_2)
        _
         =  HappyAbsSyn22
                 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
        )
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  4# happyReduction_20
happyReduction_20  =  HappyAbsSyn23
                 ([]
        )

happyReduce_21 = happySpecReduce_3  4# happyReduction_21
happyReduction_21 (HappyAbsSyn23  happy_var_3)
        _
        (HappyAbsSyn22  happy_var_1)
         =  HappyAbsSyn23
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6# 5# happyReduction_22
happyReduction_22 ((HappyAbsSyn28  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn35  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn29  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn24
                 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_23 = happyReduce 6# 5# happyReduction_23
happyReduction_23 ((HappyAbsSyn28  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn24  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn29  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn24
                 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  5# happyReduction_24
happyReduction_24 (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  6# happyReduction_25
happyReduction_25 (HappyAbsSyn24  happy_var_2)
        (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn24
                 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
        )
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  6# happyReduction_26
happyReduction_26 (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  7# happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn24
                 (Language.Lambda.Syntax.Abs.Var happy_var_1
        )
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 4# 7# happyReduction_28
happyReduction_28 (_ `HappyStk`
        (HappyAbsSyn27  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn20  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn24
                 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
        ) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  7# happyReduction_29
happyReduction_29 _
        (HappyAbsSyn24  happy_var_2)
        _
         =  HappyAbsSyn24
                 (happy_var_2
        )
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  8# happyReduction_30
happyReduction_30  =  HappyAbsSyn27
                 ([]
        )

happyReduce_31 = happySpecReduce_1  8# happyReduction_31
happyReduction_31 (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn27
                 ((:[]) happy_var_1
        )
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  8# happyReduction_32
happyReduction_32 (HappyAbsSyn27  happy_var_3)
        _
        (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn27
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9# happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn28
                 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  10# happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn29
                 (Language.Lambda.Syntax.Abs.APattern happy_var_1
        )
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11# happyReduction_35
happyReduction_35 (HappyAbsSyn35  happy_var_3)
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn30
                 (Language.Lambda.Syntax.Abs.ABinder happy_var_1 happy_var_3
        )
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  12# happyReduction_36
happyReduction_36  =  HappyAbsSyn31
                 ([]
        )

happyReduce_37 = happySpecReduce_1  12# happyReduction_37
happyReduction_37 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn31
                 ((:[]) happy_var_1
        )
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  12# happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn31
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 6# 13# happyReduction_39
happyReduction_39 ((HappyAbsSyn28  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn31  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn20  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn32
                 (Language.Lambda.Syntax.Abs.AMetaSubst happy_var_1 happy_var_3 happy_var_6
        ) `HappyStk` happyRest

happyReduce_40 = happyReduce 6# 14# happyReduction_40
happyReduction_40 ((HappyAbsSyn28  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn28  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn31  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn33
                 (Language.Lambda.Syntax.Abs.AUnificationConstraint happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_0  15# happyReduction_41
happyReduction_41  =  HappyAbsSyn34
                 ([]
        )

happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn34
                 ((:[]) happy_var_1
        )
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  15# happyReduction_43
happyReduction_43 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn34
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  16# happyReduction_44
happyReduction_44 (HappyAbsSyn35  happy_var_3)
        _
        (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn35
                 (Language.Lambda.Syntax.Abs.Fun happy_var_1 happy_var_3
        )
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn35
                 (happy_var_1
        )
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  17# happyReduction_46
happyReduction_46 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn35
                 (Language.Lambda.Syntax.Abs.Base happy_var_1
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 _
        (HappyAbsSyn35  happy_var_2)
        _
         =  HappyAbsSyn35
                 (happy_var_2
        )
happyReduction_47 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
        happyDoAction 19# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        PT _ (TS _ 1) -> cont 1#;
        PT _ (TS _ 2) -> cont 2#;
        PT _ (TS _ 3) -> cont 3#;
        PT _ (TS _ 4) -> cont 4#;
        PT _ (TS _ 5) -> cont 5#;
        PT _ (TS _ 6) -> cont 6#;
        PT _ (TS _ 7) -> cont 7#;
        PT _ (TS _ 8) -> cont 8#;
        PT _ (TS _ 9) -> cont 9#;
        PT _ (TS _ 10) -> cont 10#;
        PT _ (TS _ 11) -> cont 11#;
        PT _ (TS _ 12) -> cont 12#;
        PT _ (TS _ 13) -> cont 13#;
        PT _ (TS _ 14) -> cont 14#;
        PT _ (TS _ 15) -> cont 15#;
        PT _ (TS _ 16) -> cont 16#;
        PT _ (T_VarIdent happy_dollar_dollar) -> cont 17#;
        PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 18#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 19# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pBinder tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pListBinder tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

pUnificationConstraint tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> case x of {HappyAbsSyn34 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
