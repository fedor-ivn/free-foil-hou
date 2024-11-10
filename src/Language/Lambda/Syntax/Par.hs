{-# OPTIONS_GHC -w #-}
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
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

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

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,223) ([0,0,16384,0,0,0,32,0,0,4096,0,0,512,1632,0,0,1,3,0,128,384,0,16384,52224,0,0,32,102,0,0,4096,0,0,0,8,0,0,1024,0,0,0,4,0,0,128,0,0,32768,0,0,64,64,0,8192,8192,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,2,0,0,128,0,0,0,0,0,0,0,0,0,0,1024,0,0,512,0,0,0,0,0,0,0,0,0,2048,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,32,96,0,0,0,0,0,0,0,0,1024,3264,0,0,0,2,0,0,256,0,0,2,0,0,0,0,0,0,0,0,0,16,48,0,0,0,0,0,256,0,0,0,0,0,0,12289,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,24578,6,0,8192,0,0,0,64,0,0,128,0,0,8192,26112,0,0,0,16,0,2048,2048,0,0,0,4,0,8192,0,0,0,0,1,0,128,128,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,3264,0,0,1024,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,16,51,0,2048,2048,0,0,0,0,0,0,0,0,0,16,0,0,0,4,0,0,0,0,0,0,8,0,0,8,0,0,32776,25,0,1024,3264,0,0,24578,6,0,256,816,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pBinder","%start_pListBinder","%start_pMetaSubst","%start_pUnificationConstraint","%start_pListVarIdent","%start_pType","%start_pType1","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","Binder","ListBinder","MetaSubst","UnificationConstraint","ListVarIdent","Type","Type1","'('","')'","','","'->'","'.'","':'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","'\8704'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 55
        bit_end = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (47) = happyShift action_52
action_0 (21) = happyGoto action_54
action_0 (22) = happyGoto action_50
action_0 (23) = happyGoto action_55
action_0 _ = happyReduce_20

action_1 (47) = happyShift action_52
action_1 (22) = happyGoto action_53
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (47) = happyShift action_52
action_2 (22) = happyGoto action_50
action_2 (23) = happyGoto action_51
action_2 _ = happyReduce_20

action_3 (37) = happyShift action_42
action_3 (49) = happyShift action_43
action_3 (50) = happyShift action_44
action_3 (53) = happyShift action_17
action_3 (54) = happyShift action_29
action_3 (19) = happyGoto action_36
action_3 (20) = happyGoto action_37
action_3 (24) = happyGoto action_49
action_3 (25) = happyGoto action_39
action_3 (26) = happyGoto action_40
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (37) = happyShift action_42
action_4 (53) = happyShift action_17
action_4 (54) = happyShift action_29
action_4 (19) = happyGoto action_36
action_4 (20) = happyGoto action_37
action_4 (25) = happyGoto action_48
action_4 (26) = happyGoto action_40
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (37) = happyShift action_42
action_5 (53) = happyShift action_17
action_5 (54) = happyShift action_29
action_5 (19) = happyGoto action_36
action_5 (20) = happyGoto action_37
action_5 (26) = happyGoto action_47
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (37) = happyShift action_42
action_6 (49) = happyShift action_43
action_6 (50) = happyShift action_44
action_6 (53) = happyShift action_17
action_6 (54) = happyShift action_29
action_6 (19) = happyGoto action_36
action_6 (20) = happyGoto action_37
action_6 (24) = happyGoto action_45
action_6 (25) = happyGoto action_39
action_6 (26) = happyGoto action_40
action_6 (27) = happyGoto action_46
action_6 _ = happyReduce_30

action_7 (37) = happyShift action_42
action_7 (49) = happyShift action_43
action_7 (50) = happyShift action_44
action_7 (53) = happyShift action_17
action_7 (54) = happyShift action_29
action_7 (19) = happyGoto action_36
action_7 (20) = happyGoto action_37
action_7 (24) = happyGoto action_38
action_7 (25) = happyGoto action_39
action_7 (26) = happyGoto action_40
action_7 (28) = happyGoto action_41
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (53) = happyShift action_17
action_8 (19) = happyGoto action_34
action_8 (29) = happyGoto action_35
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (53) = happyShift action_17
action_9 (19) = happyGoto action_30
action_9 (30) = happyGoto action_33
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (53) = happyShift action_17
action_10 (19) = happyGoto action_30
action_10 (30) = happyGoto action_31
action_10 (31) = happyGoto action_32
action_10 _ = happyReduce_36

action_11 (54) = happyShift action_29
action_11 (20) = happyGoto action_27
action_11 (32) = happyGoto action_28
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (52) = happyShift action_26
action_12 (33) = happyGoto action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (53) = happyShift action_17
action_13 (19) = happyGoto action_23
action_13 (34) = happyGoto action_24
action_13 _ = happyReduce_41

action_14 (37) = happyShift action_20
action_14 (53) = happyShift action_17
action_14 (19) = happyGoto action_18
action_14 (35) = happyGoto action_21
action_14 (36) = happyGoto action_22
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (37) = happyShift action_20
action_15 (53) = happyShift action_17
action_15 (19) = happyGoto action_18
action_15 (36) = happyGoto action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (53) = happyShift action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_16

action_18 _ = happyReduce_46

action_19 (55) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (37) = happyShift action_20
action_20 (53) = happyShift action_17
action_20 (19) = happyGoto action_18
action_20 (35) = happyGoto action_70
action_20 (36) = happyGoto action_22
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (55) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (40) = happyShift action_69
action_22 _ = happyReduce_45

action_23 (39) = happyShift action_68
action_23 _ = happyReduce_42

action_24 (55) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (55) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (53) = happyShift action_17
action_26 (19) = happyGoto action_30
action_26 (30) = happyGoto action_31
action_26 (31) = happyGoto action_67
action_26 _ = happyReduce_36

action_27 (45) = happyShift action_66
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (55) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_17

action_30 (42) = happyShift action_65
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (39) = happyShift action_64
action_31 _ = happyReduce_37

action_32 (55) = happyAccept
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (55) = happyAccept
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_34

action_35 (55) = happyAccept
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_27

action_37 (45) = happyShift action_63
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_33

action_39 (37) = happyShift action_42
action_39 (53) = happyShift action_17
action_39 (54) = happyShift action_29
action_39 (19) = happyGoto action_36
action_39 (20) = happyGoto action_37
action_39 (26) = happyGoto action_58
action_39 _ = happyReduce_24

action_40 _ = happyReduce_26

action_41 (55) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (37) = happyShift action_42
action_42 (49) = happyShift action_43
action_42 (50) = happyShift action_44
action_42 (53) = happyShift action_17
action_42 (54) = happyShift action_29
action_42 (19) = happyGoto action_36
action_42 (20) = happyGoto action_37
action_42 (24) = happyGoto action_62
action_42 (25) = happyGoto action_39
action_42 (26) = happyGoto action_40
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (53) = happyShift action_17
action_43 (19) = happyGoto action_34
action_43 (29) = happyGoto action_61
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (53) = happyShift action_17
action_44 (19) = happyGoto action_34
action_44 (29) = happyGoto action_60
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (39) = happyShift action_59
action_45 _ = happyReduce_31

action_46 (55) = happyAccept
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (55) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (37) = happyShift action_42
action_48 (53) = happyShift action_17
action_48 (54) = happyShift action_29
action_48 (55) = happyAccept
action_48 (19) = happyGoto action_36
action_48 (20) = happyGoto action_37
action_48 (26) = happyGoto action_58
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (55) = happyAccept
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (43) = happyShift action_57
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (55) = happyAccept
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (37) = happyShift action_42
action_52 (49) = happyShift action_43
action_52 (50) = happyShift action_44
action_52 (53) = happyShift action_17
action_52 (54) = happyShift action_29
action_52 (19) = happyGoto action_36
action_52 (20) = happyGoto action_37
action_52 (24) = happyGoto action_56
action_52 (25) = happyGoto action_39
action_52 (26) = happyGoto action_40
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (55) = happyAccept
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (55) = happyAccept
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_18

action_56 _ = happyReduce_19

action_57 (47) = happyShift action_52
action_57 (22) = happyGoto action_50
action_57 (23) = happyGoto action_83
action_57 _ = happyReduce_20

action_58 _ = happyReduce_25

action_59 (37) = happyShift action_42
action_59 (49) = happyShift action_43
action_59 (50) = happyShift action_44
action_59 (53) = happyShift action_17
action_59 (54) = happyShift action_29
action_59 (19) = happyGoto action_36
action_59 (20) = happyGoto action_37
action_59 (24) = happyGoto action_45
action_59 (25) = happyGoto action_39
action_59 (26) = happyGoto action_40
action_59 (27) = happyGoto action_82
action_59 _ = happyReduce_30

action_60 (42) = happyShift action_81
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (44) = happyShift action_80
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (38) = happyShift action_79
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (37) = happyShift action_42
action_63 (49) = happyShift action_43
action_63 (50) = happyShift action_44
action_63 (53) = happyShift action_17
action_63 (54) = happyShift action_29
action_63 (19) = happyGoto action_36
action_63 (20) = happyGoto action_37
action_63 (24) = happyGoto action_45
action_63 (25) = happyGoto action_39
action_63 (26) = happyGoto action_40
action_63 (27) = happyGoto action_78
action_63 _ = happyReduce_30

action_64 (53) = happyShift action_17
action_64 (19) = happyGoto action_30
action_64 (30) = happyGoto action_31
action_64 (31) = happyGoto action_77
action_64 _ = happyReduce_36

action_65 (37) = happyShift action_20
action_65 (53) = happyShift action_17
action_65 (19) = happyGoto action_18
action_65 (35) = happyGoto action_76
action_65 (36) = happyGoto action_22
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (53) = happyShift action_17
action_66 (19) = happyGoto action_30
action_66 (30) = happyGoto action_31
action_66 (31) = happyGoto action_75
action_66 _ = happyReduce_36

action_67 (41) = happyShift action_74
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (53) = happyShift action_17
action_68 (19) = happyGoto action_23
action_68 (34) = happyGoto action_73
action_68 _ = happyReduce_41

action_69 (37) = happyShift action_20
action_69 (53) = happyShift action_17
action_69 (19) = happyGoto action_18
action_69 (35) = happyGoto action_72
action_69 (36) = happyGoto action_22
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (38) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_47

action_72 _ = happyReduce_44

action_73 _ = happyReduce_43

action_74 (37) = happyShift action_42
action_74 (49) = happyShift action_43
action_74 (50) = happyShift action_44
action_74 (53) = happyShift action_17
action_74 (54) = happyShift action_29
action_74 (19) = happyGoto action_36
action_74 (20) = happyGoto action_37
action_74 (24) = happyGoto action_38
action_74 (25) = happyGoto action_39
action_74 (26) = happyGoto action_40
action_74 (28) = happyGoto action_88
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (46) = happyShift action_87
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_35

action_77 _ = happyReduce_38

action_78 (46) = happyShift action_86
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_29

action_80 (37) = happyShift action_42
action_80 (49) = happyShift action_43
action_80 (50) = happyShift action_44
action_80 (53) = happyShift action_17
action_80 (54) = happyShift action_29
action_80 (19) = happyGoto action_36
action_80 (20) = happyGoto action_37
action_80 (24) = happyGoto action_85
action_80 (25) = happyGoto action_39
action_80 (26) = happyGoto action_40
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (37) = happyShift action_20
action_81 (53) = happyShift action_17
action_81 (19) = happyGoto action_18
action_81 (35) = happyGoto action_84
action_81 (36) = happyGoto action_22
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_32

action_83 _ = happyReduce_21

action_84 (41) = happyShift action_92
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (48) = happyShift action_91
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_28

action_87 (51) = happyShift action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (44) = happyShift action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (37) = happyShift action_42
action_89 (49) = happyShift action_43
action_89 (50) = happyShift action_44
action_89 (53) = happyShift action_17
action_89 (54) = happyShift action_29
action_89 (19) = happyGoto action_36
action_89 (20) = happyGoto action_37
action_89 (24) = happyGoto action_38
action_89 (25) = happyGoto action_39
action_89 (26) = happyGoto action_40
action_89 (28) = happyGoto action_96
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (37) = happyShift action_42
action_90 (49) = happyShift action_43
action_90 (50) = happyShift action_44
action_90 (53) = happyShift action_17
action_90 (54) = happyShift action_29
action_90 (19) = happyGoto action_36
action_90 (20) = happyGoto action_37
action_90 (24) = happyGoto action_38
action_90 (25) = happyGoto action_39
action_90 (26) = happyGoto action_40
action_90 (28) = happyGoto action_95
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (37) = happyShift action_42
action_91 (49) = happyShift action_43
action_91 (50) = happyShift action_44
action_91 (53) = happyShift action_17
action_91 (54) = happyShift action_29
action_91 (19) = happyGoto action_36
action_91 (20) = happyGoto action_37
action_91 (24) = happyGoto action_38
action_91 (25) = happyGoto action_39
action_91 (26) = happyGoto action_40
action_91 (28) = happyGoto action_94
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (37) = happyShift action_42
action_92 (49) = happyShift action_43
action_92 (50) = happyShift action_44
action_92 (53) = happyShift action_17
action_92 (54) = happyShift action_29
action_92 (19) = happyGoto action_36
action_92 (20) = happyGoto action_37
action_92 (24) = happyGoto action_38
action_92 (25) = happyGoto action_39
action_92 (26) = happyGoto action_40
action_92 (28) = happyGoto action_93
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_22

action_94 _ = happyReduce_23

action_95 _ = happyReduce_39

action_96 _ = happyReduce_40

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  20 happyReduction_17
happyReduction_17 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  21 happyReduction_18
happyReduction_18 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  22 happyReduction_19
happyReduction_19 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  23 happyReduction_20
happyReduction_20  =  HappyAbsSyn23
		 ([]
	)

happyReduce_21 = happySpecReduce_3  23 happyReduction_21
happyReduction_21 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 24 happyReduction_22
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

happyReduce_23 = happyReduce 6 24 happyReduction_23
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

happyReduce_24 = happySpecReduce_1  24 happyReduction_24
happyReduction_24 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  25 happyReduction_25
happyReduction_25 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  25 happyReduction_26
happyReduction_26 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  26 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn24
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 26 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  26 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  27 happyReduction_30
happyReduction_30  =  HappyAbsSyn27
		 ([]
	)

happyReduce_31 = happySpecReduce_1  27 happyReduction_31
happyReduction_31 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  27 happyReduction_32
happyReduction_32 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  28 happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn28
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  29 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn29
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  30 happyReduction_35
happyReduction_35 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn30
		 (Language.Lambda.Syntax.Abs.ABinder happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  31 happyReduction_36
happyReduction_36  =  HappyAbsSyn31
		 ([]
	)

happyReduce_37 = happySpecReduce_1  31 happyReduction_37
happyReduction_37 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ((:[]) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  31 happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 6 32 happyReduction_39
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

happyReduce_40 = happyReduce 6 33 happyReduction_40
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

happyReduce_41 = happySpecReduce_0  34 happyReduction_41
happyReduction_41  =  HappyAbsSyn34
		 ([]
	)

happyReduce_42 = happySpecReduce_1  34 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn34
		 ((:[]) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  34 happyReduction_43
happyReduction_43 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn34
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  35 happyReduction_44
happyReduction_44 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (Language.Lambda.Syntax.Abs.Fun happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  35 happyReduction_45
happyReduction_45 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  36 happyReduction_46
happyReduction_46 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn35
		 (Language.Lambda.Syntax.Abs.Base happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  36 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 55 55 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 37;
	PT _ (TS _ 2) -> cont 38;
	PT _ (TS _ 3) -> cont 39;
	PT _ (TS _ 4) -> cont 40;
	PT _ (TS _ 5) -> cont 41;
	PT _ (TS _ 6) -> cont 42;
	PT _ (TS _ 7) -> cont 43;
	PT _ (TS _ 8) -> cont 44;
	PT _ (TS _ 9) -> cont 45;
	PT _ (TS _ 10) -> cont 46;
	PT _ (TS _ 11) -> cont 47;
	PT _ (TS _ 12) -> cont 48;
	PT _ (TS _ 13) -> cont 49;
	PT _ (TS _ 14) -> cont 50;
	PT _ (TS _ 15) -> cont 51;
	PT _ (TS _ 16) -> cont 52;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 53;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 54;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 55 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pBinder tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pListBinder tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

pUnificationConstraint tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn34 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
