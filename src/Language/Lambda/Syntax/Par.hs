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
	| HappyAbsSyn17 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn18 (Language.Lambda.Syntax.Abs.MetaVarIdent)
	| HappyAbsSyn19 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn20 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn21 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn22 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn25 ([Language.Lambda.Syntax.Abs.Term])
	| HappyAbsSyn26 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn27 (Language.Lambda.Syntax.Abs.Pattern)
	| HappyAbsSyn28 (Language.Lambda.Syntax.Abs.MetaSubst)
	| HappyAbsSyn29 (Language.Lambda.Syntax.Abs.UnificationConstraint)
	| HappyAbsSyn30 ([Language.Lambda.Syntax.Abs.VarIdent])
	| HappyAbsSyn31 (Language.Lambda.Syntax.Abs.Type)

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
 action_86 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_14,
 happyReduce_15,
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
 happyReduce_41 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,203) ([0,0,1024,0,0,8192,0,0,0,1,0,512,1632,0,4096,12288,0,32768,32768,1,0,49156,12,0,32,102,0,0,256,0,0,4096,0,0,8192,0,0,0,2,0,16,16,0,128,128,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,64,64,0,0,0,0,32768,0,0,0,2,0,0,0,0,0,0,0,0,0,256,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,8,24,0,0,0,0,0,0,0,4096,13056,0,0,32768,0,0,0,4,0,128,0,0,0,0,0,0,0,0,16384,49152,0,0,0,0,0,1024,0,0,0,0,0,1024,3264,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,32768,38912,1,0,128,0,0,4096,0,0,512,0,0,2048,6528,0,0,16384,0,0,32,0,0,0,16,0,128,128,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,64,204,0,0,4,0,0,32,0,0,0,0,0,49156,12,0,32,32,0,0,0,0,0,0,0,0,4,0,0,4096,0,0,0,0,0,0,32,0,0,2,0,8192,26112,0,0,12289,3,0,32776,25,0,64,204,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pMetaSubst","%start_pUnificationConstraint","%start_pListVarIdent","%start_pType","%start_pType1","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","MetaSubst","UnificationConstraint","ListVarIdent","Type","Type1","'('","')'","','","'->'","'.'","':'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","'\8704'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 51
        bit_end = (st Prelude.+ 1) Prelude.* 51
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..50]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (43) = happyShift action_46
action_0 (19) = happyGoto action_48
action_0 (20) = happyGoto action_44
action_0 (21) = happyGoto action_49
action_0 _ = happyReduce_18

action_1 (43) = happyShift action_46
action_1 (20) = happyGoto action_47
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (43) = happyShift action_46
action_2 (20) = happyGoto action_44
action_2 (21) = happyGoto action_45
action_2 _ = happyReduce_18

action_3 (33) = happyShift action_36
action_3 (45) = happyShift action_37
action_3 (46) = happyShift action_38
action_3 (49) = happyShift action_15
action_3 (50) = happyShift action_27
action_3 (17) = happyGoto action_30
action_3 (18) = happyGoto action_31
action_3 (22) = happyGoto action_43
action_3 (23) = happyGoto action_33
action_3 (24) = happyGoto action_34
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (33) = happyShift action_36
action_4 (49) = happyShift action_15
action_4 (50) = happyShift action_27
action_4 (17) = happyGoto action_30
action_4 (18) = happyGoto action_31
action_4 (23) = happyGoto action_42
action_4 (24) = happyGoto action_34
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (33) = happyShift action_36
action_5 (49) = happyShift action_15
action_5 (50) = happyShift action_27
action_5 (17) = happyGoto action_30
action_5 (18) = happyGoto action_31
action_5 (24) = happyGoto action_41
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (33) = happyShift action_36
action_6 (45) = happyShift action_37
action_6 (46) = happyShift action_38
action_6 (49) = happyShift action_15
action_6 (50) = happyShift action_27
action_6 (17) = happyGoto action_30
action_6 (18) = happyGoto action_31
action_6 (22) = happyGoto action_39
action_6 (23) = happyGoto action_33
action_6 (24) = happyGoto action_34
action_6 (25) = happyGoto action_40
action_6 _ = happyReduce_28

action_7 (33) = happyShift action_36
action_7 (45) = happyShift action_37
action_7 (46) = happyShift action_38
action_7 (49) = happyShift action_15
action_7 (50) = happyShift action_27
action_7 (17) = happyGoto action_30
action_7 (18) = happyGoto action_31
action_7 (22) = happyGoto action_32
action_7 (23) = happyGoto action_33
action_7 (24) = happyGoto action_34
action_7 (26) = happyGoto action_35
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (49) = happyShift action_15
action_8 (17) = happyGoto action_28
action_8 (27) = happyGoto action_29
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (50) = happyShift action_27
action_9 (18) = happyGoto action_25
action_9 (28) = happyGoto action_26
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (48) = happyShift action_24
action_10 (29) = happyGoto action_23
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (49) = happyShift action_15
action_11 (17) = happyGoto action_21
action_11 (30) = happyGoto action_22
action_11 _ = happyReduce_35

action_12 (33) = happyShift action_18
action_12 (49) = happyShift action_15
action_12 (17) = happyGoto action_16
action_12 (31) = happyGoto action_19
action_12 (32) = happyGoto action_20
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (33) = happyShift action_18
action_13 (49) = happyShift action_15
action_13 (17) = happyGoto action_16
action_13 (32) = happyGoto action_17
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (49) = happyShift action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_14

action_16 _ = happyReduce_40

action_17 (51) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (33) = happyShift action_18
action_18 (49) = happyShift action_15
action_18 (17) = happyGoto action_16
action_18 (31) = happyGoto action_62
action_18 (32) = happyGoto action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (51) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (36) = happyShift action_61
action_20 _ = happyReduce_39

action_21 (35) = happyShift action_60
action_21 _ = happyReduce_36

action_22 (51) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (51) = happyAccept
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (49) = happyShift action_15
action_24 (17) = happyGoto action_21
action_24 (30) = happyGoto action_59
action_24 _ = happyReduce_35

action_25 (41) = happyShift action_58
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (51) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_15

action_28 _ = happyReduce_32

action_29 (51) = happyAccept
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_25

action_31 (41) = happyShift action_57
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_31

action_33 (33) = happyShift action_36
action_33 (49) = happyShift action_15
action_33 (50) = happyShift action_27
action_33 (17) = happyGoto action_30
action_33 (18) = happyGoto action_31
action_33 (24) = happyGoto action_52
action_33 _ = happyReduce_22

action_34 _ = happyReduce_24

action_35 (51) = happyAccept
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (33) = happyShift action_36
action_36 (45) = happyShift action_37
action_36 (46) = happyShift action_38
action_36 (49) = happyShift action_15
action_36 (50) = happyShift action_27
action_36 (17) = happyGoto action_30
action_36 (18) = happyGoto action_31
action_36 (22) = happyGoto action_56
action_36 (23) = happyGoto action_33
action_36 (24) = happyGoto action_34
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (49) = happyShift action_15
action_37 (17) = happyGoto action_28
action_37 (27) = happyGoto action_55
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (49) = happyShift action_15
action_38 (17) = happyGoto action_28
action_38 (27) = happyGoto action_54
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (35) = happyShift action_53
action_39 _ = happyReduce_29

action_40 (51) = happyAccept
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (51) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (33) = happyShift action_36
action_42 (49) = happyShift action_15
action_42 (50) = happyShift action_27
action_42 (51) = happyAccept
action_42 (17) = happyGoto action_30
action_42 (18) = happyGoto action_31
action_42 (24) = happyGoto action_52
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (51) = happyAccept
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (39) = happyShift action_51
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (51) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (33) = happyShift action_36
action_46 (45) = happyShift action_37
action_46 (46) = happyShift action_38
action_46 (49) = happyShift action_15
action_46 (50) = happyShift action_27
action_46 (17) = happyGoto action_30
action_46 (18) = happyGoto action_31
action_46 (22) = happyGoto action_50
action_46 (23) = happyGoto action_33
action_46 (24) = happyGoto action_34
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (51) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (51) = happyAccept
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_16

action_50 _ = happyReduce_17

action_51 (43) = happyShift action_46
action_51 (20) = happyGoto action_44
action_51 (21) = happyGoto action_73
action_51 _ = happyReduce_18

action_52 _ = happyReduce_23

action_53 (33) = happyShift action_36
action_53 (45) = happyShift action_37
action_53 (46) = happyShift action_38
action_53 (49) = happyShift action_15
action_53 (50) = happyShift action_27
action_53 (17) = happyGoto action_30
action_53 (18) = happyGoto action_31
action_53 (22) = happyGoto action_39
action_53 (23) = happyGoto action_33
action_53 (24) = happyGoto action_34
action_53 (25) = happyGoto action_72
action_53 _ = happyReduce_28

action_54 (38) = happyShift action_71
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (40) = happyShift action_70
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (34) = happyShift action_69
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (33) = happyShift action_36
action_57 (45) = happyShift action_37
action_57 (46) = happyShift action_38
action_57 (49) = happyShift action_15
action_57 (50) = happyShift action_27
action_57 (17) = happyGoto action_30
action_57 (18) = happyGoto action_31
action_57 (22) = happyGoto action_39
action_57 (23) = happyGoto action_33
action_57 (24) = happyGoto action_34
action_57 (25) = happyGoto action_68
action_57 _ = happyReduce_28

action_58 (49) = happyShift action_15
action_58 (17) = happyGoto action_21
action_58 (30) = happyGoto action_67
action_58 _ = happyReduce_35

action_59 (37) = happyShift action_66
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (49) = happyShift action_15
action_60 (17) = happyGoto action_21
action_60 (30) = happyGoto action_65
action_60 _ = happyReduce_35

action_61 (33) = happyShift action_18
action_61 (49) = happyShift action_15
action_61 (17) = happyGoto action_16
action_61 (31) = happyGoto action_64
action_61 (32) = happyGoto action_20
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (34) = happyShift action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_41

action_64 _ = happyReduce_38

action_65 _ = happyReduce_37

action_66 (33) = happyShift action_36
action_66 (45) = happyShift action_37
action_66 (46) = happyShift action_38
action_66 (49) = happyShift action_15
action_66 (50) = happyShift action_27
action_66 (17) = happyGoto action_30
action_66 (18) = happyGoto action_31
action_66 (22) = happyGoto action_32
action_66 (23) = happyGoto action_33
action_66 (24) = happyGoto action_34
action_66 (26) = happyGoto action_78
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (42) = happyShift action_77
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (42) = happyShift action_76
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_27

action_70 (33) = happyShift action_36
action_70 (45) = happyShift action_37
action_70 (46) = happyShift action_38
action_70 (49) = happyShift action_15
action_70 (50) = happyShift action_27
action_70 (17) = happyGoto action_30
action_70 (18) = happyGoto action_31
action_70 (22) = happyGoto action_75
action_70 (23) = happyGoto action_33
action_70 (24) = happyGoto action_34
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (33) = happyShift action_18
action_71 (49) = happyShift action_15
action_71 (17) = happyGoto action_16
action_71 (31) = happyGoto action_74
action_71 (32) = happyGoto action_20
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_30

action_73 _ = happyReduce_19

action_74 (37) = happyShift action_82
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (44) = happyShift action_81
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_26

action_77 (47) = happyShift action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (40) = happyShift action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (33) = happyShift action_36
action_79 (45) = happyShift action_37
action_79 (46) = happyShift action_38
action_79 (49) = happyShift action_15
action_79 (50) = happyShift action_27
action_79 (17) = happyGoto action_30
action_79 (18) = happyGoto action_31
action_79 (22) = happyGoto action_32
action_79 (23) = happyGoto action_33
action_79 (24) = happyGoto action_34
action_79 (26) = happyGoto action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (33) = happyShift action_36
action_80 (45) = happyShift action_37
action_80 (46) = happyShift action_38
action_80 (49) = happyShift action_15
action_80 (50) = happyShift action_27
action_80 (17) = happyGoto action_30
action_80 (18) = happyGoto action_31
action_80 (22) = happyGoto action_32
action_80 (23) = happyGoto action_33
action_80 (24) = happyGoto action_34
action_80 (26) = happyGoto action_85
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (33) = happyShift action_36
action_81 (45) = happyShift action_37
action_81 (46) = happyShift action_38
action_81 (49) = happyShift action_15
action_81 (50) = happyShift action_27
action_81 (17) = happyGoto action_30
action_81 (18) = happyGoto action_31
action_81 (22) = happyGoto action_32
action_81 (23) = happyGoto action_33
action_81 (24) = happyGoto action_34
action_81 (26) = happyGoto action_84
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (33) = happyShift action_36
action_82 (45) = happyShift action_37
action_82 (46) = happyShift action_38
action_82 (49) = happyShift action_15
action_82 (50) = happyShift action_27
action_82 (17) = happyGoto action_30
action_82 (18) = happyGoto action_31
action_82 (22) = happyGoto action_32
action_82 (23) = happyGoto action_33
action_82 (24) = happyGoto action_34
action_82 (26) = happyGoto action_83
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_20

action_84 _ = happyReduce_21

action_85 _ = happyReduce_33

action_86 _ = happyReduce_34

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn18
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  20 happyReduction_17
happyReduction_17 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  21 happyReduction_18
happyReduction_18  =  HappyAbsSyn21
		 ([]
	)

happyReduce_19 = happySpecReduce_3  21 happyReduction_19
happyReduction_19 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 22 happyReduction_20
happyReduction_20 ((HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 22 happyReduction_21
happyReduction_21 ((HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  22 happyReduction_22
happyReduction_22 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  23 happyReduction_23
happyReduction_23 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  23 happyReduction_24
happyReduction_24 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  24 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 24 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  24 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  25 happyReduction_28
happyReduction_28  =  HappyAbsSyn25
		 ([]
	)

happyReduce_29 = happySpecReduce_1  25 happyReduction_29
happyReduction_29 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn25
		 ((:[]) happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  25 happyReduction_30
happyReduction_30 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn25
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  26 happyReduction_31
happyReduction_31 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn26
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  27 happyReduction_32
happyReduction_32 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn27
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 6 28 happyReduction_33
happyReduction_33 ((HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Language.Lambda.Syntax.Abs.AMetaSubst happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 6 29 happyReduction_34
happyReduction_34 ((HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (Language.Lambda.Syntax.Abs.AUnificationConstraint happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  30 happyReduction_35
happyReduction_35  =  HappyAbsSyn30
		 ([]
	)

happyReduce_36 = happySpecReduce_1  30 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn30
		 ((:[]) happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  30 happyReduction_37
happyReduction_37 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn30
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  31 happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (Language.Lambda.Syntax.Abs.Fun happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  31 happyReduction_39
happyReduction_39 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  32 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn31
		 (Language.Lambda.Syntax.Abs.Base happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  32 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 51 51 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 33;
	PT _ (TS _ 2) -> cont 34;
	PT _ (TS _ 3) -> cont 35;
	PT _ (TS _ 4) -> cont 36;
	PT _ (TS _ 5) -> cont 37;
	PT _ (TS _ 6) -> cont 38;
	PT _ (TS _ 7) -> cont 39;
	PT _ (TS _ 8) -> cont 40;
	PT _ (TS _ 9) -> cont 41;
	PT _ (TS _ 10) -> cont 42;
	PT _ (TS _ 11) -> cont 43;
	PT _ (TS _ 12) -> cont 44;
	PT _ (TS _ 13) -> cont 45;
	PT _ (TS _ 14) -> cont 46;
	PT _ (TS _ 15) -> cont 47;
	PT _ (TS _ 16) -> cont 48;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 49;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 50;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 51 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pUnificationConstraint tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

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
