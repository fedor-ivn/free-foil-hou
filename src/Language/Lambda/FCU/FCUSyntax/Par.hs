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

module Language.Lambda.FCU.FCUSyntax.Par
  ( happyError
  , myLexer
  , pTerm
  , pPattern
  , pScopedTerm
  ) where

import Prelude

import qualified Language.Lambda.FCU.FCUSyntax.Abs
import Language.Lambda.FCU.FCUSyntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn6 (Language.Lambda.FCU.FCUSyntax.Abs.Id)
        | HappyAbsSyn7 (Language.Lambda.FCU.FCUSyntax.Abs.MetavarId)
        | HappyAbsSyn8 (Language.Lambda.FCU.FCUSyntax.Abs.ConstructorId)
        | HappyAbsSyn9 (Language.Lambda.FCU.FCUSyntax.Abs.Term)
        | HappyAbsSyn10 (Language.Lambda.FCU.FCUSyntax.Abs.Pattern)
        | HappyAbsSyn11 (Language.Lambda.FCU.FCUSyntax.Abs.ScopedTerm)

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\xc8\x03\x00\x04\x00\xf2\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x03\x00\x00\x00\x08\x00\xb0\x07\x00\x00\x00\xe4\x01\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pTerm","%start_pPattern","%start_pScopedTerm","Id","MetavarId","ConstructorId","Term","Pattern","ScopedTerm","'('","')'","'.'","'\955'","L_Id","L_MetavarId","L_ConstructorId","%eof"]
        bit_start = st               Prelude.* 19
        bit_end   = (st Prelude.+ 1) Prelude.* 19
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..18]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x10\x00\x00\x00\xfc\xff\xff\xff\x10\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\xfb\xff\xff\xff\x10\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x29\x00\x00\x00\x22\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xf8\xff\xff\xff\xf9\xff\xff\xff\xf7\xff\xff\xff\xf2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xff\xff\xfa\xff\xff\xff\xf3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\xf5\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x05\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\xff\xff\xff\xff\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\xff\xff\xff\xff\x05\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x04\x00\x00\x00\xff\xff\xff\xff\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x05\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\x0b\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\x0c\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0b\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x0e\x00\x00\x00\x16\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x13\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (3, 13) [
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13)
        ]

happy_n_terms = 9 :: Prelude.Int
happy_n_nonterms = 6 :: Prelude.Int

happyReduce_3 = happySpecReduce_1  0# happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_Id happy_var_1)))
         =  HappyAbsSyn6
                 (Language.Lambda.FCU.FCUSyntax.Abs.Id happy_var_1
        )
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (T_MetavarId happy_var_1)))
         =  HappyAbsSyn7
                 (Language.Lambda.FCU.FCUSyntax.Abs.MetavarId happy_var_1
        )
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  2# happyReduction_5
happyReduction_5 (HappyTerminal (PT _ (T_ConstructorId happy_var_1)))
         =  HappyAbsSyn8
                 (Language.Lambda.FCU.FCUSyntax.Abs.ConstructorId happy_var_1
        )
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Lambda.FCU.FCUSyntax.Abs.WTerm happy_var_1
        )
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  3# happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Lambda.FCU.FCUSyntax.Abs.OTerm happy_var_1
        )
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  3# happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Lambda.FCU.FCUSyntax.Abs.CTerm happy_var_1
        )
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  3# happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Lambda.FCU.FCUSyntax.Abs.AppTerm happy_var_1 happy_var_2
        )
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4# 3# happyReduction_10
happyReduction_10 ((HappyAbsSyn11  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn10  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Language.Lambda.FCU.FCUSyntax.Abs.AbsTerm happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  3# happyReduction_11
happyReduction_11 _
        (HappyAbsSyn9  happy_var_2)
        _
         =  HappyAbsSyn9
                 (happy_var_2
        )
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  4# happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn10
                 (Language.Lambda.FCU.FCUSyntax.Abs.PatternVar happy_var_1
        )
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn11
                 (Language.Lambda.FCU.FCUSyntax.Abs.ScopedTerm happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

happyNewToken action sts stk [] =
        happyDoAction 8# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        PT _ (TS _ 1) -> cont 1#;
        PT _ (TS _ 2) -> cont 2#;
        PT _ (TS _ 3) -> cont 3#;
        PT _ (TS _ 4) -> cont 4#;
        PT _ (T_Id happy_dollar_dollar) -> cont 5#;
        PT _ (T_MetavarId happy_dollar_dollar) -> cont 6#;
        PT _ (T_ConstructorId happy_dollar_dollar) -> cont 7#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 8# tk tks = happyError' (tks, explist)
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
pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

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
