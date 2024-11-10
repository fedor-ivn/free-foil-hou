-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Language.Lambda.Syntax.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Language.Lambda.Syntax.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transVarIdent :: Language.Lambda.Syntax.Abs.VarIdent -> Result
transVarIdent x = case x of
  Language.Lambda.Syntax.Abs.VarIdent string -> failure x

transMetaVarIdent :: Language.Lambda.Syntax.Abs.MetaVarIdent -> Result
transMetaVarIdent x = case x of
  Language.Lambda.Syntax.Abs.MetaVarIdent string -> failure x

transProgram :: Language.Lambda.Syntax.Abs.Program -> Result
transProgram x = case x of
  Language.Lambda.Syntax.Abs.AProgram commands -> failure x

transCommand :: Language.Lambda.Syntax.Abs.Command -> Result
transCommand x = case x of
  Language.Lambda.Syntax.Abs.CommandCompute term -> failure x

transTerm :: Language.Lambda.Syntax.Abs.Term -> Result
transTerm x = case x of
  Language.Lambda.Syntax.Abs.Lam pattern_ type_ scopedterm -> failure x
  Language.Lambda.Syntax.Abs.Let pattern_ term scopedterm -> failure x
  Language.Lambda.Syntax.Abs.App term1 term2 -> failure x
  Language.Lambda.Syntax.Abs.Var varident -> failure x
  Language.Lambda.Syntax.Abs.MetaVar metavarident terms -> failure x

transScopedTerm :: Language.Lambda.Syntax.Abs.ScopedTerm -> Result
transScopedTerm x = case x of
  Language.Lambda.Syntax.Abs.AScopedTerm term -> failure x

transPattern :: Language.Lambda.Syntax.Abs.Pattern -> Result
transPattern x = case x of
  Language.Lambda.Syntax.Abs.APattern varident -> failure x

transMetaSubst :: Language.Lambda.Syntax.Abs.MetaSubst -> Result
transMetaSubst x = case x of
  Language.Lambda.Syntax.Abs.AMetaSubst metavarident varidents scopedterm -> failure x

transUnificationConstraint :: Language.Lambda.Syntax.Abs.UnificationConstraint -> Result
transUnificationConstraint x = case x of
  Language.Lambda.Syntax.Abs.AUnificationConstraint varidents scopedterm1 scopedterm2 -> failure x

transType :: Language.Lambda.Syntax.Abs.Type -> Result
transType x = case x of
  Language.Lambda.Syntax.Abs.Fun type_1 type_2 -> failure x
  Language.Lambda.Syntax.Abs.Base varident -> failure x
