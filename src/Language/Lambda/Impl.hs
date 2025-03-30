{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Free foil implementation of the \(\lambda)-calculus (with pairs).
--
-- Free foil provides __general__ definitions or implementations for the
-- following:
--
-- 1. Freely generated (from a simple signature) scope-safe AST.
-- 2. Correct capture-avoiding substitution (see 'substitute').
-- 3. Correct α-equivalence checks (see 'alphaEquiv' and 'alphaEquivRefreshed')
--    as well as α-normalization (see 'refreshAST').
-- 4. Conversion helpers (see 'convertToAST' and 'convertFromAST').
--
-- The following is __generated__ using Template Haskell:
--
-- 1. Convenient pattern synonyms.
-- 2. 'ZipMatch' instances (enabling general α-equivalence).
-- 3. Conversion between scope-safe and raw term representation.
--
-- The following is implemented __manually__ in this module:
--
-- 1. Computation of weak head normal form (WHNF), see 'whnf'.
-- 2. Entry point, gluing everything together. See 'defaultMain'.
--
-- __Note:__ free foil does not (easily) support patterns at the moment, so only
-- wildcard patterns and variable patterns are handled in this implementation.
module Language.Lambda.Impl (
  -- * Types
  Term,
  MetaTerm,
  MetaSubst',
  MetaSubsts',
  MetavarBinder,
  MetavarBinders,
  UnificationConstraint (..),

  -- * Core operations
  nfMetaTerm,
  nfMetaTermWithEmptyScope,
  matchPattern,
  whnf,
  nf,
  matchMetaAbs,
  moreGeneralThan,
  toBinders,
  withMetaSubstVars,
  fromTerm,
  fromMetaTerm,
  fromMetaSubst,
  toMetaSubst,
  annotate,
  toTerm,
  toTermClosed,
  getTermFromScopedTerm,
  getPatternBinder,

  -- * Parsing
  parseMetavarBinder,
  parseMetaSubst,

  -- * Pattern synonyms
  pattern App',
  pattern Lam',
  pattern MetaVar',
  pattern MetaApp,

  -- * Re-exports
  alphaEquiv,
) where

import Control.Monad (guard)
import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.Internal as FoilInternal
import qualified Control.Monad.Foil.Relative as Foil
import Control.Monad.Foil.TH
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.TH
import Data.Biapplicative (Bifunctor (bimap))
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import Data.SOAS (
  AnnSig (..),
  MetaAbs (..),
  MetaAppSig (..),
  MetaSubst (..),
  MetaSubsts (..),
  TypedBinder (..),
  TypedSOAS,
  TypedSignature (..),
  match,
  push,
  toNameMap,
  pattern MetaApp,
 )
import Data.String (IsString (..))
import Data.ZipMatchK
import Data.ZipMatchK.Bifunctor ()
import Debug.Trace (trace)
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw
import qualified Language.Lambda.Syntax.Print as Raw

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified Control.Monad.Foil as Foil
-- >>> import Control.Monad.Free.Foil
-- >>> import Data.String (fromString)

-- * Generated code

-- ** Signature

mkSignature ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms

mkPatternSynonyms ''TermSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
-- ** Scope-safe patterns

mkFoilPattern ''Raw.VarIdent ''Raw.Pattern
deriveCoSinkable ''Raw.VarIdent ''Raw.Pattern
mkToFoilPattern ''Raw.VarIdent ''Raw.Pattern
mkFromFoilPattern ''Raw.VarIdent ''Raw.Pattern

deriveUnifiablePattern ''Raw.VarIdent ''Raw.Pattern

deriveGenericK ''FoilPattern

instance Foil.SinkableK FoilPattern

deriving instance GHC.Generic (TermSig scope term)

deriveGenericK ''TermSig

instance ZipMatchK TermSig

-- | Match 'Raw.Ident' via 'Eq'.
instance ZipMatchK Raw.VarIdent where zipMatchWithK = zipMatchViaEq

instance ZipMatchK Raw.MetavarIdent where zipMatchWithK = zipMatchViaEq

instance ZipMatchK Raw.Type where zipMatchWithK = zipMatchViaEq

instance TypedSignature TermSig Raw.MetavarIdent Raw.Type where
  mapSigWithTypes varTypes _ f g sig t = case sig of
    AppSig fun arg -> do
      funType <- case fun of
        Var x -> return (Foil.lookupName x varTypes)
        Node (AnnSig _ type_) -> return type_
      case funType of
        Raw.Fun argType _returnType ->
          return (AppSig (g fun funType) (g arg argType))
        _ -> Nothing
    LamSig binderType body
      | Raw.Fun _ returnType <- t ->
          let body' = f body (binderType, returnType)
           in return (LamSig binderType body')
    _ -> Nothing

instance TypedBinder FoilPattern Raw.Type where
  addBinderTypes (FoilAPattern nameBinder) = Foil.addNameBinder nameBinder

-- ** Pattern synonyms

pattern App'
  :: AST binder (AnnSig Raw.Type (Sum TermSig q)) n
  -> AST binder (AnnSig Raw.Type (Sum TermSig q)) n
  -> Raw.Type
  -> AST binder (AnnSig Raw.Type (Sum TermSig q)) n
pattern App' f x typ = Node (AnnSig (L2 (AppSig f x)) typ)

pattern Lam'
  :: binder n l
  -> Raw.Type
  -> AST binder (AnnSig Raw.Type (Sum TermSig q)) l
  -> Raw.Type
  -> AST binder (AnnSig Raw.Type (Sum TermSig q)) n
pattern Lam' binder binderType body returnType =
  Node (AnnSig (L2 (LamSig binderType (ScopedAST binder body))) returnType)

-- pattern Let'
--   :: AST binder (Sum TermSig q) n
--   -> binder n l
--   -> AST binder (Sum TermSig q) l
--   -> AST binder (Sum TermSig q) n
-- pattern Let' term binder body = Node (L2 (LetSig term (ScopedAST binder body)))

pattern MetaVar'
  :: Raw.MetavarIdent
  -> [AST binder (AnnSig Raw.Type (Sum TermSig q)) n]
  -> Raw.Type
  -> AST binder (AnnSig Raw.Type (Sum TermSig q)) n
pattern MetaVar' metavar args typ = Node (AnnSig (L2 (MetavarSig metavar args)) typ)

-- FV( (λ x. x) y )  =  { y }
--
-- λs. λz. s (s z)    :: Term VoidS
--     λz. s (s z)    :: Term n1      --  n1 ~ { s }
--         s (s z)    :: Term n2      --  n2 ~ { s, z }
-- λs                 :: NameBinder VoidS n1
--     λz             :: NameBinder n1 n2
--

-- * User-defined code

-- | Scope-safe λ-term representation in scope @n@.
type Term = AST FoilPattern TermSig

type MetaTerm metavar n t = TypedSOAS FoilPattern metavar TermSig n t

-- M[g, \z. z a]
-- M[x, y] -> y x
-- y = \z. z a
-- x = g
-- (\z. z a) g

-- >>> subst = "X [x0: t, x1: t -> u] ↦ x1 x0" :: MetaSubst'
-- >>> term = "λg: t. λa: u. λw: v. X[g, λz: u -> t. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 : t . λ x1 : u . λ x2 : v . x0 x1
-- >>> subst = "X [x: t, y: t -> u] ↦ (λ z: t. y z) x" :: MetaSubst'
-- >>> term = "λg: t. λa: u. λw: v. X[g, λz: u -> t. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 : t . λ x1 : u . λ x2 : v . x0 x1
-- >>> term = "λg: t. λa: u. X[g, λz: u -> t. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 : t . λ x1 : u . x0 x1

-- {-# COMPLETE Var, Lam', App', Let', MetaVar', MetaApp #-}
{-# COMPLETE Var, Lam', App', MetaVar', MetaApp #-}

nfMetaTerm
  :: (Foil.Distinct n)
  => Foil.Scope n
  -> MetaTerm metavar n Raw.Type
  -> MetaTerm metavar n Raw.Type
nfMetaTerm scope = \case
  Var x -> Var x
  Lam' binder binderType body returnType
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let extendedScope = Foil.extendScopePattern binder scope
         in Lam' binder binderType (nfMetaTerm extendedScope body) returnType
  App' f x typ ->
    case nfMetaTerm scope f of
      Lam' binder _binderType body _returnType ->
        let subst = matchPattern binder x
         in nfMetaTerm scope (substitute scope subst body)
      f' -> App' f' (nfMetaTerm scope x) typ
  MetaVar' metavar args typ -> MetaVar' metavar (map (nfMetaTerm scope) args) typ
  MetaApp metavar args typ -> MetaApp metavar (map (nfMetaTerm scope) args) typ

nfMetaTermWithEmptyScope
  :: MetaTerm metavar Foil.VoidS Raw.Type
  -> MetaTerm metavar Foil.VoidS Raw.Type
nfMetaTermWithEmptyScope = nfMetaTerm Foil.emptyScope

-- nameMapToSubsts :: Foil.NameMap i (e o) -> Foil.Substitution e i o
-- nameMapToSubsts nameMap =
--   FoilInternal.UnsafeSubstitution $ FoilInternal.getNameMap nameMap

-- ** Conversion helpers for 'MetaSubst'

toMetaSubst
  :: MetavarBinders
  -> Raw.MetaSubst
  -> Maybe MetaSubst'
toMetaSubst metavarBinders (Raw.AMetaSubst metavar argIdents term) =
  do
    (argTypes, metavarType) <- Map.lookup metavar metavarBinders
    guard (length argTypes == length argIdents)
    let binders = zip argIdents argTypes
    with binders $ \scope env binderList binderTypes ->
      do
        (term', termType) <-
          annotate
            metavarBinders
            binderTypes
            (toTerm scope env (getTermFromScopedTerm term))
        guard (termType == metavarType)
        let metaAbs = MetaAbs binderList term'
        pure (MetaSubst (metavar, metaAbs))
 where
  with =
    withMetaSubstVars
      Foil.emptyScope
      Map.empty
      NameBinderListEmpty
      Foil.emptyNameMap

withMetaSubstVars
  :: (Distinct n)
  => Scope n
  -> Map Raw.VarIdent (Foil.Name n)
  -> NameBinderList i n
  -> Foil.NameMap n Raw.Type
  -> [(Raw.VarIdent, Raw.Type)]
  -> ( forall l
        . (Distinct l)
       => Scope l
       -> Map Raw.VarIdent (Foil.Name l)
       -> NameBinderList i l
       -> Foil.NameMap l Raw.Type
       -> r
     )
  -> r
withMetaSubstVars scope env binderList binderTypes [] cont =
  cont scope env binderList binderTypes
withMetaSubstVars
  scope
  env
  binderList
  binderTypes
  ((ident, type_) : binders)
  cont =
    withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
          name = Foil.nameOf binder
          env' = Map.insert ident name (Foil.sink <$> env)
          binderList' = push binder binderList
          binderTypes' = Foil.addNameBinder binder type_ binderTypes
       in withMetaSubstVars scope' env' binderList' binderTypes' binders cont

type MetaSubst' =
  MetaSubst
    FoilPattern
    (AnnSig Raw.Type (Sum TermSig (MetaAppSig Raw.MetavarIdent)))
    Raw.MetavarIdent
    Raw.Type

type MetaSubsts' =
  MetaSubsts
    FoilPattern
    (AnnSig Raw.Type (Sum TermSig (MetaAppSig Raw.MetavarIdent)))
    Raw.MetavarIdent
    Raw.Type

fromMetaSubst :: MetaSubst' -> Raw.MetaSubst
fromMetaSubst (MetaSubst (metavar, MetaAbs binderList term)) =
  let term' = Raw.AScopedTerm $ fromTerm $ fromMetaTerm term
      idents = toIdents binderList
   in Raw.AMetaSubst metavar idents term'

toIdents :: (Foil.Distinct i) => NameBinderList i n -> [Raw.VarIdent]
toIdents NameBinderListEmpty = []
toIdents (NameBinderListCons x xs)
  | Foil.Distinct <- Foil.assertDistinct x
  , Foil.Distinct <- Foil.assertDistinct xs
  , Foil.Ext <- Foil.assertExt xs =
      let ident = Raw.VarIdent $ "x" ++ show (Foil.nameOf x)
       in ident : toIdents xs

toBinders
  :: (Foil.Distinct i)
  => NameBinderList i n
  -> Foil.NameMap n Raw.Type
  -> [Raw.VarBinder]
toBinders NameBinderListEmpty _binderTypes = []
toBinders (NameBinderListCons x xs) binderTypes
  | Foil.Distinct <- Foil.assertDistinct x
  , Foil.Distinct <- Foil.assertDistinct xs
  , Foil.Ext <- Foil.assertExt xs =
      let ident = Raw.VarIdent $ "x" ++ show (Foil.nameOf x)
          typ = Foil.lookupName (Foil.sink (Foil.nameOf x)) binderTypes
       in Raw.AVarBinder ident typ : toBinders xs binderTypes

data UnificationConstraint where
  UnificationConstraint
    :: (Distinct n)
    => Scope n
    -> NameBinderList Foil.VoidS n
    -> Foil.NameMap n Raw.Type
    -> (MetaTerm Raw.MetavarIdent n Raw.Type, Raw.Type)
    -> (MetaTerm Raw.MetavarIdent n Raw.Type, Raw.Type)
    -> UnificationConstraint

type MetavarType = ([Raw.Type], Raw.Type)
type MetavarBinder = (Raw.MetavarIdent, MetavarType)
type MetavarBinders = Map Raw.MetavarIdent MetavarType

-- ** Conversion helpers for 'MetaTerm'

-- toMetaTerm
--   :: MetavarBinders
--   -> Foil.NameMap n Raw.Type
--   -> Term n
--   -> Maybe (MetaTerm Raw.MetavarIdent n Raw.Type)
-- toMetaTerm metavarTypes varTypes term =
--   fst <$> annotate metavarTypes varTypes term

-- >>> let Just (a, b) = annotate Map.empty Foil.emptyNameMap ("λy:t.λx:u.λy:v.y" :: Term Foil.VoidS)
-- >>> b
-- Fun (Base (VarIdent "t")) (Fun (Base (VarIdent "u")) (Fun (Base (VarIdent "v")) (Base (VarIdent "v"))))
annotate
  :: MetavarBinders
  -> Foil.NameMap n Raw.Type
  -> Term n
  -> Maybe (MetaTerm Raw.MetavarIdent n Raw.Type, Raw.Type)
annotate _ varTypes (Var x) =
  Just (Var x, Foil.lookupName x varTypes)
annotate metavarTypes varTypes (App function argument) = do
  (function', functionType) <- annotate metavarTypes varTypes function
  (argument', argumentType) <- annotate metavarTypes varTypes argument
  case functionType of
    Raw.Fun argumentType' returnType
      | argumentType == argumentType' ->
          let annSig = AnnSig (L2 (AppSig function' argument')) returnType
              term = Node annSig
           in Just (term, returnType)
    _ -> Nothing
annotate metavarTypes varTypes (Lam typ binder body) = do
  let varTypes' = addBinderTypes binder typ varTypes
  (body', returnType) <- annotate metavarTypes varTypes' body
  let lamType = Raw.Fun typ returnType
      annSig = AnnSig (L2 (LamSig typ (ScopedAST binder body'))) lamType
      term = Node annSig
  Just (term, lamType)
annotate metavarTypes varTypes (Metavar metavar args) = do
  (expectedArgTypes, returnType) <- Map.lookup metavar metavarTypes
  _ <- guard (length args == length expectedArgTypes)
  annotatedArgs <- traverse checkArg (zip args expectedArgTypes)
  let term = MetaApp metavar annotatedArgs returnType
  pure (term, returnType)
 where
  checkArg (arg, expectedType) = do
    (annotatedArg, actualType) <- annotate metavarTypes varTypes arg
    guard (actualType == expectedType)
    pure annotatedArg

fromMetaTerm :: MetaTerm Raw.MetavarIdent n t -> Term n
fromMetaTerm = \case
  Var name -> Var name
  Node (AnnSig (R2 (MetaAppSig metavar args)) _typ) -> Metavar metavar (map fromMetaTerm args)
  Node (AnnSig (L2 node) _) -> Node (bimap fromMetaScopedTerm fromMetaTerm node)
 where
  fromMetaScopedTerm (ScopedAST binder body) = ScopedAST binder (fromMetaTerm body)

-- ** Conversion helpers

-- | Convert 'Raw.Term'' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.VarIdent (Foil.Name n) -> Raw.Term -> Term n
toTerm = convertToAST convertToTermSig toFoilPattern getTermFromScopedTerm

-- | Convert 'Raw.Term'' into a closed scope-safe term.
-- This is a special case of 'toTerm''.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

fromTerm :: Term n -> Raw.Term
fromTerm =
  convertFromAST
    convertFromTermSig
    Raw.Var
    (fromFoilPattern (\i -> Raw.VarIdent ("x" ++ show i)))
    Raw.AScopedTerm
    (\i -> Raw.VarIdent ("x" ++ show i))

-- >>> lam (Raw.Base (Raw.VarIdent "t")) Foil.emptyScope (\x -> App (Metavar (Raw.MetavarIdent "X") []) (Var x))
-- λ x0 : t . X [] x0
-- >>> lam (Raw.Base (Raw.VarIdent "t")) Foil.emptyScope (\x -> App (Var x) (Var x))
-- λ x0 : t . x0 x0
-- lam :: (Distinct n) => Raw.Type -> Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> Term l) -> Term n
-- lam typ scope makeBody = Foil.withFresh scope $ \x' ->
--   let x = Foil.nameOf x'
--    in Lam typ (FoilAPattern x') (makeBody x)

instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- >>> "λy:t.(λx:u.λy:v.x)y" :: Term Foil.VoidS
-- λ x0 : t . (λ x1 : u . λ x2 : v . x1) x0
instance IsString (Term Foil.VoidS) where
  fromString :: String -> Term VoidS
  fromString = unsafeParseTerm

instance Show (MetaTerm Raw.MetavarIdent n Raw.Type) where
  show :: MetaTerm Raw.MetavarIdent n t -> String
  show = Raw.printTree . fromTerm . fromMetaTerm

instance Show MetaSubst' where
  show :: MetaSubst' -> String
  show = Raw.printTree . fromMetaSubst

instance Show MetaSubsts' where
  show :: MetaSubsts' -> String
  show = show . getMetaSubsts

instance IsString MetavarBinder where
  fromString :: String -> MetavarBinder
  fromString = either error id . parseMetavarBinder

unsafeParseTerm :: String -> Term Foil.VoidS
unsafeParseTerm input =
  case Raw.pTerm tokens of
    Left err -> error err
    Right term -> toTermClosed term
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)

parseMetaSubst :: MetavarBinders -> String -> Either String MetaSubst'
parseMetaSubst metavarBinders input = do
  let tokens = Raw.resolveLayout False (Raw.myLexer input)
  raw <- Raw.pMetaSubst tokens
  case toMetaSubst metavarBinders raw of
    Just subst -> pure subst
    Nothing -> trace "here" $ Left "type error"

parseMetavarBinder :: String -> Either String MetavarBinder
parseMetavarBinder input = fmap toMetavarBinder (Raw.pMetavarBinder tokens)
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)
  toMetavarBinder (Raw.AMetavarBinder metavar args returnType) =
    (metavar, (args, returnType))

-- | Match a pattern against an term.
matchPattern :: (InjectName t) => FoilPattern n l -> t n -> Foil.Substitution t l n
matchPattern (FoilAPattern x) = Foil.addSubst Foil.identitySubst x

-- >>> whnf Foil.emptyScope (lam (Raw.Base (Raw.VarIdent "magic")) Foil.emptyScope (\x -> App (Var x) (Var x)))
-- λ x0 : magic . x0 x0
-- >>> whnf Foil.emptyScope "(λs:fix.λz:fix.s (s z)) (λs:fix.λz:fix.s (s z))"
-- λ x1 : fix . (λ x0 : fix . λ x1 : fix . x0 (x0 x1)) ((λ x0 : fix . λ x1 : fix . x0 (x0 x1)) x1)
-- >>> whnf Foil.emptyScope "(λs:fix.λz:fix.s (S[] z)) (λs:fix.λz:fix.s (s z))"
-- λ x1 : fix . (λ x0 : fix . λ x1 : fix . x0 (x0 x1)) (S [] x1)
whnf :: (Foil.Distinct n) => Foil.Scope n -> Term n -> Term n
whnf scope = \case
  App f x ->
    case whnf scope f of
      Lam _type binder body ->
        let subst = matchPattern binder x
         in whnf scope (substitute scope subst body)
      f' -> App f' x
  t -> t

-- | Normalize a term to weak head normal form (WHNF).
-- >>> nf Foil.emptyScope "λy:t.λz:f. (λx:t.λy:f.y) y z"
-- λ x0 : t . λ x1 : f . x1
-- >>> nf Foil.emptyScope "λz:t.λw:u.(λx:t.λy:u->u.y) z (λz:u.z) w"
-- λ x0 : t . λ x1 : u . x1
-- >>> nf Foil.emptyScope "(λb:t->f->t.λx:f.λy:t.b y x) (λx:t.λy:f.x)"
-- λ x1 : f . λ x2 : t . x2
-- >>> nf Foil.emptyScope "(λs:(t->t)->t->t.λz:t->t.s(s z))(λb:t->t->t.λx:t.λy:t.b y x)(λx:t.λy:t.y)"
-- λ x2 : t . λ x3 : t . x3
-- >>> nf Foil.emptyScope "(λs:(t->t)->t->t.λz:t->t.s (s z)) (λs:(t->t)->t->t.λz:t->t.s (s z)) (λb:t->t->t.λy:t.λx:t.b x y) (λy:t.λx:t.x)"
-- λ x1 : t . λ x3 : t . x3
-- >>> nf Foil.emptyScope "(λ a : (t -> t) . λ x: u . x) (λ a : t . a)"
-- λ x1 : u . x1
nf :: (Foil.Distinct n) => Foil.Scope n -> Term n -> Term n
nf scope = \case
  App f x ->
    case nf scope f of
      Lam _type binder body ->
        let subst = matchPattern binder x
         in nf scope (substitute scope subst body)
      f' -> App f' x
  Lam typ binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let extendedScope = Foil.extendScopePattern binder scope
         in Lam typ binder (nf extendedScope body)
  t -> t

-- interpretCommand :: Raw.Command -> IO ()
-- interpretCommand (Raw.CommandCompute term) =
--   print $ nf Foil.emptyScope $ toTermClosed term

-- interpretProgram :: Raw.Program -> IO ()
-- interpretProgram (Raw.AProgram commands) = mapM_ interpretCommand commands

-- | Check if the left substitution is more general than the right substitution.
-- Returns a list of booleans, one per metavariable in LHS substitution. A
-- substitution σ1 is more general than σ2 if there exists a substitution τ such
-- that σ2 = τ ∘ σ1
-- >>> rawMetavarBinders = ["M : [t] t -> t -> t", "N : [t] t -> t"]
-- >>> Right metavarBinders = parseMetavarBinders rawMetavarBinders
-- >>> Right lhsSubst = parseMetaSubst metavarBinders "M[x] ↦ λy:t.N[x]"
-- >>> Right rhsSubst = parseMetaSubst metavarBinders "M[x] ↦ λy:t.λz:t.x"
-- >>> lhsSubsts = MetaSubsts [lhsSubst]
-- >>> rhsSubsts = MetaSubsts [rhsSubst]
-- >>> moreGeneralThan metavarBinders lhsSubsts rhsSubsts
-- True
-- >>> rawMetavarBinders = ["M : [t] t -> t -> t", "N : [t] t -> t"]
-- >>> Right metavarBinders = parseMetavarBinders rawMetavarBinders
-- >>> Right lhsSubst = parseMetaSubst metavarBinders "M[x] ↦ λy:t.λz:t.x"
-- >>> lhsSubsts = MetaSubsts [lhsSubst]
-- >>> rhsSubsts = MetaSubsts []
-- >>> moreGeneralThan metavarBinders lhsSubsts rhsSubsts
-- False
-- >>> rawMetavarBinders = ["F: [t -> t, t] t", "X: [t -> t] t", "Y: [] t", "H: [t -> t, t] t"]
-- >>> Right metavarBinders = parseMetavarBinders rawMetavarBinders
-- >>> Right lhsSubst = parseMetaSubst metavarBinders "M[x] ↦ λy:t.λz:t.x"
-- >>> lhsSubsts = MetaSubsts [lhsSubst]
-- >>> rhsSubsts = MetaSubsts []
-- >>> moreGeneralThan metavarBinders lhsSubsts rhsSubsts
-- <interactive>:1:2-68: Non-exhaustive patterns in Right lhsSubst
moreGeneralThan :: MetavarBinders -> MetaSubsts' -> MetaSubsts' -> Bool
moreGeneralThan metavarBinders lhs rhs =
  flip
    all
    lhsSubsts
    $ \(metavar, lhsAbs) ->
      let maybeSubsts = do
            rhsAbs <- lookup metavar rhsSubsts
            (argTypes, _) <- Map.lookup metavar metavarBinders
            let substs = matchMetaAbs argTypes metavarBinders lhsAbs rhsAbs
            listToMaybe substs
       in isJust maybeSubsts
 where
  lhsSubsts = map getMetaSubst $ getMetaSubsts lhs
  rhsSubsts = map getMetaSubst $ getMetaSubsts rhs

-- >>> rawMetavarBinders = ["M : [t] t -> t", "H1 : [t] t -> t", "H2 : [t] t -> t"]
-- >>> Right metavarBinders = parseMetavarBinders rawMetavarBinders
-- >>> Right lhs = parseMetaSubst metavarBinders "M[x] ↦ H1[x]"
-- >>> Right rhs = parseMetaSubst metavarBinders "M[x] ↦ H2[x]"
-- >>> (_, lhsAbs) = getMetaSubst lhs
-- >>> (_, rhsAbs) = getMetaSubst rhs
-- >>> Just (_, type_) = Map.lookup "M" metavarBinders
-- >>> matchMetaAbs metavarBinders type_ lhsAbs rhsAbs
-- [[H1 [x0] ↦ H2 [x0]]]
matchMetaAbs
  :: ( UnifiablePattern binder
     , SinkableK binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK ext
     , TypedBinder binder Raw.Type
     , TypedSignature sig Raw.MetavarIdent Raw.Type
     , TypedSignature ext Raw.MetavarIdent Raw.Type
     )
  => [Raw.Type]
  -> MetavarBinders
  -> MetaAbs binder (AnnSig Raw.Type (Sum sig (MetaAppSig Raw.MetavarIdent))) Raw.Type
  -> MetaAbs binder (AnnSig Raw.Type (Sum sig ext)) Raw.Type
  -> [MetaSubsts binder (AnnSig Raw.Type (Sum sig ext)) Raw.MetavarIdent Raw.Type]
matchMetaAbs
  metavarArgTypes
  metavarBinders
  (MetaAbs lhsBinderList lhsTerm)
  (MetaAbs rhsBinderList rhsTerm) =
    case Foil.unifyPatterns lhsBinderList rhsBinderList of
      Foil.SameNameBinders _ ->
        case Foil.assertDistinct lhsBinderList of
          Foil.Distinct ->
            let scope = Foil.extendScopePattern lhsBinderList Foil.emptyScope
                argTypes = toNameMap Foil.emptyNameMap lhsBinderList metavarArgTypes
             in trace "dbg" $ match scope metavarBinders argTypes lhsTerm rhsTerm
      Foil.RenameLeftNameBinder _ rename ->
        case Foil.assertDistinct rhsBinderList of
          Foil.Distinct ->
            let scope = Foil.extendScopePattern rhsBinderList Foil.emptyScope
                argTypes = toNameMap Foil.emptyNameMap rhsBinderList metavarArgTypes
                lhsTerm' = Foil.liftRM scope (Foil.fromNameBinderRenaming rename) lhsTerm
             in match scope metavarBinders argTypes lhsTerm' rhsTerm
      Foil.RenameRightNameBinder _ rename ->
        case Foil.assertDistinct lhsBinderList of
          Foil.Distinct ->
            let scope = Foil.extendScopePattern lhsBinderList Foil.emptyScope
                argTypes = toNameMap Foil.emptyNameMap lhsBinderList metavarArgTypes
                rhsTerm' = Foil.liftRM scope (Foil.fromNameBinderRenaming rename) rhsTerm
             in match scope metavarBinders argTypes lhsTerm rhsTerm'
      Foil.RenameBothBinders _commonBinders _rename1 _rename2 -> undefined
      Foil.NotUnifiable ->
        trace "Binders cannot be unified" []
