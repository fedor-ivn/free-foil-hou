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

-- {-# OPTIONS_GHC -ddump-splices #-}

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
module Language.Lambda.Impl where

import Control.Monad (forM_, guard)
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
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SOAS
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as TIO
import Data.ZipMatchK
import Data.ZipMatchK.Bifunctor ()
import GHC.Generics (Generic)
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw
import qualified Language.Lambda.Syntax.Print as Raw
import Toml (TomlCodec, (.=))
import qualified Toml
import Unsafe.Coerce (unsafeCoerce)

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

-- instance TypedSignature TermSig where
--   mapSigWithTypes f g sig t = case sig of
--     AppSig h x -> AppSig (fmap f f) (fmap f x)
--     Lam typ binder body | Raw.Fun _ b <- t -> Lam typ binder (f body (typ, b))
--     MetavarSig metavar args -> MetavarSig metavar (fmap (mapSigWithTypes f g) args)

instance TypedSignature TermSig Raw.Type where
  mapSigWithTypes varTypes f g sig t = case sig of
    AppSig fun arg -> do
      funType <- case fun of
        Var x -> return (Foil.lookupName x varTypes)
        Node (L2 (AnnSig _ type_)) -> return type_
        _ -> Nothing
      case funType of
        Raw.Fun argType _returnType ->
          return (AppSig (g fun funType) (g arg argType))
        _ -> Nothing
    LamSig binderType body
      | Raw.Fun _ returnType <- t ->
          let body' = f body (binderType, returnType)
           in return (LamSig binderType body')
    _ -> Nothing

  debugSig :: forall binder ext n. AST binder (Sum (AnnSig Raw.Type TermSig) ext) n -> String
  debugSig term = let term' = unsafeCoerce term :: MetaTerm Raw.MetavarIdent n Raw.Type in show term'

instance TypedBinder FoilPattern Raw.Type where
  addBinderTypes (FoilAPattern nameBinder) = Foil.addNameBinder nameBinder
  mapBinderWithTypes f typ (FoilAPattern nameBinder) = [f nameBinder typ]

-- ** Pattern synonyms

pattern App'
  :: AST binder (Sum (AnnSig Raw.Type TermSig) q) n
  -> AST binder (Sum (AnnSig Raw.Type TermSig) q) n
  -> Raw.Type
  -> AST binder (Sum (AnnSig Raw.Type TermSig) q) n
pattern App' f x typ = Node (L2 (AnnSig (AppSig f x) typ))

pattern Lam'
  :: binder n l
  -> Raw.Type
  -> AST binder (Sum (AnnSig Raw.Type TermSig) q) l
  -> Raw.Type
  -> AST binder (Sum (AnnSig Raw.Type TermSig) q) n
pattern Lam' binder binderType body returnType =
  Node (L2 (AnnSig (LamSig binderType (ScopedAST binder body)) returnType))

-- pattern Let'
--   :: AST binder (Sum TermSig q) n
--   -> binder n l
--   -> AST binder (Sum TermSig q) l
--   -> AST binder (Sum TermSig q) n
-- pattern Let' term binder body = Node (L2 (LetSig term (ScopedAST binder body)))

pattern MetaVar'
  :: Raw.MetavarIdent
  -> [AST binder (Sum (AnnSig Raw.Type TermSig) q) n]
  -> Raw.Type
  -> AST binder (Sum (AnnSig Raw.Type TermSig) q) n
pattern MetaVar' metavar args typ = Node (L2 (AnnSig (MetavarSig metavar args) typ))

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
  MetaApp metavar args -> MetaApp metavar (map (nfMetaTerm scope) args)

nfMetaTermWithEmptyScope
  :: MetaTerm metavar Foil.VoidS Raw.Type
  -> MetaTerm metavar Foil.VoidS Raw.Type
nfMetaTermWithEmptyScope = nfMetaTerm Foil.emptyScope

nameMapToSubsts :: Foil.NameMap i (e o) -> Foil.Substitution e i o
nameMapToSubsts nameMap =
  FoilInternal.UnsafeSubstitution $ FoilInternal.getNameMap nameMap

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
        let metaAbs = MetaAbs binderList binderTypes term'
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
    (AnnSig Raw.Type TermSig)
    Raw.MetavarIdent
    (MetaAppSig Raw.MetavarIdent)
    Raw.Type

type MetaSubsts' =
  MetaSubsts
    FoilPattern
    (AnnSig Raw.Type TermSig)
    Raw.MetavarIdent
    (MetaAppSig Raw.MetavarIdent)
    Raw.Type

fromMetaSubst :: MetaSubst' -> Raw.MetaSubst
fromMetaSubst (MetaSubst (metavar, MetaAbs binderList _binderTypes term)) =
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

type MetavarBinder = (Raw.MetavarIdent, ([Raw.Type], Raw.Type))
type MetavarBinders = Map Raw.MetavarIdent ([Raw.Type], Raw.Type)

toUnificationConstraint
  :: MetavarBinders
  -> Raw.UnificationConstraint
  -> Maybe UnificationConstraint
toUnificationConstraint metavarBinders (Raw.AUnificationConstraint vars lhs rhs) =
  withMetaSubstVars
    Foil.emptyScope
    Map.empty
    NameBinderListEmpty
    Foil.emptyNameMap
    binders
    $ \scope env binderList binderTypes ->
      let annotate' =
            annotate
              metavarBinders
              binderTypes
              . toTerm scope env
              . getTermFromScopedTerm
       in do
            lhs' <- annotate' lhs
            rhs' <- annotate' rhs
            pure (UnificationConstraint scope binderList binderTypes lhs' rhs')
 where
  binders = map (\(Raw.AVarBinder ident typ) -> (ident, typ)) vars

fromUnificationConstraint :: UnificationConstraint -> Raw.UnificationConstraint
fromUnificationConstraint (UnificationConstraint _ binders binderTypes (lhs, _) (rhs, _)) =
  let fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm
   in Raw.AUnificationConstraint
        (toBinders binders binderTypes)
        (fromMetaTerm' lhs)
        (fromMetaTerm' rhs)

-- ** Conversion helpers for 'MetaTerm'

toMetaTerm
  :: MetavarBinders
  -> Foil.NameMap n Raw.Type
  -> Term n
  -> Maybe (MetaTerm Raw.MetavarIdent n Raw.Type)
toMetaTerm metavarTypes varTypes term =
  fst <$> annotate metavarTypes varTypes term

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
          let annSig = AnnSig (AppSig function' argument') returnType
              term = Node (L2 annSig)
           in Just (term, returnType)
    _ -> Nothing
annotate metavarTypes varTypes (Lam typ binder body) = do
  let (FoilAPattern name) = binder
      varTypes' = Foil.addNameBinder name typ varTypes
  (body', returnType) <- annotate metavarTypes varTypes' body
  let lamType = Raw.Fun typ returnType
      annSig = AnnSig (LamSig typ (ScopedAST binder body')) lamType
      term = Node (L2 annSig)
  Just (term, lamType)
annotate metavarTypes varTypes (Metavar metavar args) = do
  (expectedArgTypes, returnType) <- Map.lookup metavar metavarTypes
  _ <- guard (length args == length expectedArgTypes)
  annotatedArgs <- traverse checkArg (zip args expectedArgTypes)
  let term = MetaApp metavar annotatedArgs
  pure (term, returnType)
 where
  checkArg (arg, expectedType) = do
    (annotatedArg, actualType) <- annotate metavarTypes varTypes arg
    guard (actualType == expectedType)
    pure annotatedArg

fromMetaTerm :: MetaTerm Raw.MetavarIdent n t -> Term n
fromMetaTerm = \case
  Var name -> Var name
  Node (R2 (MetaAppSig metavar args)) -> Metavar metavar (map fromMetaTerm args)
  Node (L2 (AnnSig node _)) -> Node (bimap fromMetaScopedTerm fromMetaTerm node)
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
lam :: (Distinct n) => Raw.Type -> Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> Term l) -> Term n
lam typ scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam typ (FoilAPattern x') (makeBody x)

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
    Nothing -> Left "type error"

parseMetavarBinder :: String -> Either String MetavarBinder
parseMetavarBinder input = fmap toMetavarBinder (Raw.pMetavarBinder tokens)
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)
  toMetavarBinder (Raw.AMetavarBinder metavar args returnType) =
    (metavar, (args, returnType))

instance IsString MetavarBinder where
  fromString :: String -> MetavarBinder
  fromString = either error id . parseMetavarBinder

instance Show UnificationConstraint where
  show :: UnificationConstraint -> String
  show = Raw.printTree . fromUnificationConstraint

parseUnificationConstraint :: MetavarBinders -> String -> Either String UnificationConstraint
parseUnificationConstraint metavarBinders input = do
  let tokens = Raw.resolveLayout False (Raw.myLexer input)
  raw <- Raw.pUnificationConstraint tokens
  case toUnificationConstraint metavarBinders raw of
    Just uc -> pure uc
    Nothing -> Left "type error"

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
  -- Let value binder body
  --   | Foil.Distinct <- Foil.assertDistinct binder ->
  --       let subst = matchPattern binder value
  --        in nf scope (substitute scope subst body)
  t -> t

interpretCommand :: Raw.Command -> IO ()
interpretCommand (Raw.CommandCompute term) =
  print $ nf Foil.emptyScope $ toTermClosed term

interpretProgram :: Raw.Program -> IO ()
interpretProgram (Raw.AProgram commands) = mapM_ interpretCommand commands

-- ** Test framework implementation

parseMetavarBinders :: [String] -> Either String MetavarBinders
parseMetavarBinders rawMetavarBinders = do
  metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
  pure $ Map.fromList metavarBinders

matchUnificationConstraint
  :: [String]
  -> String
  -> Either String [MetaSubsts']
matchUnificationConstraint
  rawMetavarBinders
  rawUnificationConstraint = do
    metavarBindersMap <- parseMetavarBinders rawMetavarBinders
    (UnificationConstraint scope _ binderTypes lhs rhs) <-
      parseUnificationConstraint
        metavarBindersMap
        rawUnificationConstraint
    let substs = match scope metavarBindersMap binderTypes lhs rhs
    pure substs

solveUnificationConstraint
  :: MetaSubsts'
  -> UnificationConstraint
  -> UnificationConstraint
solveUnificationConstraint
  substs
  (UnificationConstraint scope binders binderTypes (lhs, lhsType) (rhs, rhsType)) =
    let solve = nfMetaTerm scope . applyMetaSubsts id scope substs
     in UnificationConstraint
          scope
          binders
          binderTypes
          (solve lhs, lhsType)
          (solve rhs, rhsType)

isSolvedUnificationConstraint
  :: [String]
  -> String
  -> [String]
  -> Either String Bool
isSolvedUnificationConstraint rawMetavarBinders rawUnificationConstraint rawMetaSubsts = do
  metavarBinders <- traverse parseMetavarBinder rawMetavarBinders
  let metavarBindersMap = Map.fromList metavarBinders
  (UnificationConstraint scope _ _ (lhs, _) (rhs, _)) <-
    parseUnificationConstraint metavarBindersMap rawUnificationConstraint
  metaSubsts <- traverse (parseMetaSubst metavarBindersMap) rawMetaSubsts
  let solve = nfMetaTerm scope . applyMetaSubsts id scope (MetaSubsts metaSubsts)
  pure $ alphaEquiv scope (solve lhs) (solve rhs)

isSolved :: UnificationConstraint -> Bool
isSolved (UnificationConstraint scope _binders _binderTypes (lhs, _) (rhs, _)) = alphaEquiv scope lhs rhs

-- Data types
data RawConfig = RawConfig
  { rawConfigLanguage :: Text
  , rawConfigFragment :: Text
  , rawConfigProblems :: [RawProblem]
  }
  deriving (Show, Generic)

data Config = Config
  { configLanguage :: Text
  , configFragment :: Text
  , configProblems :: [Problem]
  }
  deriving (Show, Generic)

data RawProblem = RawProblem
  { rawProblemMetavars :: [String]
  , rawProblemConstraints :: [String]
  , rawProblemSolutions :: [RawSolution]
  }
  deriving (Show, Generic)

data Problem = Problem
  { problemMetavars :: MetavarBinders
  , problemConstraints :: [UnificationConstraint]
  , problemSolutions :: [Solution]
  }
  deriving (Show, Generic)

data RawSolution = RawSolution
  { rawSolutionName :: Text
  , rawSolutionSubstitutions :: [String]
  }
  deriving (Show, Generic)

data Solution = Solution
  { solutionName :: Text
  , solutionSubstitutions :: MetaSubsts'
  }
  deriving (Show, Generic)

-- TomlCodecs
configCodec :: TomlCodec RawConfig
configCodec =
  RawConfig
    <$> Toml.text "language" .= rawConfigLanguage
    <*> Toml.text "fragment" .= rawConfigFragment
    <*> Toml.list problemCodec "problems" .= rawConfigProblems

problemCodec :: TomlCodec RawProblem
problemCodec =
  RawProblem
    <$> stringsCodec "metavars"
    <*> stringsCodec "constraints"
    <*> Toml.list solutionCodec "solutions" .= rawProblemSolutions

stringsCodec :: Toml.Key -> Toml.Codec object [String]
stringsCodec field =
  map TIO.unpack
    <$> Toml.arrayOf Toml._Text field .= error "no encode needed"

solutionCodec :: TomlCodec RawSolution
solutionCodec =
  RawSolution
    <$> Toml.text "name" .= rawSolutionName
    <*> stringsCodec "substitutions" .= rawSolutionSubstitutions

validateProblem :: Problem -> Either String ([(Solution, [UnificationConstraint])], [Solution])
validateProblem (Problem{..}) = do
  let results = map (validateSolution problemConstraints) problemSolutions
  pure (partitionEithers results)

parseRawProblem :: RawProblem -> Either String Problem
parseRawProblem RawProblem{..} = do
  metavarBindersList <- traverse parseMetavarBinder rawProblemMetavars
  let metavarBinders = Map.fromList metavarBindersList
  constraints <- traverse (parseUnificationConstraint metavarBinders) rawProblemConstraints
  solutions <- traverse (parseSolution metavarBinders) rawProblemSolutions
  pure (Problem metavarBinders constraints solutions)
 where
  parseSolution metavarBinders (RawSolution{..}) = do
    parsedSubsts <-
      traverse
        (parseMetaSubst metavarBinders)
        rawSolutionSubstitutions
    pure (Solution rawSolutionName (MetaSubsts parsedSubsts))

parseRawConfig :: RawConfig -> Either String Config
parseRawConfig RawConfig{..} = do
  problems <- traverse parseRawProblem rawConfigProblems
  pure (Config rawConfigLanguage rawConfigFragment problems)

validateSolution
  :: [UnificationConstraint]
  -> Solution
  -> Either (Solution, [UnificationConstraint]) Solution
validateSolution constraints solution@Solution{..} =
  let solvedConstraints =
        map (solveUnificationConstraint solutionSubstitutions) constraints
   in if all isSolved solvedConstraints
        then Right solution
        else Left (solution, solvedConstraints)

printInvalidSolutionsWithConstraint :: (Foldable t, Show a) => (Solution, t a) -> IO ()
printInvalidSolutionsWithConstraint (solution, constraints) = do
  putStrLn $ replicate 25 '-' <> "\n"
  putStrLn $ "Solution: " <> TIO.unpack (solutionName solution)
  putStrLn ""
  putStrLn "Substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) (getMetaSubsts (solutionSubstitutions solution))
  putStrLn ""
  putStrLn "Constraints with applied substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) constraints
  putStrLn ""
  putStrLn $ replicate 25 '-' <> "\n"

-- Main function to parse and print the configuration
data ValidationError
  = ConfigError Toml.TomlDecodeError
  | ProblemError String RawProblem
  deriving (Show)

parseConfigAndValidate :: IO ()
parseConfigAndValidate = do
  configResult <- Toml.decodeFileEither configCodec "config.toml"
  case configResult of
    Left errs -> do
      putStrLn "\n=== Configuration Errors ==="
      putStr $ formatTomlErrors errs
    Right cfg -> case parseRawConfig cfg of
      Left err -> putStrLn err
      Right Config{..} ->
        mapM_ processValidation configProblems
 where
  processValidation problem =
    case validateProblem problem of
      Left err -> printError err
      Right (invalid, valid) -> do
        printValidSolutions valid
        printInvalidSolutions invalid

  printValidSolutions valid = do
    putStrLn "\n=== Validated solutions ==="
    mapM_ (putStrLn . ("✓ " ++) . show . solutionName) valid

  printInvalidSolutions invalid = do
    putStrLn "\n=== Invalid solutions ==="
    forM_ invalid $ \(solution, constraints) -> do
      putStrLn $ "✗ " ++ show (solutionName solution)
      mapM_ (putStrLn . ("  - " ++) . show) constraints

  printError err = putStrLn $ "\n=== Error ===" ++ "\n" ++ show err

  -- Add helper for error formatting
  formatTomlErrors errs =
    unlines $
      "Configuration errors:" : map (\err -> "  - " ++ show err) errs

-- >>> rawMetavarBinders = ["M : [t] t -> t -> t", "N : [t] t -> t"]
-- >>> Right metavarBinders = parseMetavarBinders rawMetavarBinders
-- >>> Right lhs = parseMetaSubst metavarBinders "M[x] ↦ λy:t.N[x]"
-- >>> Right rhs = parseMetaSubst metavarBinders "M[x] ↦ λy:t.λz:t.x"
-- >>> (_, lhsAbs) = getMetaSubst lhs
-- >>> (_, rhsAbs) = getMetaSubst rhs
-- >>> Just (_, type_) = Map.lookup "M" metavarBinders
-- >>> matchMetaAbs metavarBinders type_ lhsAbs rhsAbs
-- [[N [x0] ↦ λ x2 : t . x0]]
matchMetaAbs
  :: ( UnifiablePattern binder
     , SinkableK binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK ext
     , TypedBinder binder Raw.Type
     , TypedSignature sig Raw.Type
     )
  => MetavarBinders
  -> Raw.Type
  -> MetaAbs binder (Sum (AnnSig Raw.Type sig) (MetaAppSig Raw.MetavarIdent)) Raw.Type
  -> MetaAbs binder (Sum (AnnSig Raw.Type sig) ext) Raw.Type
  -> [MetaSubsts binder (AnnSig Raw.Type sig) Raw.MetavarIdent ext Raw.Type]
matchMetaAbs
  metavarBinders
  type_
  (MetaAbs lhsBinderList lhsBinderTypes lhsTerm)
  (MetaAbs rhsBinderList rhsBinderTypes rhsTerm) =
    case Foil.unifyPatterns lhsBinderList rhsBinderList of
      Foil.SameNameBinders _ ->
        case Foil.assertDistinct lhsBinderList of
          Foil.Distinct ->
            let commonScope = Foil.extendScopePattern lhsBinderList Foil.emptyScope
             in match commonScope metavarBinders lhsBinderTypes (lhsTerm, type_) (rhsTerm, type_)
      Foil.RenameLeftNameBinder _ rename ->
        case Foil.assertDistinct rhsBinderList of
          Foil.Distinct ->
            let commonScope = Foil.extendScopePattern rhsBinderList Foil.emptyScope
                lhsTerm' = Foil.liftRM commonScope (Foil.fromNameBinderRenaming rename) lhsTerm
             in match commonScope metavarBinders rhsBinderTypes (lhsTerm', type_) (rhsTerm, type_)
      Foil.RenameRightNameBinder _ rename ->
        case Foil.assertDistinct lhsBinderList of
          Foil.Distinct ->
            let commonScope = Foil.extendScopePattern lhsBinderList Foil.emptyScope
                rhsTerm' = Foil.liftRM commonScope (Foil.fromNameBinderRenaming rename) rhsTerm
             in match commonScope metavarBinders lhsBinderTypes (lhsTerm, type_) (rhsTerm', type_)
      Foil.RenameBothBinders commonBinders rename1 rename2 -> undefined
      Foil.NotUnifiable ->
        -- Binders cannot be unified
        trace "here" []

main :: IO ()
main = parseConfigAndValidate

-- main = do
--   let rawMetavarBinder = "M : [t -> t, t] t"
--   let rawConstraint = "∀ f: t -> t, g: t. M[f, g] = f g"
--   let rawSubst = "M [x, y] ↦ x y"
--   print $ matchUnificationConstraint [rawMetavarBinder] rawConstraint
--   print $ isSolvedUnificationConstraint [rawMetavarBinder] rawConstraint [rawSubst]

-- $setup
-- >>> import Language.Lambda.Impl

-- | Test cases
-- >>> metavarBinders = ["X : [t, t] t"]
-- >>> constraint     = "∀ m: t, n: t. X[m, n] = n"
-- >>> substs         = ["X [x, y] ↦ y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint substs
-- Right [[X [x0, x1] ↦ x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t] t"]
-- >>> constraint     = "∀ f: t -> t, g: t. M[f, g] = f g"
-- >>> subst          = ["M [x, y] ↦ x y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0 x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t] t"]
-- >>> constraint     = "∀ f : t -> t, g : t. M[f, g] = g"
-- >>> subst          = ["M [x, y] ↦ y"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x1]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t -> t] t -> t"]
-- >>> constraint     = "∀ . M[λf:t. f, λg:t. g] = λy:t. y"
-- >>> subst          = ["M [x, y] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0],[M [x0, x1] ↦ x1],[M [x0, x1] ↦ λ x2 : t . x2]]
-- Right True
-- >>> metavarBinders = ["M : [t -> t, t -> t -> t] t -> t"]
-- >>> constraint     = "∀ . M[λx:t. x, λy:t. λy:t. y] = λa:t. a"
-- >>> subst          = ["M [x, y] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0, x1] ↦ x0],[M [x0, x1] ↦ λ x2 : t . x2]]
-- Right True
-- >>> metavarBinders = ["M : [] t -> t"]
-- >>> constraint     = "∀ . M[] = λx:t.x"
-- >>> subst          = ["M [] ↦ λ x:t.x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [] ↦ λ x0 : t . x0]]
-- Right True
-- >>> metavarBinders = []
-- >>> constraint     = "∀ . λx:t.x = λy:t.y"
-- >>> subst          = []
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[]]
-- Right True
-- >>> metavarBinders = []
-- >>> constraint     = "∀ . λx:t.λx:t.x = λy:t.y"
-- >>> subst          = []  -- no substitution can solve this constraint
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right []
-- Right False
-- >>> metavarBinders = ["M : [t -> t] t -> t"]
-- >>> constraint     = "∀ . λy:t.M[λx:t.x] = λy:t.λx:t.x"
-- >>> subst          = ["M [x] ↦ x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right [[M [x0] ↦ x0],[M [x0] ↦ λ x1 : t . x1]]
-- Right True

-- | Constraint couldn't be solved by matching
-- >>> metavarBinders = ["N : [t] t -> t"]
-- >>> constraint     = "∀ x : t, y : t. N[x] y = x"
-- >>> subst          = ["N [x] ↦ λz:t.x"]
-- >>> matchUnificationConstraint metavarBinders constraint
-- >>> isSolvedUnificationConstraint metavarBinders constraint subst
-- Right []
-- Right True
