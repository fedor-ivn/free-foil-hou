{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
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

import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.Internal as FoilInternal
import Control.Monad.Foil.TH
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.TH
import Data.Biapplicative (Bifunctor (bimap, first))
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
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

instance ZipMatchK Raw.MetaVarIdent where zipMatchWithK = zipMatchViaEq

instance ZipMatchK Raw.Type where zipMatchWithK = zipMatchViaEq

-- ** Pattern synonyms

pattern App' :: AST binder (Sum TermSig q) n -> AST binder (Sum TermSig q) n -> AST binder (Sum TermSig q) n
pattern App' f x = Node (L2 (AppSig f x))

pattern Lam' :: Raw.Type -> binder n l -> AST binder (Sum TermSig q) l -> AST binder (Sum TermSig q) n
pattern Lam' typ binder body = Node (L2 (LamSig typ (ScopedAST binder body)))

pattern Let' :: AST binder (Sum TermSig q) n -> binder n l -> AST binder (Sum TermSig q) l -> AST binder (Sum TermSig q) n
pattern Let' term binder body = Node (L2 (LetSig term (ScopedAST binder body)))

pattern MetaVar' :: Raw.MetaVarIdent -> [AST binder (Sum TermSig q) n] -> AST binder (Sum TermSig q) n
pattern MetaVar' metavar args = Node (L2 (MetaVarSig metavar args))

-- FV( (λ x. x) y )  =  { y }
--
-- λs. λz. s (s z)    :: Term VoidS
--     λz. s (s z)    :: Term n1      --  n1 ~ { s }
--         s (s z)    :: Term n2      --  n2 ~ { s, z }
-- λs                 :: NameBinder VoidS n1
--     λz             :: NameBinder n1 n2
--

-- * User-defined code

type AST' = AST FoilPattern

-- | Scope-safe λ-term representation in scope @n@.
type Term = AST' TermSig

type MetaTerm metavar n = SOAS FoilPattern metavar TermSig n

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

{-# COMPLETE Var, Lam', App', Let', MetaVar', MetaApp #-}

nfMetaTerm
  :: (Foil.Distinct n)
  => Foil.Scope n
  -> MetaTerm metavar n
  -> MetaTerm metavar n
nfMetaTerm scope = \case
  Var x -> Var x
  Lam' typ binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let extendedScope = Foil.extendScopePattern binder scope
         in Lam' typ binder (nfMetaTerm extendedScope body)
  App' f x ->
    case nfMetaTerm scope f of
      Lam' _typ binder body ->
        let subst = matchPattern binder x
         in nfMetaTerm scope (substitute scope subst body)
      f' -> App' f' (nfMetaTerm scope x)
  Let' value binder body ->
    let subst = matchPattern binder value
        body' = substitute scope subst body
     in nfMetaTerm scope body'
  MetaVar' metavar args -> MetaVar' metavar (map (nfMetaTerm scope) args)
  -- MetaSubst' metavar term -> MetaSubst' metavar (nfMetaTerm scope term)
  MetaApp metavar args -> MetaApp metavar (map (nfMetaTerm scope) args)

nfMetaTermWithEmptyScope
  :: SOAS FoilPattern metavar TermSig VoidS
  -> SOAS FoilPattern metavar TermSig VoidS
nfMetaTermWithEmptyScope = nfMetaTerm Foil.emptyScope

nameMapToSubsts :: Foil.NameMap i (e o) -> Foil.Substitution e i o
nameMapToSubsts nameMap =
  FoilInternal.UnsafeSubstitution $ FoilInternal.getNameMap nameMap

-- ** Conversion helpers for 'MetaSubst'

toMetaSubst :: Raw.MetaSubst -> MetaSubst'
toMetaSubst (Raw.AMetaSubst metavar vars term) =
  withMetaSubstVars vars Foil.emptyScope Map.empty NameBinderListEmpty Foil.emptyNameMap $ \scope env binderList binderTypes ->
    let term' = toTerm scope env (getTermFromScopedTerm term)
     in MetaSubst (metavar, MetaAbs binderList binderTypes (toMetaTerm term'))

withMetaSubstVars
  :: (Distinct n)
  => [Raw.Binder]
  -> Scope n
  -> Map Raw.VarIdent (Foil.Name n)
  -> NameBinderList i n
  -> Foil.NameMap n Raw.Type
  -> ( forall l
        . (Distinct l)
       => Scope l
       -> Map Raw.VarIdent (Foil.Name l)
       -> NameBinderList i l
       -> Foil.NameMap l Raw.Type
       -> r
     )
  -> r
withMetaSubstVars [] scope env binderList binderTypes cont = cont scope env binderList binderTypes
withMetaSubstVars (Raw.ABinder ident type_ : idents) scope env binderList binderTypes cont =
  withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        name = Foil.nameOf binder
        env' = Map.insert ident name (Foil.sink <$> env)
        binderList' = push binder binderList
        binderTypes' = Foil.addNameBinder binder type_ binderTypes
     in withMetaSubstVars idents scope' env' binderList' binderTypes' cont
 where
  push :: Foil.NameBinder i l -> NameBinderList n i -> NameBinderList n l
  push x NameBinderListEmpty = NameBinderListCons x NameBinderListEmpty
  push x (NameBinderListCons y ys) = NameBinderListCons y (push x ys)

type MetaSubst' =
  MetaSubst
    FoilPattern
    TermSig
    Raw.MetaVarIdent
    (MetaAppSig Raw.MetaVarIdent)
    Raw.Type

type MetaSubsts' =
  MetaSubsts
    FoilPattern
    TermSig
    Raw.MetaVarIdent
    (MetaAppSig Raw.MetaVarIdent)
    Raw.Type

fromMetaSubst :: MetaSubst' -> Raw.MetaSubst
fromMetaSubst (MetaSubst (metavar, MetaAbs binderList binderTypes term)) =
  let term' = Raw.AScopedTerm $ fromTerm $ fromMetaTerm term
      idents = toBinders binderList binderTypes
   in Raw.AMetaSubst metavar idents term'

toBinders :: (Foil.Distinct i) => NameBinderList i n -> Foil.NameMap n Raw.Type -> [Raw.Binder]
toBinders NameBinderListEmpty _binderTypes = []
toBinders (NameBinderListCons x xs) binderTypes
  | Foil.Distinct <- Foil.assertDistinct x
  , Foil.Distinct <- Foil.assertDistinct xs
  , Foil.Ext <- Foil.assertExt xs =
      let ident = Raw.VarIdent $ "x" ++ show (Foil.nameOf x)
          typ = Foil.lookupName (Foil.sink (Foil.nameOf x)) binderTypes
       in Raw.ABinder ident typ : toBinders xs binderTypes

data UnificationConstraint where
  UnificationConstraint
    :: (Distinct n)
    => Scope n
    -> NameBinderList Foil.VoidS n
    -> Foil.NameMap n Raw.Type
    -> MetaTerm Raw.MetaVarIdent n
    -> MetaTerm Raw.MetaVarIdent n
    -> UnificationConstraint

toUnificationConstraint :: Raw.UnificationConstraint -> UnificationConstraint
toUnificationConstraint (Raw.AUnificationConstraint vars lhs rhs) =
  withMetaSubstVars vars Foil.emptyScope Map.empty NameBinderListEmpty Foil.emptyNameMap $ \scope env binders binderTypes ->
    let toMetaTerm' = toMetaTerm . toTerm scope env . getTermFromScopedTerm
     in UnificationConstraint scope binders binderTypes (toMetaTerm' lhs) (toMetaTerm' rhs)

fromUnificationConstraint :: UnificationConstraint -> Raw.UnificationConstraint
fromUnificationConstraint (UnificationConstraint _ binders binderTypes lhs rhs) =
  let fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm
   in Raw.AUnificationConstraint (toBinders binders binderTypes) (fromMetaTerm' lhs) (fromMetaTerm' rhs)

-- ** Conversion helpers for 'MetaTerm'

toMetaTerm :: Term n -> MetaTerm Raw.MetaVarIdent n
toMetaTerm = \case
  MetaVar metavar args -> MetaApp metavar (map toMetaTerm args)
  Var name -> Var name
  Node node -> Node (L2 (bimap toMetaScopedTerm toMetaTerm node))
 where
  toMetaScopedTerm (ScopedAST binder body) = ScopedAST binder (toMetaTerm body)

fromMetaTerm :: MetaTerm Raw.MetaVarIdent n -> Term n
fromMetaTerm = \case
  Var name -> Var name
  Node (R2 (MetaAppSig metavar args)) -> MetaVar metavar (map fromMetaTerm args)
  Node (L2 node) -> Node (bimap fromMetaScopedTerm fromMetaTerm node)
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

-- >>> lam (Raw.Base (Raw.VarIdent "t")) Foil.emptyScope (\x -> App (MetaVar (Raw.MetaVarIdent "X") []) (Var x))
-- λ x0 : t . X [] x0
lam :: (Distinct n) => Raw.Type -> Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> Term l) -> Term n
lam typ scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam typ (FoilAPattern x') (makeBody x)

lam' :: (Distinct n) => Raw.Type -> Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> MetaTerm metavar l) -> MetaTerm metavar n
lam' typ scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam' typ (FoilAPattern x') (makeBody x)

-- >>> lam (Raw.Base (Raw.VarIdent "t")) Foil.emptyScope (\x -> App (Var x) (Var x))
-- λ x0 : t . x0 x0
instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- >>> "λy:t.(λx:u.λy:v.x)y" :: Term Foil.VoidS
-- λ x0 : t . (λ x1 : u . λ x2 : v . x1) x0
instance IsString (Term Foil.VoidS) where
  fromString :: String -> Term VoidS
  fromString = unsafeParseTerm

instance Show (MetaTerm Raw.MetaVarIdent n) where
  show :: MetaTerm Raw.MetaVarIdent n -> String
  show = Raw.printTree . fromTerm . fromMetaTerm

-- >>> "λy:t.(λx:u.λy:v.X[x, y X[y, x]])y" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- λ x0 : t . (λ x1 : u . λ x2 : v . X [x1, x2 X [x2, x1]]) x0
instance IsString (MetaTerm Raw.MetaVarIdent Foil.VoidS) where
  fromString :: String -> MetaTerm Raw.MetaVarIdent VoidS
  fromString = toMetaTerm . unsafeParseTerm

-- >>> "X [ x: t, y: u, z: v ] ↦ λy:t.(λx:t.λy:u.X[x, y X[y, x]])y" :: MetaSubst'
-- X [x0 : t, x1 : u, x2 : v] ↦ λ x3 : t . (λ x4 : t . λ x5 : u . X [x4, x5 X [x5, x4]]) x3
instance Show MetaSubst' where
  show :: MetaSubst' -> String
  show = Raw.printTree . fromMetaSubst

instance Show MetaSubsts' where
  show :: MetaSubsts' -> String
  show = show . getMetaSubsts

-- >>> "X [ x: a, y: u ] ↦ λ x : t. y" :: MetaSubst'
-- X [x0 : a, x1 : u] ↦ λ x2 : t . x1
instance IsString MetaSubst' where
  fromString :: String -> MetaSubst'
  fromString = unsafeParseMetaSubst

unsafeParseTerm :: String -> Term Foil.VoidS
unsafeParseTerm input =
  case Raw.pTerm tokens of
    Left err -> error err
    Right term -> toTermClosed term
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)

parseMetaSubst :: String -> Either String MetaSubst'
parseMetaSubst input =
  let tokens = Raw.resolveLayout False (Raw.myLexer input)
   in toMetaSubst <$> Raw.pMetaSubst tokens

unsafeParseMetaSubst :: String -> MetaSubst'
unsafeParseMetaSubst = either error id . parseMetaSubst

-- >>> "∀ m: t, n: u. Y[m, X[n, m]] = (λ x: t. m (x n)) m" :: UnificationConstraint
-- ∀ x0 : t, x1 : u . Y [x0, X [x1, x0]] = (λ x2 : t . x0 (x2 x1)) x0
instance IsString UnificationConstraint where
  fromString :: String -> UnificationConstraint
  fromString = unsafeParseUnificationConstraint

instance Show UnificationConstraint where
  show :: UnificationConstraint -> String
  show = Raw.printTree . fromUnificationConstraint

unsafeParseUnificationConstraint :: String -> UnificationConstraint
unsafeParseUnificationConstraint input =
  case Raw.pUnificationConstraint tokens of
    Left err -> error err
    Right problem -> toUnificationConstraint problem
 where
  tokens = Raw.resolveLayout False (Raw.myLexer input)

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
-- >>> nf Foil.emptyScope "let f = λ a : (t -> t) . λ x : (u -> u) . x in f (λ a : t . a) (λ a : u . a)"
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
  Let value binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let subst = matchPattern binder value
         in nf scope (substitute scope subst body)
  t -> t

interpretCommand :: Raw.Command -> IO ()
interpretCommand (Raw.CommandCompute term) =
  print $ nf Foil.emptyScope $ toTermClosed term

interpretProgram :: Raw.Program -> IO ()
interpretProgram (Raw.AProgram commands) = mapM_ interpretCommand commands

-- ** Test framework implementation

-- >>> constraint = "∀ g:t1, a:t2, w:t3. X[g, λz:t4. z a] = g a" :: UnificationConstraint
-- >>> subst = "X[x : t2, y : t3] ↦ (λ z : t1 . y z) x" :: MetaSubst'
-- >>> isSolved (solveUnificationConstraint (MetaSubsts [subst]) constraint)
-- True
-- >>> constraint1 = "∀ f:t, x:t . X[f, x] = f Y[x]" :: UnificationConstraint
-- >>> constraint2 = "∀ x:t . Y[x] = x x" :: UnificationConstraint
-- >>> subst1 = "Y[x:t] ↦ x x" :: MetaSubst'
-- >>> subst2 = "X[f:t, x:t] ↦ f (x x)" :: MetaSubst'
-- >>> subst3 = "M[x:t, y:t] ↦ y x" :: MetaSubst'
-- >>> all (isSolved . solveUnificationConstraint (MetaSubsts [subst1, subst2, subst3])) [constraint1, constraint2]
-- True

solveUnificationConstraint
  :: MetaSubsts FoilPattern TermSig Raw.MetaVarIdent (MetaAppSig Raw.MetaVarIdent) Raw.Type
  -> UnificationConstraint
  -> UnificationConstraint
solveUnificationConstraint substs (UnificationConstraint scope binders binderTypes lhs rhs) =
  let solve = nfMetaTerm scope . applyMetaSubsts id scope substs
   in UnificationConstraint scope binders binderTypes (solve lhs) (solve rhs)

isSolved :: UnificationConstraint -> Bool
isSolved (UnificationConstraint scope _binders _binderTypes lhs rhs) = alphaEquiv scope lhs rhs

-- Data types
data Config = Config
  { configLanguage :: Text
  , configFragment :: Text
  , configProblems :: [Problem]
  }
  deriving (Show, Generic)

data Problem = Problem
  { problemConstraints :: [UnificationConstraint]
  , problemSolutions :: [Solution]
  }
  deriving (Show, Generic)

data Solution = Solution
  { solutionName :: Text
  , solutionSubstitutions :: [MetaSubst']
  }
  deriving (Show, Generic)

-- TomlCodecs
configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.text "language" .= configLanguage
    <*> Toml.text "fragment" .= configFragment
    <*> Toml.list problemCodec "problems" .= configProblems

problemCodec :: TomlCodec Problem
problemCodec =
  Problem
    <$> constraintsCodec
    <*> Toml.list solutionCodec "solutions" .= problemSolutions
 where
  constraintsCodec =
    map (unsafeParseUnificationConstraint . TIO.unpack)
      <$> Toml.arrayOf Toml._Text "constraints" .= error "no encode needed"

parseTextToEither :: (String -> Either String a) -> Text -> Either Text a
parseTextToEither parse = first fromString . parse . TIO.unpack

solutionCodec :: TomlCodec Solution
solutionCodec =
  Solution
    <$> Toml.text "name" .= solutionName
    <*> substitutionsCodec
 where
  substitutionsCodec =
    map (unsafeParseMetaSubst . TIO.unpack)
      <$> Toml.arrayOf Toml._Text "substitutions" .= error "no encode needed"

toCodec :: (Show a) => (String -> Either String a) -> Toml.Key -> TomlCodec a
toCodec parseString =
  let parseText = first fromString . parseString . TIO.unpack
      showText = TIO.pack . show
   in Toml.textBy showText parseText

validateProblem :: Problem -> ([(Solution, [UnificationConstraint])], [Solution])
validateProblem (Problem constraints solutions) =
  let results = map (validateSolution constraints) solutions
   in partitionEithers results

-- Helper function to check if a constraint is solved by a specific solution
validateSolution :: [UnificationConstraint] -> Solution -> Either (Solution, [UnificationConstraint]) Solution
validateSolution constraints solution =
  let substs = MetaSubsts (solutionSubstitutions solution)
      constraints' = map (solveUnificationConstraint substs) constraints
   in if all isSolved constraints'
        then Right solution
        else Left (solution, constraints')

printInvalidSolutionsWithConstraint :: (Foldable t, Show a) => (Solution, t a) -> IO ()
printInvalidSolutionsWithConstraint (solution, constraints) = do
  putStrLn $ replicate 25 '-' <> "\n"
  putStrLn $ "Solution: " <> TIO.unpack (solutionName solution)
  putStrLn ""
  putStrLn "Substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) (solutionSubstitutions solution)
  putStrLn ""
  putStrLn "Constraints with applied substitutions:"
  mapM_ (putStrLn . ("- " ++) . show) constraints
  putStrLn ""
  putStrLn $ replicate 25 '-' <> "\n"

-- Main function to parse and print the configuration
parseConfigAndValidate :: IO ()
parseConfigAndValidate = do
  configResult <- Toml.decodeFileEither configCodec "config.toml"
  case configResult of
    Left err -> print err
    Right cfg -> mapM_ validateAndPrintProblem (configProblems cfg)
 where
  validateAndPrintProblem :: Problem -> IO ()
  validateAndPrintProblem problem = do
    let (invalidSolutionsWithConstraints, validatedSolutions) = validateProblem problem
    putStrLn "=== Validated solutions ==="
    mapM_ (putStrLn . ("- " ++) . show . solutionName) validatedSolutions
    putStrLn "\n=== Invalid solutions ===\n"
    mapM_ printInvalidSolutionsWithConstraint invalidSolutionsWithConstraints

main :: IO ()
-- main = parseConfigAndValidate
main = do
  let lhs = "λy:t. M[]" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
  let rhs = "λy:t. λx:t. x" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
  let res = match Foil.emptyScope lhs rhs :: [MetaSubsts']
  print res

-- >>> lhs = "M[]" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> rhs = "λx:t.x" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> match Foil.emptyScope lhs rhs :: [MetaSubsts']
-- [[M [] ↦ λ x0 : t . x0]]

-- >>> lhs = "λx : t. x" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> rhs = "λy : t. y" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> match Foil.emptyScope lhs lhs :: [MetaSubsts']
-- []

-- >>> lhs = "λy:t. M[]" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> rhs = "λy:t. λx:t. x" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> match Foil.emptyScope lhs rhs :: [MetaSubsts']
-- []

