{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Lambda.FCU.FreeFoil.Syntax where

import Control.Monad.Foil qualified as Foil
import Control.Monad.Foil.Internal as FoilInternal hiding (Substitution)
import Control.Monad.Foil.Relative (liftRM)
import Control.Monad.Foil.TH
  ( deriveCoSinkable,
    deriveUnifiablePattern,
    mkFoilPattern,
    mkFromFoilPattern,
    mkToFoilPattern,
  )
import Control.Monad.Free.Foil
  ( AST (..),
    ScopedAST (ScopedAST),
    alphaEquiv,
    convertFromAST,
    convertToAST,
    substitute,
  )
import Control.Monad.Free.Foil.TH
  ( mkConvertFromFreeFoil,
    mkConvertToFreeFoil,
    mkPatternSynonyms,
    mkSignature,
  )
import Data.Biapplicative (Bifunctor (bimap))
import Data.Bifoldable (bifoldr, biany)
import Data.Bifunctor.Sum (Sum (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.SOAS hiding (SOAS)
import Data.String (IsString (..))
import Data.ZipMatchK
import Data.ZipMatchK.Bifunctor ()
import GHC.Generics qualified as GHC
import Generics.Kind.TH (deriveGenericK)
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Lex qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Par qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Print qualified as Raw
import Language.Lambda.FCU.Terms (showRaw)

-- * Generated code

-- ** Signature

mkSignature ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms

mkPatternSynonyms ''TermSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern

-- ** Scope-safe patterns

mkFoilPattern ''Raw.Id ''Raw.Pattern
deriveCoSinkable ''Raw.Id ''Raw.Pattern
mkToFoilPattern ''Raw.Id ''Raw.Pattern
mkFromFoilPattern ''Raw.Id ''Raw.Pattern

deriveUnifiablePattern ''Raw.Id ''Raw.Pattern

-- | Deriving 'GHC.Generic' and 'GenericK' instances.
deriving instance GHC.Generic (TermSig scope term)

deriveGenericK ''TermSig
instance ZipMatchK Raw.MetavarId where zipMatchWithK = zipMatchViaEq
instance ZipMatchK Raw.ConstructorId where zipMatchWithK = zipMatchViaEq
instance ZipMatchK TermSig

data AnnBinder ann binder (n :: Foil.S) (l :: Foil.S)
  = AnnBinder (binder n l) ann
  deriving (GHC.Generic)

deriveGenericK ''AnnBinder

instance (Foil.CoSinkable binder) => Foil.CoSinkable (AnnBinder ann binder) where
  coSinkabilityProof rename (AnnBinder binder ann) cont =
    Foil.coSinkabilityProof rename binder (\rename' binder' -> cont rename' (AnnBinder binder' ann))

  withPattern f empty append scope (AnnBinder binder t) cont =
    Foil.withPattern f empty append scope binder (\f' binder' -> cont f' (AnnBinder binder' t))

instance (Foil.SinkableK binder) => Foil.SinkableK (AnnBinder ann binder)

-- -- | Match 'Raw.Ident' via 'Eq'.
-- instance ZipMatchK Raw.Ident where zipMatchWithK = zipMatchViaEq

-- | Ignore 'Raw.BNFC'Position' when matching terms.
-- instance ZipMatchK Raw.BNFC'Position where zipMatchWithK = zipMatchViaChooseLeft

-- | Generic 'ZipMatchK' instance.
-- instance ZipMatchK TermSig

-- instance ZipMatch TermSig where
--   zipMatch = genericZipMatch2

-- -- * User-defined code

-- | Generic annotated scope-safe \(\lambda\Pi\)-terms with patterns.
type Term = AST FoilPattern TermSig

-- | Scode-safe \(\lambda\Pi\)-terms annotated with source code position.
-- type Term = Term Raw.BNFC'Position

-- | Scope-safe patterns annotated with source code position.
-- type FoilPattern = FoilPattern Raw.BNFC'Position

-- ** Conversion helpers

-- | Convert 'Raw.Term' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.Id (Foil.Name n) -> Raw.Term -> Term n
toTerm = convertToAST convertToTermSig toFoilPattern getTermFromScopedTerm

-- | Convert 'Raw.Term' into a closed scope-safe term.
-- This is a special case of 'toTerm'.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

-- | Convert a scope-safe representation back into 'Raw.Term'.
-- This is a special case of 'convertFromAST'.
--
-- 'Raw.Id' names are generated based on the raw identifiers in the underlying foil representation.
--
-- This function does not recover location information for variables, patterns, or scoped terms.
fromTerm :: Term n -> Raw.Term
fromTerm =
  convertFromAST
    convertFromTermSig
    Raw.OTerm
    (fromFoilPattern mkId)
    Raw.ScopedTerm
    mkId
  where
    mkId n = Raw.Id ("x" ++ show n)

-- | Parse scope-safe terms via raw representation.
-- >>> fromString "λx.λy.λx.x" :: Term Foil.VoidS
-- λ x0 . λ x1 . λ x2 . x2
instance IsString (AST FoilPattern TermSig Foil.VoidS) where
  fromString input = case Raw.pTerm (Raw.tokens input) of
    Left err -> error ("could not parse λΠ-term: " <> input <> "\n  " <> err)
    Right term -> toTermClosed term

-- | Pretty-print scope-safe terms via raw representation.
instance Show (AST FoilPattern TermSig Foil.VoidS) where
  show = Raw.printTree . fromTerm

-- * Unification test

pattern Lam' ::
  binder n l ->
  AST binder (AnnSig typ (Sum TermSig q)) l ->
  typ ->
  AST binder (AnnSig typ (Sum TermSig q)) n
pattern Lam' binder body returnType =
  Node (AnnSig (L2 (AbsTermSig (ScopedAST binder body))) returnType)

pattern App' ::
  AST binder (AnnSig typ (Sum TermSig q)) n ->
  AST binder (AnnSig typ (Sum TermSig q)) n ->
  typ ->
  AST binder (AnnSig typ (Sum TermSig q)) n
pattern App' f x typ = Node (AnnSig (L2 (AppTermSig f x)) typ)

pattern MetaVar' ::
  Raw.MetavarId ->
  typ ->
  AST binder (AnnSig typ (Sum TermSig q)) n
pattern MetaVar' metavar typ = Node (AnnSig (L2 (WTermSig metavar)) typ)

-- | Generalized term representation
type Sig typ metavar binder sig n =
  sig (TypedScopedSOAS binder metavar sig n typ) (TypedSOAS binder metavar sig n typ)

data Constraint typ metavar binder sig where
  Constraint ::
    (Foil.Distinct n) =>
    NameBinderList VoidS n ->
    NameMap n typ ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ ->
    Constraint typ metavar binder sig

-- | Substitutions representations
data Substitution typ metavar binder sig where
  Substitution ::
    metavar ->
    NameBinderList VoidS n ->
    TypedSOAS binder metavar sig n typ ->
    Substitution typ metavar binder sig

newtype Substitutions typ metavar binder sig = Substitutions [Substitution typ metavar binder sig]

-- | Generalized FCU typeclass
class
  ( Eq typ,
    Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig
  ) =>
  FCUUnifiable typ metavar binder sig
  where
  -- | Check if a term is flexible
  isFlexible :: Sig typ metavar binder sig n -> Bool

  -- | Apply a single substitution to a term
  applySubstitution ::
    (Distinct n) =>
    Substitution typ metavar binder sig ->
    Scope n ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ

  -- | Apply multiple substitutions to a term
  applySubstitutions ::
    (Distinct n) =>
    Substitutions typ metavar binder sig ->
    Scope n ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ

  -- | Perform a single beta reduction step
  betaReduceOnce ::
    (Distinct n) =>
    Scope n ->
    TypedSOAS binder metavar sig n typ ->
    Maybe (TypedSOAS binder metavar sig n typ)

  -- | Full beta reduction to normal form
  betaReduce ::
    (Distinct n) =>
    Scope n ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ

  -- | Combined substitution application and beta reduction
  devar ::
    (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Foil.Distinct n) =>
    Substitutions typ metavar binder sig ->
    Foil.Scope n ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ

  -- | Strip a term to separate the head and arguments
  strip ::
    TypedSOAS binder metavar sig n typ ->
    (TypedSOAS binder metavar sig n typ, [TypedSOAS binder metavar sig n typ])

devar' ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Foil.Distinct n) =>
  Substitutions typ metavar binder sig ->
  Foil.Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
devar' (Substitutions substs) scope term =
  betaReduce' scope (foldr (`applySubstitution'` scope) term substs)

betaReduce' ::
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
betaReduce' scope term = case betaReduceOnce' scope term of
  Nothing -> term
  Just reduced -> betaReduce' scope reduced

betaReduceOnce' ::
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  Maybe (TypedSOAS binder metavar sig n typ)
betaReduceOnce' scope term = case term of
  Var {} -> Nothing
  MetaApp metavar args _ -> Nothing

toNameMap ::
  NameMap m a ->
  NameBinderList m l ->
  [a] ->
  NameMap l a
toNameMap nameMap NameBinderListEmpty [] = nameMap
toNameMap nameMap (NameBinderListCons binder rest) (x : xs) =
  toNameMap (addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

-- | Apply a substitution to a term
applySubstitution' ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Foil.Distinct n) =>
  Substitution typ metavar binder sig ->
  Foil.Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
applySubstitution' substitution scope node = case node of
  Var {} -> node
  MetaApp meta arguments _
    | Substitution expectedMeta parameters body <- substitution,
      meta == expectedMeta ->
        let nameMap = toNameMap emptyNameMap parameters arguments
            substs' = nameMapToSubstitution nameMap
         in substitute scope substs' body
  MetaApp meta parameters typ ->
    MetaApp meta (applySubstitution' substitution scope <$> parameters) typ
  Node term -> Node (bimap goScoped go term)
    where
      go = applySubstitution' substitution scope
      goScoped (ScopedAST binder term)
        | Distinct <- assertDistinct binder =
            ScopedAST binder (applySubstitution' substitution scope' term)
        where
          scope' = extendScopePattern binder scope

applySubstitutions' ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n) =>
  Substitutions typ metavar binder sig ->
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
applySubstitutions' (Substitutions []) _ term = term
applySubstitutions' (Substitutions (subst : substs)) scope term =
  applySubstitutions' (Substitutions substs) scope (applySubstitution' subst scope term)

combineSubstitutions ::
  Substitutions typ metavar binder sig ->
  Substitutions typ metavar binder sig ->
  Substitutions typ metavar binder sig
combineSubstitutions (Substitutions s1) (Substitutions s2) = Substitutions (s1 ++ s2)

instance (SinkableK FoilPattern, ZipMatchK TermSig) => FCUUnifiable () Raw.MetavarId (AnnBinder () FoilPattern) TermSig where
  isFlexible node = case node of
    WTermSig {} -> True
    _ -> False

  betaReduceOnce = betaReduceOnce'
  betaReduce = betaReduce'
  applySubstitution = applySubstitution'
  applySubstitutions = applySubstitutions'
  devar = devar'

data Stream a = Stream a (Stream a) deriving (Eq, GHC.Generic)

-- | Type of a metavariable with its parameter types and return type
data MetaType typ = MetaType [typ] typ deriving (Eq, GHC.Generic)

data Metavariables typ metavar = Metavariables
  { metavariableTypes :: [(metavar, MetaType typ)],
    freshMetavarStream :: Stream metavar
  }

data Problem typ metavar binder sig = Problem
  { problemMetavariables :: Metavariables typ metavar,
    problemConstraints :: [Constraint typ metavar binder sig]
  }

data Solution typ metavar binder sig = Solution
  { solutionMetavariables :: Metavariables typ metavar,
    solutionConstraints :: [Constraint typ metavar binder sig],
    solutionSubstitutions :: Substitutions typ metavar binder sig
  }

strip' :: TypedSOAS binder Raw.MetavarId TermSig n typ -> (TypedSOAS binder Raw.MetavarId TermSig n typ, [TypedSOAS binder Raw.MetavarId TermSig n typ])
strip' term = case term of
  Node (AnnSig (L2 (AppTermSig f x)) _) ->
    let (head, args) = strip' f
     in (head, args ++ [x])
  MetaApp metavar args typ -> (Node (AnnSig (L2 (WTermSig metavar)) typ), args)
  _ -> (term, [])

unify ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK (Sum TermSig (MetaAppSig Raw.MetavarId)), ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (TypedSOAS binder Raw.MetavarId TermSig n typ, TypedSOAS binder Raw.MetavarId TermSig n typ)) ->
  Substitutions typ Raw.MetavarId binder TermSig
unify scope (th, (s, t)) = case (devar' th scope s, devar' th scope t) of
  (Lam' binder1 body1 _, Lam' binder2 body2 _) -> case Foil.unifyPatterns binder1 binder2 of
    Foil.NotUnifiable -> error "Not unifiable"
    Foil.RenameLeftNameBinder _ rename ->
      case Foil.assertDistinct binder2 of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder2 scope
              lhsTerm' = liftRM scope' (Foil.fromNameBinderRenaming rename) body1
           in unify scope' (th, (lhsTerm', body2))
    Foil.RenameRightNameBinder _ rename ->
      case Foil.assertDistinct binder1 of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder1 scope
              rhsTerm' = liftRM scope' (Foil.fromNameBinderRenaming rename) body2
           in unify scope' (th, (body1, rhsTerm'))
    _ -> error "Not unifiable"
  (s', t') -> cases scope (th, (s', t'))

cases ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK (Sum TermSig (MetaAppSig Raw.MetavarId)), ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (TypedSOAS binder Raw.MetavarId TermSig n typ, TypedSOAS binder Raw.MetavarId TermSig n typ)) ->
  Substitutions typ Raw.MetavarId binder TermSig
cases scope (th, (s, t)) = case (strip' s, strip' t) of
  ((MetaVar' _ _, sn), (App' {}, _)) -> caseFlexRigid scope (th, (s, sn, t))
  ((App' {}, _), (MetaVar' _ _, sn)) -> caseFlexRigid scope (th, (t, sn, s))
  ((MetaVar' metavar1 _, sn), (MetaVar' metavar2 _, tn)) -> caseFlexFlex scope (th, (metavar1, sn, metavar2, tn))
  ((Var a, []), (Var b, [])) -> if a == b then th else error "Not unifiable"
  ((a, sn), (b, tm)) -> caseRigidRigid scope (th, (a, sn, b, tm))

caseRigidRigid ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK (Sum TermSig (MetaAppSig Raw.MetavarId)), ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (TypedSOAS binder Raw.MetavarId TermSig n typ, [TypedSOAS binder Raw.MetavarId TermSig n typ], TypedSOAS binder Raw.MetavarId TermSig n typ, [TypedSOAS binder Raw.MetavarId TermSig n typ])) ->
  Substitutions typ Raw.MetavarId binder TermSig
caseRigidRigid scope (th, (a, sn, b, tm)) = case (a, b) of
  (Var x, Var y) -> applicableCase (x, y)
  _ -> error "Not unifiable in (2) rule"
  where
    applicableCase (x, y) =
      if x == y && length sn == length tm
        then foldl (\th' (s, t) -> unify scope (th', (s, t))) th (zip sn tm)
        else error "Not unifiable in (2) rule"

caseFlexRigid ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (TypedSOAS binder Raw.MetavarId TermSig n typ, [TypedSOAS binder Raw.MetavarId TermSig n typ], TypedSOAS binder Raw.MetavarId TermSig n typ)) ->
  Substitutions typ Raw.MetavarId binder TermSig
caseFlexRigid scope (th, (MetaVar' metavar _, args, rigid))
  | not (argumentRestriction args) = error "Argument restriction fail at flexrigid case"
  | not (localRestriction scope args) = error "Local restriction fail at flexrigid case"
  | otherwise = combineSubstitutions (combineSubstitutions th pruningResult) newMetavarSubs
  where
    pruningResult = prune scope args (th, rigid)
    newMetavarSubs = Substitutions []
caseFlexRigid _ _ = error "Error at flexrigid case"

-- | Helper to fold over a list of terms with an accumulator
foldlN ::
  Foil.Scope n ->
  [TypedSOAS binder Raw.MetavarId TermSig n typ] ->
  (Foil.Scope n -> (Substitutions typ Raw.MetavarId binder TermSig, TypedSOAS binder Raw.MetavarId TermSig n typ) -> Substitutions typ Raw.MetavarId binder TermSig) ->
  (Substitutions typ Raw.MetavarId binder TermSig, [TypedSOAS binder Raw.MetavarId TermSig n typ]) ->
  Substitutions typ Raw.MetavarId binder TermSig
foldlN _ _ _ (rho, []) = rho
foldlN scope args f (rho, t : ts) = foldlN scope args f (f scope (rho, t), ts)

prune ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder) =>
  Scope n ->
  [TypedSOAS binder Raw.MetavarId TermSig n typ] ->
  (Substitutions typ Raw.MetavarId binder TermSig, TypedSOAS binder Raw.MetavarId TermSig n typ) ->
  Substitutions typ Raw.MetavarId binder TermSig
prune scope args (rho, term) = case strip' (devar' rho scope term) of
  (_, []) -> rho
  (App' {}, rr) -> foldlN scope args (`prune` args) (rho, rr)
  -- (Var x, rr) -> if Var x `elem` args
  --   then foldlN scope args (`prune` args) (rho, rr)
  --   else error "Prune: unexpected case"
  -- (MetaVar' metavar _, sm) -> if all (`elem` args) sm
  --   then rho
  --   else
  --       let vsm = mkvars sm
  --           newSubst = Substitutions [(_W, hnf (vsm, Raw.WTerm (newMetaVarId (Raw.MetavarId _W)), eqsel vsm tn sm))]
  --        in combineSubstitutions newSubst rho
  -- (Lam' binder body _, _) ->
  --   -- Extend the scope with the new binder and continue pruning
  --   case assertDistinct binder of
  --     Distinct ->
  --       let scope' = extendScopePattern binder scope
  --        in prune scope' args (th, body)
  _ -> error "Prune: unexpected case"

generateFreshMetavar :: Raw.MetavarId -> Raw.MetavarId
generateFreshMetavar (Raw.MetavarId meta) = Raw.MetavarId (meta ++ "'")

caseFlexFlex ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK (Sum TermSig (MetaAppSig Raw.MetavarId)), ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (Raw.MetavarId, [TypedSOAS binder Raw.MetavarId TermSig n typ], Raw.MetavarId, [TypedSOAS binder Raw.MetavarId TermSig n typ])) ->
  Substitutions typ Raw.MetavarId binder TermSig
caseFlexFlex scope (th, (meta1, sn, meta2, tn))
  | not (argumentRestriction sn) = error "Global restriction fail at flexflex case"
  | not (argumentRestriction tn) = error "Global restriction fail at flexflex case"
  | not (localRestriction scope sn) = error "Local restriction fail at flexflex case"
  | not (localRestriction scope tn) = error "Local restriction fail at flexflex case"
  | meta1 == meta2 = caseFlexFlexSame scope (th, (meta1, sn, tn))
  | otherwise = caseFlexFlexDiff scope (th, (meta1, meta2, sn, tn))

caseFlexFlexSame ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder, ZipMatchK (Sum TermSig (MetaAppSig Raw.MetavarId)), ZipMatchK typ) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (Raw.MetavarId, [TypedSOAS binder Raw.MetavarId TermSig n typ], [TypedSOAS binder Raw.MetavarId TermSig n typ])) ->
  Substitutions typ Raw.MetavarId binder TermSig
caseFlexFlexSame scope (th, (meta, sn, tn))
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | all (uncurry (alphaEquiv scope)) (zip sn tn) = th
  | otherwise =
      let metaNew = generateFreshMetavar meta
       in Substitutions [] -- placeholder

caseFlexFlexDiff ::
  (Eq Raw.MetavarId, CoSinkable binder, SinkableK binder, Distinct n, UnifiablePattern binder) =>
  Scope n ->
  (Substitutions typ Raw.MetavarId binder TermSig, (Raw.MetavarId, Raw.MetavarId, [TypedSOAS binder Raw.MetavarId TermSig n typ], [TypedSOAS binder Raw.MetavarId TermSig n typ])) ->
  Substitutions typ Raw.MetavarId binder TermSig
caseFlexFlexDiff scope (th, (meta1, meta2, sn, tn)) = Substitutions [] -- placeholder

-- >>> :set -XOverloadedStrings
-- >>> let var = Var undefined :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> argumentRestriction [var]
-- True

-- >>> let app = App' (Var undefined) (App' (Var undefined) (Var undefined) ()) () :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> argumentRestriction [app]
-- True

-- >>> let meta = MetaVar' (Raw.MetavarId "X") () :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> argumentRestriction [meta]
-- False

-- >>> let app = App' (Var undefined) (App' (Var undefined) (Var undefined) ()) () :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> let meta = MetaVar' (Raw.MetavarId "X") () :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> let var = Var undefined :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS ()
-- >>> argumentRestriction [var, app, meta]
-- False

-- | Check if the arguments of a term are all restricted terms
argumentRestriction :: [TypedSOAS binder Raw.MetavarId TermSig n typ] -> Bool
argumentRestriction tn = and [isRTerm t | t <- tn]
  where
    isRTerm :: TypedSOAS binder Raw.MetavarId TermSig n typ -> Bool
    isRTerm Var {} = True
    isRTerm (App' _ x _) = isRTerm x
    isRTerm _ = False

-- | Check local restriction:
-- for all occurrences Xtn in S where X ∈Q∃and for each ti and tj such that 0 <i,j ≤n and i̸= j, ti ̸⊑tj.
localRestriction ::
  (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder) =>
  Scope n ->
  [AST binder sig n] ->
  Bool
localRestriction scope args =
  and
    [ not (isSubTerm scope t1 t2) && not (isSubTerm scope t2 t1)
      | (t1, i1) <- zip args ([0 ..] :: [Int]),
        (t2, i2) <- zip args ([0 ..] :: [Int]),
        i1 < i2
    ]

isSubTerm
  :: (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder)
  => Foil.Scope n -> AST binder sig n -> AST binder sig n -> Bool
isSubTerm scope l r | alphaEquiv scope l r = True
isSubTerm _ _ Var{} = False
isSubTerm scope l (Node node) =
  biany (isSubTermScoped scope l) (isSubTerm scope l) node

isSubTermScoped
  :: (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder)
  => Foil.Scope n -> AST binder sig n -> ScopedAST binder sig n -> Bool
isSubTermScoped scope l (ScopedAST binder r) =
  case (Foil.assertDistinct binder, Foil.assertExt binder) of
    (Foil.Distinct, Foil.Ext) ->
      let scope' = Foil.extendScopePattern binder scope
      in isSubTerm scope' (Foil.sink l) r

globalRestriction :: Scope n -> [TypedSOAS binder Raw.MetavarId TermSig n typ] -> [TypedSOAS binder Raw.MetavarId TermSig n typ] -> Bool
globalRestriction scope sn tn = True -- placeholder
