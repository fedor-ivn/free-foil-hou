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
{-# LANGUAGE TypeOperators #-}
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
import Data.Bifoldable (Bifoldable, biany, bifoldr)
import Data.Bifunctor.Sum (Sum (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.SOAS hiding (SOAS, withFreshNameBinderList)
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
instance ZipMatchK Raw.Id where zipMatchWithK = zipMatchViaEq

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
  fromString :: String -> AST FoilPattern TermSig VoidS
  fromString input = case Raw.pTerm (Raw.tokens input) of
    Left err -> error ("could not parse λΠ-term: " <> input <> "\n  " <> err)
    Right term -> toTermClosed term

-- | Pretty-print scope-safe terms via raw representation.
instance Show (AST FoilPattern TermSig Foil.VoidS) where
  show = Raw.printTree . fromTerm

-- * Unification test

withFreshNameBinderList ::
  (Foil.Distinct n) =>
  [a] ->
  Foil.Scope n ->
  Foil.NameBinderList i n ->
  Foil.NameMap n a ->
  ( forall l.
    (Foil.Distinct l) =>
    Foil.Scope l ->
    Foil.NameBinderList i l ->
    Foil.NameMap l a ->
    r
  ) ->
  r
withFreshNameBinderList [] scope binders nameMap cont = cont scope binders nameMap
withFreshNameBinderList (typ : types) scope binders nameMap cont =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        binders' = push binder binders
        nameMap' = Foil.addNameBinder binder typ nameMap
     in withFreshNameBinderList types scope' binders' nameMap' cont

typeOf :: NameMap n typ -> AST binder (AnnSig typ sig) n -> typ
typeOf variables (Var name) = lookupName name variables
typeOf _ (Node (AnnSig _ typ)) = typ

termType :: TypedSOAS binder metavar sig n typ -> typ
termType (Var _) = undefined
termType (MetaApp _ _ t) = t
termType (Node (AnnSig _ t)) = t

class MetavarFreshable metavar where
  newMetavarId :: metavar -> metavar

instance MetavarFreshable Raw.MetavarId where
  newMetavarId (Raw.MetavarId name) = Raw.MetavarId (name ++ "'")

class
  ( Eq typ,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    MetavarFreshable metavar
  ) =>
  FCUUnifiable typ metavar binder sig
  where
  -- | Method to check if term is an application term and return its head and argument
  matchApplication ::
    TypedSOAS binder metavar sig n typ ->
    Maybe (TypedSOAS binder metavar sig n typ, TypedSOAS binder metavar sig n typ)

  -- | Method to check if term is an abstraction term and return itself
  matchAbstraction ::
    TypedSOAS binder metavar sig n typ -> Maybe (TypedScopedSOAS binder metavar sig n typ)

instance (Eq typ, SinkableK FoilPattern) => FCUUnifiable typ Raw.MetavarId FoilPattern TermSig where
  matchApplication term = case term of
    Node (AnnSig (L2 (AppTermSig function argument)) _) -> Just (function, argument)
    _ -> Nothing

  matchAbstraction term = case term of
    Node (AnnSig (L2 (AbsTermSig sc)) _) -> Just sc
    _ -> Nothing

-- | Generalized term representation
type Sig typ metavar binder sig n =
  sig (TypedScopedSOAS binder metavar sig n typ) (TypedSOAS binder metavar sig n typ)

-- | Substitutions representations
data Substitution typ metavar binder sig where
  Substitution ::
    metavar ->
    NameBinderList VoidS n ->
    TypedSOAS binder metavar sig n typ ->
    Substitution typ metavar binder sig

newtype Substitutions typ metavar binder sig = Substitutions [Substitution typ metavar binder sig]

isFlexible :: TypedSOAS binder metavar sig n t -> Bool
isFlexible MetaApp {} = True
isFlexible _ = False

-- -- | Replace metavariables in terms with its substitutions
-- devar' ::
--   forall sig metavar metavar' n binder t.
--   ( Bifunctor sig,
--     Eq metavar,
--     Foil.Distinct n,
--     Foil.CoSinkable binder,
--     Foil.SinkableK binder
--   ) =>
--   Foil.Scope n ->
--   MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t ->
--   TypedSOAS binder metavar sig n t ->
--   TypedSOAS binder metavar' sig n t
-- devar' = applyMetaSubsts

toNameMap ::
  NameMap m a ->
  NameBinderList m l ->
  [a] ->
  NameMap l a
toNameMap nameMap NameBinderListEmpty [] = nameMap
toNameMap nameMap (NameBinderListCons binder rest) (x : xs) =
  toNameMap (addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

applySubstitutions' ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n) =>
  Substitutions typ metavar binder sig ->
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
applySubstitutions' (Substitutions []) _ term = term
applySubstitutions' (Substitutions (subst : substs)) scope term =
  applySubstitutions' (Substitutions substs) scope (applySubstitution' subst scope term)

collapseMetaSubsts' ::
     [MetaSubsts binder sig metavar t]
  -> MetaSubsts binder sig metavar t
collapseMetaSubsts' = MetaSubsts . concatMap getMetaSubsts

data Stream a = Stream a (Stream a) deriving (Eq, GHC.Generic)

-- | Type of a metavariable with its parameter types and return type
data MetaType typ = MetaType [typ] typ deriving (Eq, GHC.Generic)

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

-- >>> let meta = Raw.MetavarId "M"
-- >>> let typedTerm = MetaApp meta [] "DummyType" :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS String
-- >>> termType typedTerm
-- "DummyType"

-- | Decomposes a term into head and arguments
strip' ::
  (FCUUnifiable typ metavar binder sig) =>
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  (TypedSOAS binder metavar sig n typ, [TypedSOAS binder metavar sig n typ])
strip' scope term = case term of
  MetaApp _ args _ -> (term, args)
  _ -> case matchApplication term of
    Just (f, x) ->
      let (headF, argsF) = strip' scope f
       in (headF, argsF ++ [x])
    Nothing -> (term, [])

-- | Unify two terms using FCU algorithm
unify ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK (Sum sig (MetaAppSig metavar)),
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS,
    metavar ~ metavar'
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    ( TypedSOAS binder metavar sig n typ,
      TypedSOAS binder metavar sig n typ
    )
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
unify scope (th, (s, t)) = case (applyMetaSubsts scope th s, applyMetaSubsts scope th t) of
  (Node sNode, Node tNode) ->
    case (findScopedAST sNode, findScopedAST tNode) of
      (Just scopedAST1, Just scopedAST2) ->
        unifyScoped scope th scopedAST1 scopedAST2
      (Nothing, Nothing) -> cases scope (th, (Node sNode, Node tNode))
      (Just _, Nothing) -> error "Not unifiable"
      (Nothing, Just _) -> error "Not unifiable"
  (s', t') -> cases scope (th, (s', t'))

findScopedAST ::
  (Bifoldable sig) =>
  AnnSig typ (Sum sig q) (TypedScopedSOAS binder metavar sig n typ) (TypedSOAS binder metavar sig n typ) ->
  Maybe (TypedScopedSOAS binder metavar sig n typ)
findScopedAST (AnnSig (L2 node) _) =
  bifoldr collectScoped (const id) Nothing node
  where
    collectScoped ::
      TypedScopedSOAS binder metavar sig n typ ->
      Maybe (TypedScopedSOAS binder metavar sig n typ) ->
      Maybe (TypedScopedSOAS binder metavar sig n typ)
    collectScoped scoped@(ScopedAST _ _) _ = Just scoped
    collectScoped _ acc = acc
findScopedAST _ = Nothing

-- | Rule (1) implementation, abstraction case
unifyScoped ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK typ,
    Bifunctor sig,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ ->
  TypedScopedSOAS binder metavar sig n typ ->
  TypedScopedSOAS binder metavar sig n typ ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
unifyScoped scope th (ScopedAST binder1 body1) (ScopedAST binder2 body2) =
  case Foil.unifyPatterns binder1 binder2 of
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

-- | Select a rule (2)-(5) to proceed with (rule (1) is covered earlier)
cases ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK (Sum sig (MetaAppSig metavar)),
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  (MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ, (TypedSOAS binder metavar sig n typ, TypedSOAS binder metavar sig n typ)) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
cases scope (th, (s, t)) = case (isFlexible s, isFlexible t) of
  (True, False) -> caseFlexRigid scope (th, (s, t))
  (False, True) -> caseFlexRigid scope (th, (t, s))
  (True, True) -> caseFlexFlex scope (th, (s, t))
  (False, False) -> caseRigidRigid scope (th, (s, t))
  _ -> error "Unexpected case in cases"

-- | Rule (2) implementation, rigid-rigid
caseRigidRigid ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    ( TypedSOAS binder metavar sig n typ,
      TypedSOAS binder metavar sig n typ
    )
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseRigidRigid scope (th, (a, b)) = case (strip' scope a, strip' scope b) of
  ((Var x, sn), (Var y, tm)) -> applicableCase (x, sn, y, tm)
  _ -> error "Not unifiable in (2) rule"
  where
    applicableCase (x, sn, y, tm) =
      if x == y && length sn == length tm
        then foldl (\th' (s, t) -> unify scope (th', (s, t))) th (zip sn tm)
        else error "Not unifiable in (2) rule"

-- | Rule (3) implementation, flex-rigid
caseFlexRigid ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    ( TypedSOAS binder metavar sig n typ,
      TypedSOAS binder metavar sig n typ
    )
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexRigid scope (th, (s, t))
  | not (argumentRestriction scope tn) = error "argument restriction failed"
  | not (localRestriction scope tn) = error "local restriction failed"
  | otherwise =
      let pruningResult = prune scope tn (th, s)
          s' = applyMetaSubsts scope pruningResult s
       in withFreshNameBinderList
            (map termType tn)
            emptyScope
            NameBinderListEmpty
            emptyNameMap
            $ \_ zn argsTypes ->
              case (Foil.assertDistinct zn, Foil.assertExt zn) of
                (Foil.Distinct, Foil.Ext) ->
                  let tn' = fmap Foil.sink tn
                      s'' = Foil.sink s'
                      body = discharge tn' zn s''
                      newSubst = MetaSubst (metavar, MetaAbs zn argsTypes body)
                   in collapseMetaSubsts' [th, pruningResult, MetaSubsts [newSubst]]
  where
    tn = case strip' scope t of
      (_, args) -> args
      _ -> error "Unexpected flexible term structure in flex-rigid case"
    metavar = case t of
      MetaApp meta _ _ -> meta
      _ -> error "Unexpected flexible term structure in flex-rigid case"
caseFlexRigid _ _ = error "unexpected case in flex-rigid"

-- | Discharge a term by replacing all matching occurences with zn
discharge ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    Bifunctor sig
  ) =>
  [TypedSOAS binder metavar sig n typ] ->
  NameBinderList VoidS n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
discharge args freshVars body =
  let scope' = extendScopePattern freshVars emptyScope
      nameMap = toNameMap emptyNameMap freshVars args
      substs = nameMapToSubstitution nameMap
   in substitute scope' substs body

-- | Helper to fold over a list of terms with an accumulator
foldlN' ::
  Scope n ->
  [TypedSOAS binder metavar sig n typ] ->
  ( ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
      TypedSOAS binder metavar sig n typ
    ) ->
    MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
  ) ->
  (MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ, [TypedSOAS binder metavar sig n typ]) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
foldlN' _ _ _ (rho, []) = rho
foldlN' scope tn f (rho, t : ts) = foldlN' scope tn f (f (rho, t), ts)

-- | Helper function to check if one list of terms is a subset of another
subset' ::
  ( Distinct n,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar
  ) =>
  Scope n ->
  [TypedSOAS binder metavar sig n typ] ->
  [TypedSOAS binder metavar sig n typ] ->
  Bool
subset' scope xs ys = all (\x -> any (alphaEquiv scope x) ys) xs

-- | Helper function to get a list of names
eqsel' ::
  ( Distinct n,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar
  ) =>
  Scope n ->
  NameBinderList VoidS n ->
  [TypedSOAS binder metavar sig n typ] ->
  [TypedSOAS binder metavar sig n typ] ->
  [Name n]
eqsel' scope vsm tn sm =
  [v | (v, s) <- zip (namesOfPattern vsm) sm, any (alphaEquiv scope s) tn]

-- | Helper function for permutation in (5) rule
permutate' ::
  ( Distinct n,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar
  ) =>
  Scope n ->
  [Name n] ->
  [TypedSOAS binder metavar sig n typ] ->
  [TypedSOAS binder metavar sig n typ] ->
  [Name n]
permutate' scope zs as bs =
  [ zs !! i
    | b <- bs,
      i <- maybeToList (elemIndexBy (alphaEquiv scope) b as)
  ]

-- | Helper function to get an index of the element
elemIndexBy ::
  (a -> a -> Bool) ->
  a ->
  [a] ->
  Maybe Int
elemIndexBy _ _ [] = Nothing
elemIndexBy eq x (y : ys)
  | eq x y = Just 0
  | otherwise = (1 +) <$> elemIndexBy eq x ys

-- | Pruning implementation, applied before rules (3) and (5)
prune ::
  ( Distinct n,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    Bitraversable sig,
    ZipMatchK sig,
    Eq metavar,
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  [TypedSOAS binder metavar sig n typ] ->
  (MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ, TypedSOAS binder metavar sig n typ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
prune scope tn (rho, u) = case strip' scope (applyMetaSubsts scope rho u) of
  (_, []) -> rho
  (term, _) | Just (ScopedAST binder body) <- matchAbstraction term ->
    case (assertDistinct binder, assertExt binder) of
      (Distinct, Ext) ->
        let scope' = extendScopePattern binder scope
            [x] = namesOfPattern binder
            newVar = Var x
            tn' = newVar : fmap sink tn
         in prune scope' tn' (rho, body)
  (Var x, rr) ->
    if any (alphaEquiv scope (Var x)) tn
      then foldlN' scope tn (prune scope tn) (rho, rr)
      else error "var not in lhs context"
  (MetaApp meta sm typ, _) ->
    if subset' scope sm tn
      then rho
      else withFreshNameBinderList
        (map termType tn)
        emptyScope
        NameBinderListEmpty
        emptyNameMap
        $ \scope' vsm argsTypes -> case (Foil.assertDistinct vsm, Foil.assertExt vsm) of
          (Foil.Distinct, Foil.Ext) ->
            let tn' = fmap sink tn
                sm' = fmap sink sm
                selectedArgs = eqsel' scope' vsm tn' sm'
                newMeta = newMetavarId meta
                body = MetaApp newMeta (Var <$> selectedArgs) typ 
                newSubst = MetaSubst (meta, MetaAbs vsm argsTypes body)
             in collapseMetaSubsts' [MetaSubsts [newSubst], rho]
  (Node _, rr) ->
    foldlN' scope tn (prune scope tn) (rho, rr)
  _ -> error "Prune: unexpected generalized case"

-- | Check the restrictions and select rule (4) or (5) to proceed with
caseFlexFlex ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    ExtEndo VoidS
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    (TypedSOAS binder metavar sig n typ, TypedSOAS binder metavar sig n typ)
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlex scope (th, (MetaApp meta1 sn typ1, MetaApp meta2 tn typ2))
  | not (argumentRestriction scope sn) = error "Global restriction fail at flexflex case"
  | not (argumentRestriction scope tn) = error "Global restriction fail at flexflex case"
  | not (localRestriction scope sn) = error "Local restriction fail at flexflex case"
  | not (localRestriction scope tn) = error "Local restriction fail at flexflex case"
  | meta1 == meta2 = caseFlexFlexSame scope (th, (MetaApp meta1 sn typ1, MetaApp meta2 tn typ2))
  | otherwise = caseFlexFlexDiff scope (th, (MetaApp meta1 sn typ1, MetaApp meta2 tn typ2))
caseFlexFlex _ _ = error "Unexpected case at flexflex case"

-- | Rule (4) implementation, flex-flex with same metavariable on both sides
caseFlexFlexSame ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK typ,
    MetavarFreshable metavar,
    FCUUnifiable typ metavar binder sig,
    ZipMatchK metavar,
    ExtEndo VoidS
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    (TypedSOAS binder metavar sig n typ, TypedSOAS binder metavar sig n typ)
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlexSame scope (th, (MetaApp meta1 sn typ1, MetaApp _ tn _))
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | and (zipWith (alphaEquiv scope) sn tn) = error "Same argument lists in (4) rule"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \scope' vsm argsTypes -> case (Foil.assertDistinct vsm, Foil.assertExt vsm) of
        (Foil.Distinct, Foil.Ext) ->
          let tn' = fmap sink tn
              sn' = fmap sink sn
              selectedArgs = eqsel' scope' vsm tn' sn'
              newMeta = newMetavarId meta1
              body = MetaApp newMeta (Var <$> selectedArgs) typ1
              newSubs = MetaSubst (meta1, MetaAbs vsm argsTypes body)
           in collapseMetaSubsts' [th, MetaSubsts [newSubs]]
caseFlexFlexSame _ _ = error "Unexpected case at FlexFlexSame"


-- | Rule (5) implementation, flex-flex with different metavariables
caseFlexFlexDiff ::
  ( ZipMatchK typ,
    ZipMatchK metavar,
    FCUUnifiable typ metavar binder sig,
    Distinct n,
    UnifiablePattern binder,
    Eq metavar,
    ExtEndo VoidS
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    (TypedSOAS binder metavar sig n typ, TypedSOAS binder metavar sig n typ)
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlexDiff scope (th, (MetaApp meta1 sn typ1, MetaApp meta2 tn typ2))
  | not (globalRestriction scope sn tn) = error "Global restriction fail at flexflex case"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \scope' vsm argsTypes -> case (Foil.assertDistinct vsm, Foil.assertExt vsm) of
        (Foil.Distinct, Foil.Ext) ->
          let pruningResultLeft = prune scope sn (th, MetaApp meta2 tn typ2)
              pruningResultRight = prune scope tn (th, MetaApp meta1 sn typ1)
              tmnew = snd (strip' scope (applyMetaSubsts scope pruningResultLeft (MetaApp meta2 tn typ2)))
              snnew = snd (strip' scope (applyMetaSubsts scope pruningResultRight (MetaApp meta1 sn typ1)))

              tmnew' = fmap sink tmnew
              snnew' = fmap sink snnew

              permutatedArgs = permutate' scope' (namesOfPattern vsm) tmnew' snnew'
              body = MetaApp meta1 (Var <$> permutatedArgs) typ1
              newSubs = MetaSubst (meta2, MetaAbs vsm argsTypes body)

           in collapseMetaSubsts' [th, pruningResultLeft, pruningResultRight, MetaSubsts [newSubs]]
caseFlexFlexDiff _ _ = error "Unexpected case at FlexFlexDiff"

-- | RESTRICTIONS (Done)

-- | Check if the arguments of a term are all restricted terms
argumentRestriction ::
  ( Bifoldable sig,
    Bitraversable sig,
    ZipMatchK sig,
    Distinct n,
    UnifiablePattern binder,
    SinkableK binder
  ) =>
  Scope n ->
  [AST binder sig n] ->
  Bool
argumentRestriction scope tn = and [isRTerm scope t | t <- tn]

isRTerm ::
  ( Bifoldable sig,
    Bitraversable sig,
    ZipMatchK sig,
    Distinct n,
    UnifiablePattern binder,
    SinkableK binder
  ) =>
  Scope n ->
  AST binder sig n ->
  Bool
isRTerm _ (Var {}) = True
isRTerm scope (Node node) = biany (isRTermScoped scope) (isRTerm scope) node
isRTerm _ _ = False

isRTermScoped ::
  ( Bifoldable sig,
    Bitraversable sig,
    ZipMatchK sig,
    Distinct n,
    UnifiablePattern binder,
    SinkableK binder
  ) =>
  Scope n ->
  ScopedAST binder sig n ->
  Bool
isRTermScoped scope (ScopedAST binder t) =
  case (Foil.assertDistinct binder, Foil.assertExt binder) of
    (Foil.Distinct, Foil.Ext) ->
      let scope' = Foil.extendScopePattern binder scope
       in isRTerm scope' t

-- | Check local restriction:
-- for all occurrences X_tn in S where X ∈ Q and for each t_i and t_j such that 0 < i,j ≤n and i != j, t_i ̸⊑ t_j.
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

isSubTerm ::
  (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder) =>
  Foil.Scope n ->
  AST binder sig n ->
  AST binder sig n ->
  Bool
isSubTerm scope l r | alphaEquiv scope l r = True
isSubTerm _ _ Var {} = False
isSubTerm scope l (Node node) =
  biany (isSubTermScoped scope l) (isSubTerm scope l) node

isSubTermScoped ::
  (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder) =>
  Foil.Scope n ->
  AST binder sig n ->
  ScopedAST binder sig n ->
  Bool
isSubTermScoped scope l (ScopedAST binder r) =
  case (Foil.assertDistinct binder, Foil.assertExt binder) of
    (Foil.Distinct, Foil.Ext) ->
      let scope' = Foil.extendScopePattern binder scope
       in isSubTerm scope' (Foil.sink l) r

-- | Check global restriction:
-- for all occurrences X_tn and Y_sm in S where X ∈ Q and for each t_i and s_j such that 0 < i ≤ n, 0 < i ≤ m, t_i ⊄ s_j.
globalRestriction ::
  (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder) =>
  Scope n ->
  [AST binder sig n] ->
  [AST binder sig n] ->
  Bool
globalRestriction scope tn sn =
  and
    [ not (isStrictSubTerm scope t1 t2) && not (isStrictSubTerm scope t2 t1)
      | t1 <- tn,
        t2 <- sn
    ]

isStrictSubTerm ::
  (Bitraversable sig, ZipMatchK sig, Distinct n, UnifiablePattern binder, SinkableK binder) =>
  Foil.Scope n ->
  AST binder sig n ->
  AST binder sig n ->
  Bool
isStrictSubTerm scope l r = isSubTerm scope l r && not (alphaEquiv scope l r)
