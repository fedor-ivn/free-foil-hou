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
import Data.Bifoldable (Bifoldable (bifoldl), biany, bifoldl', bifoldr)
import Data.Bifunctor.Sum (Sum (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
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
import Data.Bifunctor (first)

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

data Constraint typ metavar binder sig where
  Constraint ::
    (Distinct n) =>
    NameBinderList VoidS n ->
    NameMap n typ ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ ->
    Constraint typ metavar binder sig

data UnifyNameBinders' typ (pattern_ :: S -> S -> Type) n l r where
  RenameBothBinders' ::
    NameBinders n lr ->
    (NameMap n typ -> NameMap lr typ) ->
    (NameBinder n l -> NameBinder n lr) ->
    (NameBinder n r -> NameBinder n lr) ->
    UnifyNameBinders' typ pattern_ n l r
  NotUnifiable' :: UnifyNameBinders' typ pattern_ n l r

class TypedUnifiablePattern typ pattern_ where
  typedUnifyPatterns ::
    (Distinct n) =>
    pattern_ n l ->
    pattern_ n r ->
    UnifyNameBinders' typ pattern_ n l r

instance TypedUnifiablePattern () (AnnBinder () FoilPattern) where
  typedUnifyPatterns
    (AnnBinder (FoilPatternVar b1) t1)
    (AnnBinder (FoilPatternVar b2) t2)
      | t1 == t2 =
          case Foil.unifyNameBinders b1 b2 of
            Foil.NotUnifiable -> NotUnifiable'
            Foil.RenameBothBinders ub rl rr ->
              RenameBothBinders' ub (addNameBinders ub [t1]) rl rr
            Foil.RenameLeftNameBinder ub rl ->
              RenameBothBinders' ub (addNameBinders ub [t1]) rl id
            Foil.RenameRightNameBinder ub rr ->
              RenameBothBinders' ub (addNameBinders ub [t1]) id rr
            Foil.SameNameBinders ub ->
              RenameBothBinders' ub (addNameBinders ub [t1]) id id
      | otherwise = NotUnifiable'

swapConstraint ::
  Constraint typ metavar binder sig ->
  Constraint typ metavar binder sig
swapConstraint (Constraint vs ty lhs rhs) = Constraint vs ty rhs lhs

class MetavarFreshable metavar where
  newMetavarId :: metavar -> metavar

instance MetavarFreshable Raw.MetavarId where
  newMetavarId (Raw.MetavarId name) = Raw.MetavarId (name ++ "'")

-- | Generalized term representation
type Sig typ metavar binder sig n =
  sig (TypedScopedSOAS binder metavar sig n typ) (TypedSOAS binder metavar sig n typ)

isFlexible :: TypedSOAS binder metavar sig n t -> Bool
isFlexible MetaApp {} = True
isFlexible _ = False

toNameMap ::
  NameMap m a ->
  NameBinderList m l ->
  [a] ->
  NameMap l a
toNameMap nameMap NameBinderListEmpty [] = nameMap
toNameMap nameMap (NameBinderListCons binder rest) (x : xs) =
  toNameMap (addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

collapseMetaSubsts' ::
  [MetaSubsts binder sig metavar t] ->
  MetaSubsts binder sig metavar t
collapseMetaSubsts' = MetaSubsts . concatMap getMetaSubsts

data Stream a = Stream a (Stream a) deriving (Eq, GHC.Generic)

-- | Type of a metavariable with its parameter types and return type
data MetaType typ = MetaType [typ] typ deriving (Eq, GHC.Generic)

-- >>> let meta = Raw.MetavarId "M"
-- >>> let typedTerm = MetaApp meta [] "DummyType" :: TypedSOAS FoilPattern Raw.MetavarId TermSig VoidS String
-- >>> termType typedTerm
-- "DummyType"

-- | Get the metavariable arguments from a term.
getMetavarArgs ::
  TypedSOAS binder metavar sig n typ ->
  [TypedSOAS binder metavar sig n typ]
getMetavarArgs term = case term of
  MetaApp _ args _ -> args
  _ -> error "Not a metavariable application term"

getMetavar :: TypedSOAS binder metavar sig n typ -> metavar
getMetavar term = case term of
  MetaApp metavar _ _ -> metavar
  _ -> error "Not a metavariable application term"

-- | Unify two terms (generalised FCU algorithm)
unify ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK (Sum sig (MetaAppSig metavar)),
    ZipMatchK typ,
    ZipMatchK metavar,
    Bifoldable sig,
    Bifunctor sig,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig,
    TypedUnifiablePattern typ binder
  ) =>
  ( MetaSubsts
      binder
      (AnnSig typ (Sum sig (MetaAppSig metavar)))
      metavar
      typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts
    binder
    (AnnSig typ (Sum sig (MetaAppSig metavar)))
    metavar
    typ
unify (th, Constraint forall_ forallTypes s t) =
  let scope = extendScopePattern forall_ emptyScope
      s' = applyMetaSubsts scope th s
      t' = applyMetaSubsts scope th t
      c' = Constraint forall_ forallTypes s' t'
   in cases scope (th, c')

-- | Select a rule (0)-(5) to proceed with
cases ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    Distinct n,
    UnifiablePattern binder,
    ZipMatchK (Sum sig (MetaAppSig metavar)),
    ZipMatchK typ,
    ZipMatchK metavar,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig,
    TypedUnifiablePattern typ binder
  ) =>
  Scope n ->
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
cases scope (th, c@(Constraint _ _ s t)) = case (isFlexible s, isFlexible t) of
  (True, False) -> caseFlexRigid (th, c) -- Rule (3)
  (False, True) -> caseFlexRigid (th, swapConstraint c) -- Rule (3)
  (True, True) -> caseFlexFlex (th, c) -- Rules (4-5)
  (False, False) -> caseRigidRigid (th, c) -- Rules (0-2)
  _ -> error "Unexpected case in cases"

decomposeRigidRigid ::
  forall typ metavar binder sig.
  ( CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    TypedUnifiablePattern typ binder
  ) =>
  Constraint typ metavar binder sig ->
  Maybe
    [Constraint typ metavar binder sig]
decomposeRigidRigid (Constraint forall_ forallTypes s t) = case (s, t) of
  (Var x, Var y)
    | x == y -> Just []
    | otherwise -> Nothing
  (MetaApp {}, _) -> Nothing
  (_, MetaApp {}) -> Nothing
  (Node sTerm, Node tTerm) ->
    case zipMatch2 sTerm tTerm of
      Nothing -> Nothing
      Just term ->
        getConst $
          bitraverse goScoped go term
    where
      go (s', t') = Const (Just [Constraint forall_ forallTypes s' t'])

      goScoped (ScopedAST sBinder s', ScopedAST tBinder t') =
        case typedUnifyPatterns sBinder tBinder of
          NotUnifiable' -> Const Nothing
          RenameBothBinders' unifiedBinder unifiedBinderTypes renameLeft renameRight
            | Distinct <- assertDistinct unifiedBinder ->
                let forall' =
                      concatNameBinderLists
                        (nameBindersList unifiedBinder)
                        forall_
                    scope' = extendScopePattern forall' emptyScope
                    s'' = liftRM scope' (Foil.fromNameBinderRenaming renameLeft) s'
                    t'' = liftRM scope' (Foil.fromNameBinderRenaming renameRight) t'
                 in Const (Just [Constraint forall' (unifiedBinderTypes forallTypes) s'' t''])
  _ -> Nothing

-- | Rules (1-2) implementation
caseRigidRigid ::
  ( CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    Eq metavar,
    ZipMatchK typ,
    ZipMatchK metavar,
    MetavarFreshable metavar,
    TypedUnifiablePattern typ binder
  ) =>
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseRigidRigid (th, constraint) =
  case decomposeRigidRigid constraint of
    Nothing -> error "not unifiable in rigid–rigid case"
    Just [] -> th
    Just newConstraints ->
      let step th' nc = unify (th', nc)
       in foldl step th newConstraints

discharge ::
  forall n l m typ metavar binder sig.
  ( Distinct n,
    Distinct l,
    CoSinkable binder,
    SinkableK binder,
    Bifunctor sig,
    Bitraversable sig,
    ZipMatchK sig,
    ZipMatchK typ,
    ZipMatchK metavar,
    UnifiablePattern binder, Distinct m
  ) =>
  Scope n ->
  [(TypedSOAS binder metavar sig n typ, Name l)] ->
  -- left are terms we are searching from, right are variables we are replacing them with
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig m typ
discharge scope pairs = go
  where
    replaceIfMatch u =
      case [v | (k, v) <- pairs, alphaEquiv scope u k] of
        (v : _) -> Just v
        [] -> Nothing

    go :: TypedSOAS binder metavar sig n typ -> TypedSOAS binder metavar sig m typ
    go tm = case replaceIfMatch tm of
      Just newVar -> Var newVar
      Nothing -> case tm of
        Var {} -> tm
        MetaApp m args t -> MetaApp m (map (discharge scope pairs) args) t
        Node term -> Node (bimap goScoped go term)

    goScoped (ScopedAST binder term) =
      case (Foil.assertDistinct binder, Foil.assertExt binder) of
        (Foil.Distinct, Foil.Ext) ->
          ScopedAST binder (discharge (extendScopePattern binder scope) pairs term)

-- | Rule (3) implementation, flex-rigid
caseFlexRigid ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig
  ) =>
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexRigid (th, Constraint forall_ _ s t)
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
            $ \scope' zn argsTypes -> case (Foil.assertDistinct zn, Foil.assertExt zn) of
              (Foil.Distinct, Foil.Ext) ->
                let pairs = zip tn (namesOfPattern zn)
                    body = discharge scope pairs s'
                    newSub = MetaSubst (metavar, MetaAbs zn argsTypes body)
                 in collapseMetaSubsts' [th, pruningResult, MetaSubsts [newSub]]
  where
    scope = extendScopePattern forall_ emptyScope
    tn = getMetavarArgs t
    metavar = getMetavar t
caseFlexRigid _ = error "unexpected case in flex-rigid"

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
  forall m n typ metavar binder sig.
  ( Distinct m,
    CoSinkable binder,
    SinkableK binder,
    Bitraversable sig,
    ZipMatchK sig,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    Distinct n
  ) =>
  Scope n ->
  NameBinderList VoidS m ->
  [TypedSOAS binder metavar sig n typ] ->
  [TypedSOAS binder metavar sig n typ] ->
  [Name m]
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
  [Name l] ->
  [TypedSOAS binder metavar sig n typ] ->
  [TypedSOAS binder metavar sig n typ] ->
  [Name l]
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
    MetavarFreshable metavar
  ) =>
  Scope n ->
  [TypedSOAS binder metavar sig n typ] ->
  (MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ, TypedSOAS binder metavar sig n typ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
prune scope tn (rho, u) = case applyMetaSubsts scope rho u of
  MetaApp meta sm typ ->
    if subset' scope sm tn
      then rho
      else withFreshNameBinderList
        (map termType tn)
        emptyScope
        NameBinderListEmpty
        emptyNameMap
        $ \_ vsm argsTypes ->
          let selectedArgs = eqsel' scope vsm tn sm
              newMeta = newMetavarId meta
              body = MetaApp newMeta (Var <$> selectedArgs) typ
              newSubst = MetaSubst (meta, MetaAbs vsm argsTypes body)
           in collapseMetaSubsts' [MetaSubsts [newSubst], rho]
  Var x ->
    if any (alphaEquiv scope (Var x)) tn
      then rho
      else error "var not in lhs context"
  Node (AnnSig sig _) ->
    bifoldl' stepScoped step rho sig
    where
      step rho' t = prune scope tn (rho', t)

      stepScoped rho' (ScopedAST binder body) =
        case (Foil.assertDistinct binder, Foil.assertExt binder) of
          (Foil.Distinct, Foil.Ext) ->
            let scope' = extendScopePattern binder scope
                x = Var (head (namesOfPattern binder))
                tn' = x : fmap Foil.sink tn
             in prune scope' tn' (rho', body)
  _ -> error "Prune: unexpected generalized case"

-- | Check the restrictions and select rule (4) or (5) to proceed with
caseFlexFlex ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig
  ) =>
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlex (th, c@(Constraint forall_ _ (MetaApp meta1 sn _) (MetaApp meta2 tn _)))
  | not (argumentRestriction (extendScopePattern forall_ emptyScope) sn) = error "Global restriction fail at flexflex case"
  | not (argumentRestriction (extendScopePattern forall_ emptyScope) tn) = error "Global restriction fail at flexflex case"
  | not (localRestriction (extendScopePattern forall_ emptyScope) sn) = error "Local restriction fail at flexflex case"
  | not (localRestriction (extendScopePattern forall_ emptyScope) tn) = error "Local restriction fail at flexflex case"
  | meta1 == meta2 = caseFlexFlexSame (th, c)
  | otherwise = caseFlexFlexDiff (th, c)
caseFlexFlex _ = error "Unexpected case at flexflex case"

-- | Rule (4) implementation, flex-flex with same metavariable on both sides
caseFlexFlexSame ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK typ,
    MetavarFreshable metavar,
    ZipMatchK metavar,
    Bitraversable sig,
    ZipMatchK sig
  ) =>
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlexSame (th, c@(Constraint forall_ _ (MetaApp meta1 sn typ1) (MetaApp _ tn _)))
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | and (zipWith (alphaEquiv (extendScopePattern forall_ emptyScope)) sn tn) = error "Same argument lists in (4) rule"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \_ vsm argsTypes ->
        let scope = extendScopePattern forall_ emptyScope
            selectedArgs = eqsel' scope vsm tn sn
            newMeta = newMetavarId meta1
            body = MetaApp newMeta (Var <$> selectedArgs) typ1
            newSubs = MetaSubst (meta1, MetaAbs vsm argsTypes body)
         in collapseMetaSubsts' [th, MetaSubsts [newSubs]]
caseFlexFlexSame _ = error "Unexpected case at FlexFlexSame"

-- | Rule (5) implementation, flex-flex with different metavariables
caseFlexFlexDiff ::
  ( ZipMatchK typ,
    ZipMatchK metavar,
    UnifiablePattern binder,
    Eq metavar,
    Bitraversable sig,
    ZipMatchK sig,
    SinkableK binder,
    MetavarFreshable metavar
  ) =>
  ( MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ,
    Constraint typ metavar binder sig
  ) ->
  MetaSubsts binder (AnnSig typ (Sum sig (MetaAppSig metavar))) metavar typ
caseFlexFlexDiff (th, Constraint forall_ _ (MetaApp meta1 sn typ1) (MetaApp meta2 tn typ2))
  | not (globalRestriction (extendScopePattern forall_ emptyScope) sn tn) = error "Global restriction fail at flexflex case"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \_ vsm argsTypes ->
        let pruningResultLeft = prune (extendScopePattern forall_ emptyScope) sn (th, MetaApp meta2 tn typ2)
            pruningResultRight = prune (extendScopePattern forall_ emptyScope) tn (th, MetaApp meta1 sn typ1)
            tmnew = getMetavarArgs (applyMetaSubsts (extendScopePattern forall_ emptyScope) pruningResultLeft (MetaApp meta2 tn typ2))
            snnew = getMetavarArgs (applyMetaSubsts (extendScopePattern forall_ emptyScope) pruningResultRight (MetaApp meta1 sn typ1))

            scope = extendScopePattern forall_ emptyScope
            permutatedArgs = permutate' scope (namesOfPattern vsm) tmnew snnew

            body = MetaApp meta1 (Var <$> permutatedArgs) typ1
            newSubs = MetaSubst (meta2, MetaAbs vsm argsTypes body)
         in collapseMetaSubsts' [th, pruningResultLeft, pruningResultRight, MetaSubsts [newSubs]]
caseFlexFlexDiff _ = error "Unexpected case at FlexFlexDiff"

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
