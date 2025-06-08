{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Data.Functor.Sum ()
import Data.Kind (Type)
import Data.List (intercalate)
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
import Debug.Trace (trace)

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

instance ZipMatchK () where
  zipMatchWithK = zipMatchViaEq

data AnnBinder ann binder (n :: Foil.S) (l :: Foil.S)
  = AnnBinder (binder n l) ann
  deriving (GHC.Generic)

deriveGenericK ''AnnBinder

instance (Foil.CoSinkable binder) => Foil.CoSinkable (AnnBinder ann binder) where
  coSinkabilityProof rename (AnnBinder binder ann) cont =
    Foil.coSinkabilityProof rename binder (\rename' binder' -> cont rename' (AnnBinder binder' ann))

  withPattern f empty append scope (AnnBinder binder t) cont =
    Foil.withPattern f empty append scope binder (\f' binder' -> cont f' (AnnBinder binder' t))

deriveGenericK ''FoilPattern

instance Foil.SinkableK FoilPattern

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
instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- Helpers

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

data Constraint typ metavar binder sig where
  Constraint ::
    (Distinct n) =>
    NameBinderList VoidS n ->
    NameMap n typ ->
    TypedSOAS binder metavar sig n typ ->
    TypedSOAS binder metavar sig n typ ->
    Constraint typ metavar binder sig

data Substitution typ metavar binder sig where
  Substitution ::
    metavar ->
    NameBinderList VoidS n ->
    TypedSOAS binder metavar sig n typ ->
    Substitution typ metavar binder sig

newtype Substitutions typ metavar binder sig = Substitutions [Substitution typ metavar binder sig]

applySubstitutionInTerm ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n) =>
  Substitution typ metavar binder sig ->
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
applySubstitutionInTerm substitution scope node = case node of
  Var {} -> node
  MetaApp meta arguments _
    | Substitution expectedMeta parameters body <- substitution,
      meta == expectedMeta ->
        let nameMap = toNameMap emptyNameMap parameters arguments
            substs' = nameMapToSubstitution nameMap
         in substitute scope substs' body
  MetaApp meta parameters typ ->
    MetaApp meta (applySubstitutionInTerm substitution scope <$> parameters) typ
  Node term -> Node (bimap goScoped go term)
    where
      go = applySubstitutionInTerm substitution scope
      goScoped (ScopedAST binder term)
        | Distinct <- assertDistinct binder =
            ScopedAST binder (applySubstitutionInTerm substitution scope' term)
        where
          scope' = extendScopePattern binder scope

-- Helper: apply all substitutions in a Substitutions to a term
applySubstitutionsInTerm ::
  (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n) =>
  Substitutions typ metavar binder sig ->
  Scope n ->
  TypedSOAS binder metavar sig n typ ->
  TypedSOAS binder metavar sig n typ
applySubstitutionsInTerm (Substitutions subs) scope term =
  foldl (\acc sub -> applySubstitutionInTerm sub scope acc) term subs

termType :: TypedSOAS binder metavar sig n typ -> typ
termType (Var _) = undefined
termType (MetaApp _ _ t) = t
termType (Node (AnnSig _ t)) = t

collapseSubstitutions ::
  [Substitutions typ metavar binder sig] ->
  Substitutions typ metavar binder sig
collapseSubstitutions = Substitutions . concatMap (\(Substitutions xs) -> xs)

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

instance TypedUnifiablePattern () FoilPattern where
  typedUnifyPatterns
    (FoilPatternVar b1)
    (FoilPatternVar b2) =
      case Foil.unifyNameBinders b1 b2 of
        Foil.NotUnifiable -> NotUnifiable'
        Foil.RenameBothBinders ub rl rr ->
          RenameBothBinders' ub (addNameBinders ub [()]) rl rr
        Foil.RenameLeftNameBinder ub rl ->
          RenameBothBinders' ub (addNameBinders ub [()]) rl id
        Foil.RenameRightNameBinder ub rr ->
          RenameBothBinders' ub (addNameBinders ub [()]) id rr
        Foil.SameNameBinders ub ->
          RenameBothBinders' ub (addNameBinders ub [()]) id id

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

data Stream a = Stream a (Stream a) deriving (Eq, GHC.Generic)

-- | Type of a metavariable with its parameter types and return type
data MetaType typ = MetaType [typ] typ deriving (Eq, GHC.Generic)

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

-- * Unification

-- | Unify two terms (generalised FCU algorithm)
unify ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    Bifoldable sig,
    Bifunctor sig,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig,
    TypedUnifiablePattern typ binder
  ) =>
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
unify (th, Constraint forall_ forallTypes s t) =
  let scope = extendScopePattern forall_ emptyScope
      s' = applySubstitutionsInTerm th scope s
      t' = applySubstitutionsInTerm th scope t
      c' = Constraint forall_ forallTypes s' t'
   in cases (th, c')

-- | Select a rule (0)-(5) to proceed with
cases ::
  ( Eq metavar,
    CoSinkable binder,
    SinkableK binder,
    UnifiablePattern binder,
    ZipMatchK typ,
    ZipMatchK metavar,
    MetavarFreshable metavar,
    Bitraversable sig,
    ZipMatchK sig,
    TypedUnifiablePattern typ binder
  ) =>
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
cases (th, c@(Constraint _ _ s t)) = case (isFlexible s, isFlexible t) of
  (False, True) -> caseFlexRigid (th, c) -- Rule (3)
  (True, False) -> caseFlexRigid (th, swapConstraint c) -- Rule (3)
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
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
caseRigidRigid (th, constraint) =
  case decomposeRigidRigid constraint of
    Nothing -> error "not unifiable in rigid–rigid case"
    Just [] -> th
    Just newConstraints ->
      let step th' nc = unify (th', nc)
       in foldl step th newConstraints

discharge ::
  forall n l typ metavar binder sig.
  ( Distinct n,
    Distinct l,
    CoSinkable binder,
    SinkableK binder,
    Bifunctor sig,
    Bitraversable sig,
    ZipMatchK sig,
    ZipMatchK typ,
    ZipMatchK metavar,
    UnifiablePattern binder
  ) =>
  Scope n ->
  Scope l ->
  [(TypedSOAS binder metavar sig n typ, Name l)] ->
  TypedSOAS binder metavar sig n typ ->
  Maybe (TypedSOAS binder metavar sig l typ)
discharge rhsScope substScope pairs term = case replaceIfMatch pairs term of
  Just v -> Just (Var v)
  Nothing -> case term of
    Var {} -> Nothing
    MetaApp m args t -> case mapM (discharge rhsScope substScope pairs) args of
      Just args' -> Just (MetaApp m args' t)
      Nothing -> Nothing
    Node term' -> Node <$> bimapM goScoped go term'
  where
    replaceIfMatch tzn u =
      case [v | (k, v) <- tzn, alphaEquiv rhsScope u k] of
        (v : _) -> Just v
        [] -> Nothing

    go = discharge rhsScope substScope pairs

    goScoped (ScopedAST binder body) =
      Foil.withRefreshedPattern @_ @_ @(AST binder sig) substScope binder $
        \_ binder' ->
          case (Foil.assertDistinct binder, Foil.assertExt binder) of
            (Foil.Distinct, Foil.Ext) ->
              let rhsScope' = Foil.extendScopePattern binder rhsScope
                  substScope' = Foil.extendScopePattern binder' substScope
                  binderMap = zip (Var <$> Foil.namesOfPattern binder) (Foil.namesOfPattern binder')
                  pairs' = binderMap <> (bimap Foil.sink Foil.sink <$> pairs)
               in ScopedAST binder' <$> discharge rhsScope' substScope' pairs' body

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
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
caseFlexRigid (th, Constraint forall_ _ s t)
  | not (argumentRestriction scope tn) = error "argument restriction failed"
  | not (localRestriction scope tn) = error "local restriction failed"
  | otherwise =
      let pruningResult = prune scope tn (th, s)
          s' = applySubstitutionsInTerm pruningResult scope s
       in withFreshNameBinderList
            (map termType tn)
            emptyScope
            NameBinderListEmpty
            emptyNameMap
            $ \scope' zn _ -> case (Foil.assertDistinct zn, Foil.assertExt zn) of
              (Foil.Distinct, Foil.Ext) ->
                let pairs = zip tn (namesOfPattern zn)
                    dischargeResult = discharge scope scope' pairs s'
                    newSub = case dischargeResult of
                      Just body -> Substitution metavar zn body
                      Nothing -> error "Error at discharge"
                 in collapseSubstitutions [th, pruningResult, Substitutions [newSub]]
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
  (Substitutions typ metavar binder sig, TypedSOAS binder metavar sig n typ) ->
  Substitutions typ metavar binder sig
prune scope tn (rho, u) = case applySubstitutionsInTerm rho scope u of
  MetaApp meta sm typ ->
    if subset' scope sm tn
      then rho
      else withFreshNameBinderList
        (map termType tn)
        emptyScope
        NameBinderListEmpty
        emptyNameMap
        $ \_ vsm _ ->
          let selectedArgs = eqsel' scope vsm tn sm
              newMeta = newMetavarId meta
              body = MetaApp newMeta (Var <$> selectedArgs) typ
              newSubst = Substitution meta vsm body
           in collapseSubstitutions [Substitutions [newSubst], rho]
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
                x = Var <$> namesOfPattern binder
                tn' = x <> fmap Foil.sink tn
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
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
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
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
caseFlexFlexSame (th, Constraint forall_ _ (MetaApp meta1 sn typ1) (MetaApp _ tn _))
  | length sn /= length tn = error "Different argument lists lengths in (4) rule"
  | and (zipWith (alphaEquiv (extendScopePattern forall_ emptyScope)) sn tn) = error "Same argument lists in (4) rule"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \_ vsm _ ->
        let scope = extendScopePattern forall_ emptyScope
            selectedArgs = eqsel' scope vsm tn sn
            newMeta = newMetavarId meta1
            body = MetaApp newMeta (Var <$> selectedArgs) typ1
            newSubs = Substitution meta1 vsm body
         in collapseSubstitutions [th, Substitutions [newSubs]]
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
  ( Substitutions typ metavar binder sig,
    Constraint typ metavar binder sig
  ) ->
  Substitutions typ metavar binder sig
caseFlexFlexDiff (th, Constraint forall_ _ (MetaApp meta1 sn typ1) (MetaApp meta2 tn typ2))
  | not (globalRestriction (extendScopePattern forall_ emptyScope) sn tn) = error "Global restriction fail at flexflex case"
  | otherwise = withFreshNameBinderList
      (map termType tn)
      emptyScope
      NameBinderListEmpty
      emptyNameMap
      $ \_ vsm _ ->
        let scope = extendScopePattern forall_ emptyScope
            pruningResultLeft = prune scope sn (th, MetaApp meta2 tn typ2)
            pruningResultRight = prune scope tn (th, MetaApp meta1 sn typ1)
            tmnew = getMetavarArgs (applySubstitutionsInTerm pruningResultLeft scope (MetaApp meta2 tn typ2))
            snnew = getMetavarArgs (applySubstitutionsInTerm pruningResultRight scope (MetaApp meta1 sn typ1))

            permutatedArgs = permutate' scope (namesOfPattern vsm) tmnew snnew

            body = MetaApp meta1 (Var <$> permutatedArgs) typ1
            newSubs = Substitution meta2 vsm body
         in collapseSubstitutions [th, pruningResultLeft, pruningResultRight, Substitutions [newSubs]]
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

-- * Testing

-- ** Pretty printing

convertFromAnnSig ::
  AnnSig
    typ
    (Sum TermSig (MetaAppSig Raw.MetavarId))
    (Raw.Pattern, Raw.ScopedTerm)
    Raw.Term ->
  Raw.Term
convertFromAnnSig (AnnSig sig _) = case sig of
  -- user constructors: just reuse the function that was generated
  -- for the original signature
  L2 termSig ->
    convertFromTermSig termSig
  -- metavariable application
  R2 (MetaAppSig m args) ->
    foldl Raw.AppTerm (Raw.WTerm m) args --  X t1 … tn

fromFoilPatternScoped ::
  TypedScopedSOAS FoilPattern Raw.MetavarId TermSig n typ ->
  (Raw.Pattern, Raw.ScopedTerm)
fromFoilPatternScoped (ScopedAST pat body) =
  (fromFoilPattern mkId pat, Raw.ScopedTerm (fromTypedSOAS body))
  where
    mkId n = Raw.Id ("x" ++ show n)

fromTypedSOAS ::
  TypedSOAS FoilPattern Raw.MetavarId TermSig n typ ->
  Raw.Term
fromTypedSOAS =
  convertFromAST
    convertFromAnnSig
    Raw.OTerm
    (fromFoilPattern mkId)
    Raw.ScopedTerm
    mkId
  where
    mkId n = Raw.Id ("x" ++ show n)

instance Show (TypedSOAS FoilPattern Raw.MetavarId TermSig n typ) where
  show = Raw.printTree . fromTypedSOAS

instance
  (Show typ, forall n. Show (TypedSOAS binder metavar sig n typ)) =>
  Show (Constraint typ metavar binder sig)
  where
  show (Constraint _ _ lhs rhs) =
    show lhs <> " = " <> show rhs

deriving instance Show (Foil.NameBinderList n l)

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ)) =>
  Show (Substitution typ metavar binder sig)
  where
  show (Substitution meta parameters body) =
    show meta <> "[" <> intercalate ", " parameters' <> "] ↦ " <> show body
    where
      parameters' = fmap (\name -> "x" <> show name) (namesOfPattern parameters)

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ)) =>
  Show (Substitutions typ metavar binder sig)
  where
  show (Substitutions []) = "{ }"
  show (Substitutions substitutions) = "{ " <> intercalate ", " (show <$> substitutions) <> " }"

-- ** Patterns

withVar :: (Distinct n) => Scope n -> (forall l. (DExt n l) => NameBinder n l -> Scope l -> r) -> r
withVar scope makeTerm = withFresh scope $ \x -> makeTerm x (extendScope x scope)

pattern Lam' ::
  binder n l ->
  AST binder (AnnSig () (Sum TermSig q)) l ->
  AST binder (AnnSig () (Sum TermSig q)) n
pattern Lam' binder body =
  Node (AnnSig (L2 (AbsTermSig (ScopedAST binder body))) ())

lam' ::
  (Distinct n) =>
  Scope n ->
  (forall l. (DExt n l) => Name l -> Scope l -> TypedSOAS FoilPattern Raw.MetavarId TermSig l ()) ->
  TypedSOAS FoilPattern Raw.MetavarId TermSig n ()
lam' scope makeBody = withFresh scope $ \x ->
  let body = makeBody (nameOf x) (extendScope x scope)
      binder = FoilPatternVar x
   in Lam' binder body

pattern App' ::
  AST binder (AnnSig () (Sum TermSig q)) n ->
  AST binder (AnnSig () (Sum TermSig q)) n ->
  AST binder (AnnSig () (Sum TermSig q)) n
pattern App' f x = Node (AnnSig (L2 (AppTermSig f x)) ())

------------------------

-- >>> let id1 = lam' Foil.emptyScope (\x _ -> Var x)
-- >>> id1
-- λ x0 . x0

-- | A simple example with two lambda abstractions
-- >>> let id1 = lam' Foil.emptyScope (\x _ -> Var x)
-- >>> let id2 = lam' Foil.emptyScope (\y _ -> Var y)
-- >>> let c = Constraint Foil.NameBinderListEmpty Foil.emptyNameMap id1 id2
-- >>> unify (Substitutions [], c)
-- { }

-- | Same variables on both sides
-- >>> let parameters x = NameBinderListCons x NameBinderListEmpty
-- >>> let parameterTypes x typ = addNameBinder x typ emptyNameMap
-- >>> withFresh Foil.emptyScope $ \x -> unify(Substitutions [], (Constraint (parameters x) (parameterTypes x ()) (Var (nameOf x)) (Var (nameOf x)) :: Constraint () Raw.MetavarId FoilPattern TermSig))
-- { }

-- | Different variables
-- >>> let parameters2 x y = NameBinderListCons x (NameBinderListCons y NameBinderListEmpty)
-- >>> let parameterTypes2 x y typ1 typ2 = addNameBinder y typ2 (addNameBinder x typ1 emptyNameMap)
-- >>> withFresh Foil.emptyScope $ \x -> withFresh (Foil.extendScope x Foil.emptyScope) $ \y -> unify (Substitutions [], (Constraint (parameters2 x y) (parameterTypes2 x y () ()) (Var (Foil.sink (nameOf x))) (Var (nameOf y)) :: Constraint () Raw.MetavarId FoilPattern TermSig))
-- not unifiable in rigid–rigid case

-- | Two same function applications
-- >>> let parameters x = NameBinderListCons x NameBinderListEmpty
-- >>> let parameterTypes x typ = addNameBinder x typ emptyNameMap
-- >>> withFresh Foil.emptyScope $ \x -> unify(Substitutions [], (Constraint (parameters x) (parameterTypes x ()) (App' (Var (nameOf x)) (Var (nameOf x))) (App' (Var (nameOf x)) (Var (nameOf x))) :: Constraint () Raw.MetavarId FoilPattern TermSig))
-- { }

-- | Two syntactically equal function applications
-- >>> testSameApplication
-- { }
testSameApplication ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testSameApplication =
  withVar Foil.emptyScope $ \x _ ->
    let tm = App' (Var (nameOf x)) (Var (nameOf x))
        binders = NameBinderListCons x NameBinderListEmpty
        typeEnv = addNameBinder x () emptyNameMap
        constr =
          Constraint binders typeEnv tm tm ::
            Constraint () Raw.MetavarId FoilPattern TermSig
     in unify (Substitutions [], constr)

-- | Two different function applications
-- >>> testDifferentApplications
-- not unifiable in rigid–rigid case
testDifferentApplications ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testDifferentApplications =
  withVar Foil.emptyScope $ \x scope1 ->
    withVar scope1 $ \y _ ->
      let term1 = Foil.sink (App' (Var (nameOf x)) (Var (nameOf x)))
          term2 = App' (Var (Foil.sink (nameOf x))) (Var (nameOf y))
          binders = NameBinderListCons x $ NameBinderListCons y NameBinderListEmpty
          typeEnv =
            addNameBinder y () $
              addNameBinder x () emptyNameMap
          constr =
            Constraint binders typeEnv term1 term2 ::
              Constraint () Raw.MetavarId FoilPattern TermSig
       in unify (Substitutions [], constr)


-- >>> testFlexRigid
-- { MetavarId "X"[x0, x1] ↦ x0 W x1 }

testFlexRigid ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testFlexRigid =
  withVar Foil.emptyScope $ \x scope1 ->
    withVar scope1 $ \y _ ->
      let _X = Raw.MetavarId "X"
          _W = Raw.MetavarId "W"
          termFlex = Foil.sink (MetaApp _X [Var (nameOf y), Foil.sink (Var (nameOf x))] ())
          termFlexInternal = MetaApp _W [Foil.sink (Var (nameOf x))] ()
          termRigid = App' (Var (Foil.sink (nameOf y))) termFlexInternal
          binders = NameBinderListCons x $ NameBinderListCons y NameBinderListEmpty
          typeEnv =
            addNameBinder y () $
              addNameBinder x () emptyNameMap
          constr =
            Constraint binders typeEnv termRigid termFlex ::
              Constraint () Raw.MetavarId FoilPattern TermSig
       in unify (Substitutions [], constr)


-- >>> testFlexRigidPruning
-- { MetavarId "W"[x0] ↦ W', MetavarId "X"[x0] ↦ x0 W' }

testFlexRigidPruning ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testFlexRigidPruning =
  withVar Foil.emptyScope $ \x scope1 ->
    withVar scope1 $ \y _ ->
      let _X = Raw.MetavarId "X"
          _W = Raw.MetavarId "W"
          termFlex = Foil.sink (MetaApp _X [Var (nameOf x)] ())
          termFlexInternal = MetaApp _W [Foil.sink (Var (nameOf y)), Foil.sink (Var (nameOf x))] ()
          termRigid = App' (Var (Foil.sink (nameOf x))) termFlexInternal
          binders = NameBinderListCons x $ NameBinderListCons y NameBinderListEmpty
          typeEnv =
            addNameBinder y () $
              addNameBinder x () emptyNameMap
          constr =
            Constraint binders typeEnv termRigid termFlex ::
              Constraint () Raw.MetavarId FoilPattern TermSig
       in unify (Substitutions [], constr)

-- >>> testArgumentRestriction
-- argument restriction failed

testArgumentRestriction ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testArgumentRestriction =
  withVar Foil.emptyScope $ \x _ ->
    let termRigid = Var (nameOf x)
        _X = Raw.MetavarId "X"
        termFlex = MetaApp _X [MetaApp _X [] ()] ()
        binders = NameBinderListCons x NameBinderListEmpty
        typeEnv = addNameBinder x () emptyNameMap
        constr =
          Constraint binders typeEnv termRigid termFlex ::
            Constraint () Raw.MetavarId FoilPattern TermSig
     in unify (Substitutions [], constr)

-- >>> testLocalRestriction
-- local restriction failed

testLocalRestriction ::
  Substitutions () Raw.MetavarId FoilPattern TermSig
testLocalRestriction =
  withVar Foil.emptyScope $ \x _ ->
    let termRigid = Var (nameOf x)
        _X = Raw.MetavarId "X"
        termFlex = MetaApp _X [Var (nameOf x), Var (nameOf x)] ()
        binders = NameBinderListCons x NameBinderListEmpty
        typeEnv = addNameBinder x () emptyNameMap
        constr =
          Constraint binders typeEnv termRigid termFlex ::
            Constraint () Raw.MetavarId FoilPattern TermSig
     in unify (Substitutions [], constr)
