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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Lambda.FCU.FreeFoil.Syntax where

import Control.Monad.Foil qualified as Foil
import Control.Monad.Foil.Internal as FoilInternal
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
    convertFromAST,
    convertToAST,
    substitute,
  )
import Control.Monad.Free.Foil.Generic ()
import Control.Monad.Free.Foil.TH
  ( mkConvertFromFreeFoil,
    mkConvertToFreeFoil,
    mkPatternSynonyms,
    mkSignature,
  )
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (..))
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
    fromFoilPattern
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

type Substitutions = Map String (Term Foil.VoidS) -- Placeholder

applySubst :: (Distinct n) => Foil.Scope n -> Substitutions -> Term n -> Term n
applySubst scope subs term = term -- TODO, maybe applyMetavarSubst

unify ::
  (Distinct n) =>
  Foil.Scope n -> -- (bvs)
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
unify scope th (s, t) =
  let s' = applySubst scope th s
      t' = applySubst scope th t
   in cases scope th (s', t')

cases ::
  forall n. -- Use ScopedTypeVariables
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
cases scope th (s', t') =
  case (s', t') of
    -- Case: Identical terms
    _ | s' == t' -> Just th
    -- Case: Vars
    (Var v1, Var v2)
      | v1 == v2 -> Just th
      | otherwise -> Nothing
    -- Case: Lambda Abstractions
    (AbsTerm _ _, AbsTerm _ _) -> unifyAbsCase scope th (s', t')
    -- Case: Applications
    (AppTerm head1 body1, AppTerm head2 body2) -> caseRigidRigid scope th (head1, body1) (head2, body2)
    -- Case: FlexRigid
    (Node (WTermSig metaId1), term2) -> caseFlexRigid scope th (s', t')
    (term1, Node (WTermSig metaId2)) -> caseFlexRigid scope th (t', s')
    -- Fail case - none of the rules apply
    _ -> Nothing

unifyAbsCase ::
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
unifyAbsCase scope th (AbsTerm binder1 body1, AbsTerm binder2 body2) =
  case (binder1, binder2) of
    (FoilPatternVar nameBinder1, FoilPatternVar namebinder2) ->
      case Foil.assertDistinct nameBinder1 of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder1 scope
           in unify scope' th (body1, body2)
unifyAbsCase _ _ _ = Nothing

caseRigidRigid ::
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  (Term n, Term n) ->
  Maybe Substitutions
caseRigidRigid scope th (head1, body1) (head2, body2) = do
  th' <- unify scope th (head1, head2)
  unify scope th' (body1, body2)

caseFlexRigid ::
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
caseFlexRigid scope th (s', t') = Nothing

caseFlexFlexSame ::
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
caseFlexFlexSame scope th (s', t') = Nothing

caseFlexFlexDiff ::
  (Distinct n) =>
  Foil.Scope n ->
  Substitutions ->
  (Term n, Term n) ->
  Maybe Substitutions
caseFlexFlexDiff scope th (s', t') = Nothing
