{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.SOAS where

import qualified Control.Monad.Foil as Foil
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Generics.Kind.TH (deriveGenericK)
import qualified GHC.Generics as GHC

import Control.Monad.Free.Foil
import Data.ZipMatchK

data MetaAppSig metavar scope term = MetaAppSig metavar [term]
  deriving (Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig

deriveGenericK ''MetaAppSig

instance ZipMatchK a => ZipMatchK (MetaAppSig a)

-- >>> a = "λy.(λx.λy.X[x, y X[y, x]])y" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> b = "λz.(λx.λy.X[x, y X[y, x]])z" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> alphaEquiv Foil.emptyScope a b
-- True

pattern MetaApp :: metavar -> [AST binder (Sum p (MetaAppSig metavar)) n] -> AST binder (Sum p (MetaAppSig metavar)) n
pattern MetaApp metavar args = Node (R2 (MetaAppSig metavar args))

type SOAS binder metavar sig n = AST binder (Sum sig (MetaAppSig metavar)) n

data MetaAbs binder sig where
  MetaAbs :: Foil.NameBinderList Foil.VoidS n -> AST binder sig n -> MetaAbs binder sig

newtype MetaSubst binder sig metavar metavar' = MetaSubst {getMetaSubst :: (metavar, MetaAbs binder (Sum sig (MetaAppSig metavar')))}

newtype MetaSubsts binder sig metavar metavar' = MetaSubsts
  { getSubsts :: [MetaSubst binder sig metavar metavar']
  }

-- M[g, \z. z a]
-- M[x, y] -> y x
-- y = \z. z a
-- x = g
-- (\z. z a) g

-- >>> subst = "X [x0, x1] ↦ x1 x0" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- >>> term = "λg. λa. λw. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . λ x2 . x0 x1
-- >>> subst = "X [x, y] ↦ (λ z . y z) x" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- >>> term = "λg. λa. λw. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . λ x2 . x0 x1
-- >>> term = "λg. λa. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . x0 x1
applyMetaSubsts ::
  (Bifunctor sig, Eq metavar, Bifunctor (MetaAppSig metavar'), Foil.Distinct n, Foil.CoSinkable binder, Foil.SinkableK binder) =>
  (metavar -> metavar') ->
  Foil.Scope n ->
  MetaSubsts binder sig metavar metavar' ->
  SOAS binder metavar sig n ->
  SOAS binder metavar' sig n
applyMetaSubsts rename scope substs = \case
  Var x -> Var x
  Node (R2 (MetaAppSig metavar args)) ->
    let args' = map apply args
     in case lookup metavar (getMetaSubst <$> getSubsts substs) of
          Just (MetaAbs names body) ->
            let substs' =
                  Foil.nameMapToSubstitution $
                    toNameMap Foil.emptyNameMap names args'
             in substitute scope substs' body
          Nothing -> Node $ R2 $ MetaAppSig (rename metavar) args'
  Node (L2 term) -> Node $ L2 $ bimap (goScopedAST rename scope substs) apply term
  where
    apply = applyMetaSubsts rename scope substs

    toNameMap :: Foil.NameMap n a -> Foil.NameBinderList n l -> [a] -> Foil.NameMap l a
    toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
    toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) = toNameMap fresh rest xs
      where
        fresh = Foil.addNameBinder binder x nameMap
    toNameMap _ _ _ = error "mismatched name list and argument list"

    goScopedAST ::
      (Bifunctor sig, Eq metavar, Bifunctor (MetaAppSig metavar'), Foil.Distinct n, Foil.CoSinkable binder, Foil.SinkableK binder) =>
      (metavar -> metavar') ->
      Foil.Scope n ->
      MetaSubsts binder sig metavar metavar' ->
      ScopedAST binder (Sum sig (MetaAppSig metavar)) n ->
      ScopedAST binder (Sum sig (MetaAppSig metavar')) n
    goScopedAST rename' scope' substs' (ScopedAST binder body) =
      case Foil.assertDistinct binder of
        Foil.Distinct ->
          ScopedAST binder (applyMetaSubsts rename' newScope substs' body)
      where
        newScope = Foil.extendScopePattern binder scope'
