{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SOAS where

import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.ZipMatchK
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)

data MetaAppSig metavar scope term = MetaAppSig metavar [term]
  deriving (Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig

deriveGenericK ''MetaAppSig

instance (ZipMatchK a) => ZipMatchK (MetaAppSig a)

-- >>> a = "λy.(λx.λy.X[x, y X[y, x]])y" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> b = "λz.(λx.λy.X[x, y X[y, x]])z" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> alphaEquiv Foil.emptyScope a b
-- True

type SOAS binder metavar sig n = AST binder (Sum sig (MetaAppSig metavar)) n

pattern MetaApp
  :: metavar
  -> [SOAS binder metavar sig n]
  -> SOAS binder metavar sig n
pattern MetaApp metavar args = Node (R2 (MetaAppSig metavar args))

data MetaAbs binder sig t where
  MetaAbs
    :: Foil.NameBinderList Foil.VoidS n
    -> Foil.NameMap n t
    -> AST binder sig n
    -> MetaAbs binder sig t

newtype MetaSubst binder sig metavar metavar' t = MetaSubst
  { getMetaSubst :: (metavar, MetaAbs binder (Sum sig (MetaAppSig metavar')) t)
  }

newtype MetaSubsts binder sig metavar metavar' t = MetaSubsts
  { getSubsts :: [MetaSubst binder sig metavar metavar' t]
  }

applyMetaSubsts
  :: ( Bifunctor sig
     , Eq metavar
     , Bifunctor (MetaAppSig metavar')
     , Foil.Distinct n
     , Foil.CoSinkable binder
     , Foil.SinkableK binder
     )
  => (metavar -> metavar')
  -> Foil.Scope n
  -> MetaSubsts binder sig metavar metavar' t
  -> SOAS binder metavar sig n
  -> SOAS binder metavar' sig n
applyMetaSubsts rename scope substs = \case
  Var x -> Var x
  -- FIXME: MetaApp metavar args ->
  Node (R2 (MetaAppSig metavar args)) ->
    let args' = map apply args
     in case lookup metavar (getMetaSubst <$> getSubsts substs) of
          Just (MetaAbs names _types body) ->
            let nameMap = toNameMap Foil.emptyNameMap names args'
                substs' = Foil.nameMapToSubstitution nameMap
             in substitute scope substs' body
          Nothing -> MetaApp (rename metavar) args'
  Node (L2 term) ->
    let term' = bimap (goScoped rename scope substs) apply term
     in Node (L2 term')
 where
  apply = applyMetaSubsts rename scope substs

  toNameMap :: Foil.NameMap n a -> Foil.NameBinderList n l -> [a] -> Foil.NameMap l a
  toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
  toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) = toNameMap fresh rest xs
   where
    fresh = Foil.addNameBinder binder x nameMap
  toNameMap _ _ _ = error "mismatched name list and argument list"

  goScoped
    :: ( Bifunctor sig
       , Eq metavar
       , Bifunctor (MetaAppSig metavar')
       , Foil.Distinct n
       , Foil.CoSinkable binder
       , Foil.SinkableK binder
       )
    => (metavar -> metavar')
    -> Foil.Scope n
    -> MetaSubsts binder sig metavar metavar' t
    -> ScopedAST binder (Sum sig (MetaAppSig metavar)) n
    -> ScopedAST binder (Sum sig (MetaAppSig metavar')) n
  goScoped rename' scope' substs' (ScopedAST binder body) =
    case Foil.assertDistinct binder of
      Foil.Distinct ->
        ScopedAST binder (applyMetaSubsts rename' newScope substs' body)
   where
    newScope = Foil.extendScopePattern binder scope'
