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
import qualified Control.Monad.Foil.Relative as Foil
import Control.Monad.Free.Foil
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ZipMatchK
import Debug.Trace (trace)
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)
import Unsafe.Coerce (unsafeCoerce)

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

type ScopedSOAS binder metavar sig n = ScopedAST binder (Sum sig (MetaAppSig metavar)) n

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

newtype MetaSubst binder sig metavar ext t = MetaSubst
  { getMetaSubst :: (metavar, MetaAbs binder (Sum sig ext) t)
  }

newtype MetaSubsts binder sig metavar ext t = MetaSubsts
  { getMetaSubsts :: [MetaSubst binder sig metavar ext t]
  }

-- M[x, y] M[y, x] = x y
-- M[x, y] M[x, y] = x y

combineMetaSubsts
  :: (Eq metavar, Bitraversable sig, Bitraversable ext, ZipMatchK (Sum sig ext), Foil.UnifiablePattern binder, Foil.SinkableK binder)
  => MetaSubsts binder sig metavar ext t
  -> MetaSubsts binder sig metavar ext t
  -> [MetaSubsts binder sig metavar ext t]
combineMetaSubsts (MetaSubsts xs) (MetaSubsts ys)
  | conflicts = []
  | otherwise = [MetaSubsts (xs ++ ys)]
 where
  conflicts =
    and
      [ case Foil.unifyPatterns binders binders' of
        Foil.SameNameBinders _ ->
          case Foil.assertDistinct binders of
            Foil.Distinct ->
              let scope' = Foil.extendScopePattern binders Foil.emptyScope
               in alphaEquiv scope' body body'
        Foil.NotUnifiable -> False
        -- FIXME: RenameLeftNameBinder _ _ -> undefined
        _ -> undefined
      | MetaSubst (m, MetaAbs binders _types body) <- xs
      , MetaSubst (m', MetaAbs binders' _types' body') <- ys
      , m == m'
      ]

combineMetaSubsts'
  :: (Eq metavar, Bitraversable sig, Bitraversable ext, ZipMatchK (Sum sig ext), Foil.UnifiablePattern binder, Foil.SinkableK binder)
  => [MetaSubsts binder sig metavar ext t]
  -> [MetaSubsts binder sig metavar ext t]
combineMetaSubsts' =
  foldr
    (\substs substsList -> concatMap (combineMetaSubsts substs) substsList)
    []

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
  -> MetaSubsts binder sig metavar (MetaAppSig metavar') t
  -> SOAS binder metavar sig n
  -> SOAS binder metavar' sig n
applyMetaSubsts rename scope substs = \case
  Var x -> Var x
  -- FIXME: MetaApp metavar args ->
  Node (R2 (MetaAppSig metavar args)) ->
    let args' = map apply args
     in case lookup metavar (getMetaSubst <$> getMetaSubsts substs) of
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
    -> MetaSubsts binder sig metavar (MetaAppSig metavar') t
    -> ScopedAST binder (Sum sig (MetaAppSig metavar)) n
    -> ScopedAST binder (Sum sig (MetaAppSig metavar')) n
  goScoped rename' scope' substs' (ScopedAST binder body) =
    case Foil.assertDistinct binder of
      Foil.Distinct ->
        ScopedAST binder (applyMetaSubsts rename' newScope substs' body)
   where
    newScope = Foil.extendScopePattern binder scope'

-- | This function takes the following parameters:
--
--   * `Scope n` - The current scope.
--   * `SOAS binder metavar sig n` - The left-hand side (where we want to apply
--     substitutions).
--   * `AST binder sig' n` - The right-hand side (unchangeable).
--   * `[MetaSubsts sig' metavar]` - The substitutions that turn the LHS into
--     the RHS.
match
  :: ( Bitraversable sig
     , ZipMatchK sig
     , Eq metavar
     , Foil.Distinct n
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     )
  => Foil.Scope n
  -> SOAS binder metavar sig n
  -> AST binder (Sum sig ext) n
  -> [MetaSubsts binder sig metavar ext t]
match scope lhs rhs =
  case (lhs, rhs) of
    (Var x, Var y) | x == y -> return (MetaSubsts [])
    (MetaApp metavar args, _) ->
      trace "matching metavar" map addMetaSubst (matchMetavar scope args rhs)
     where
      addMetaSubst (metaAbs, MetaSubsts substs) =
        MetaSubsts (MetaSubst (metavar, metaAbs) : substs)
    (Node (L2 leftTerm), Node (L2 rightTerm)) ->
      -- AppSig t1 t2 -- left term
      -- AppSig a1 a2 -- right term
      -- AppSig (t1, a1) (t2, a2) -- node
      -- AppSig [s1, s2] [s3, s4] -- bimap _ (match ...) node
      -- [AppSig s1 s3, AppSig s1 s4, AppSig s2 s3, AppSig s2 s4] -- bitraverse _ (match ...) node
      -- [s1 + s3, s1 + s4, s2 + s3, s2 + s4] -- map (combineMetaSubsts' . biList) ...
      -- [[s13], [], [], [s24]]
      case zipMatch2 leftTerm rightTerm of
        Just node ->
          let tmp = bitraverse (uncurry (matchScoped scope)) (uncurry (match scope)) node
           in trace "terms matched, combine substitutions" concatMap (combineMetaSubsts' . biList) tmp
        Nothing -> trace "term structs doesn't match" []
    (Node (L2 _term), Node (R2 _ext)) -> []
    (_, Var _) -> []
    (Var _, _) -> []
    _ -> []

matchScoped
  :: ( Bitraversable sig
     , ZipMatchK sig
     , Foil.Distinct n
     , Eq metavar
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     )
  => Foil.Scope n
  -> ScopedSOAS binder metavar sig n
  -> ScopedAST binder (Sum sig ext) n
  -> [MetaSubsts binder sig metavar ext t]
matchScoped scope (ScopedAST binder lhs) (ScopedAST binder' rhs) =
  case trace "matching scoped terms" Foil.unifyPatterns binder binder' of
    -- \x.t1 = \x.t2
    Foil.SameNameBinders _ ->
      case trace "same name binders" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
           in match scope' lhs rhs
    -- \x.t1 = \y.t2
    Foil.RenameLeftNameBinder _ rename ->
      case trace "rename left binder" Foil.assertDistinct binder' of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder' scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) lhs
           in match scope' lhs' rhs
    Foil.RenameRightNameBinder _ rename ->
      case trace "rename right binder" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) rhs
           in match scope' lhs rhs'
    Foil.RenameBothBinders binders rename1 rename2 ->
      case trace "rename both binders" Foil.assertDistinct binders of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binders scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename1) lhs
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename2) rhs
           in match scope' lhs' rhs'
    Foil.NotUnifiable -> trace "not unifiable" []

closed :: AST binder sig n -> Maybe (AST binder sig Foil.VoidS)
closed term = Just (unsafeCoerce term)

-- M[x, x] = x + x
matchMetavar
  :: Foil.Scope n
  -> [SOAS binder metavar sig n]
  -> AST binder (Sum sig ext) n
  -> [(MetaAbs binder (Sum sig ext) t, MetaSubsts binder sig metavar ext t)]
matchMetavar _scope args rhs =
  case args of
    [] ->
      case closed rhs of
        Just rhs' ->
          trace "term is closed, substitute with rhs" [
            ( MetaAbs Foil.NameBinderListEmpty Foil.emptyNameMap rhs'
            , MetaSubsts []
            )
          ]
        Nothing -> []
    _ -> undefined
