{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SOAS where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import Control.Monad.Free.Foil
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.ZipMatchK
import Debug.Trace (trace)
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)

-- import qualified Language.Lambda.Syntax.Abs as Raw
-- import Unsafe.Coerce (unsafeCoerce)

data MetaAppSig metavar scope term = MetaAppSig metavar [term]
  deriving (Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig

deriveGenericK ''MetaAppSig

instance (ZipMatchK a) => ZipMatchK (MetaAppSig a)

type SOAS binder metavar sig n = AST binder (Sum sig (MetaAppSig metavar)) n
type ScopedSOAS binder metavar sig n = ScopedAST binder (Sum sig (MetaAppSig metavar)) n

type TypedSOAS binder metavar sig n t = SOAS binder metavar (AnnSig t sig) n
type TypedScopedSOAS binder metavar sig n t = ScopedSOAS binder metavar (AnnSig t sig) n

data AnnSig ann sig scope term = AnnSig
  { annSigNode :: sig scope term
  , annSigAnnotation :: ann
  }

instance (Bifunctor sig) => Bifunctor (AnnSig ann sig) where
  bimap :: (a -> b) -> (c -> d) -> AnnSig ann sig a c -> AnnSig ann sig b d
  bimap f g (AnnSig node ann) = AnnSig (bimap f g node) ann

instance (Bitraversable sig) => Bitraversable (AnnSig ann sig) where
  bitraverse :: (a -> f c) -> (b -> f d) -> AnnSig ann sig a b -> f (AnnSig ann sig c d)
  bitraverse f g (AnnSig node ann) = AnnSig <$> bitraverse f g node <*> pure ann

instance (ZipMatchK sig) => ZipMatchK (AnnSig ann sig)

-- type TypedSOAS binder metavar sig n t = SOAS binder metavar (AnnSig t sig) n

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

applyMetaSubsts
  :: forall sig metavar metavar' n binder t
   . ( Bifunctor sig
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
    case lookup metavar (getMetaSubst <$> getMetaSubsts substs) of
      Just (MetaAbs names _types body) ->
        let nameMap = toNameMap Foil.emptyNameMap names args'
            substs' = Foil.nameMapToSubstitution nameMap
         in substitute scope substs' body
      Nothing -> MetaApp (rename metavar) args'
   where
    args' = map go args
  Node (L2 term) ->
    let term' = bimap goScoped go term in Node (L2 term')
 where
  go = applyMetaSubsts rename scope substs
  goScoped (ScopedAST binder body) =
    let scope' = Foil.extendScopePattern binder scope
     in case Foil.assertDistinct binder of
          Foil.Distinct ->
            ScopedAST binder (applyMetaSubsts rename scope' substs body)

  toNameMap
    :: Foil.NameMap m a
    -> Foil.NameBinderList m l
    -> [a]
    -> Foil.NameMap l a
  toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
  toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) =
    toNameMap (Foil.addNameBinder binder x nameMap) rest xs
  toNameMap _ _ _ = error "mismatched name list and argument list"

-- M[x, y] M[y, x] = x y
-- M[x, y] M[x, y] = x y

-- TODO: refactor
combineMetaSubsts
  :: ( Eq metavar
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     )
  => [MetaSubsts binder sig metavar ext t]
  -> [MetaSubsts binder sig metavar ext t]
combineMetaSubsts = foldr go []
 where
  go x [] = [x]
  go x xs = mapMaybe (inner x) xs

  inner (MetaSubsts xs) (MetaSubsts ys)
    | conflicts = trace "there are conflicts" Nothing
    | otherwise = trace "no conflicts" return (MetaSubsts (xs ++ ys))
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
          -- FIXME: RenameLeftNameBinder _ _ ->
          _ -> error "unexpected renaming"
        | MetaSubst (m, MetaAbs binders _types body) <- xs
        , MetaSubst (m', MetaAbs binders' _types' body') <- ys
        , m == m'
        ]

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
     , Foil.Distinct n
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     , Bitraversable (AnnSig t sig)
     , ZipMatchK (Sum (AnnSig t sig) ext)
     , ZipMatchK (AnnSig t sig)
     , Eq t
     , Ord metavar
     )
  => Foil.Scope n
  -> Map metavar ([t], t) -- big theta
  -> Foil.NameMap n t -- big gamma
  -> TypedSOAS binder metavar sig n t
  -> AST binder (Sum (AnnSig t sig) ext) n -- todo: should we annotate ext as well?
  -> t
  -> [MetaSubsts binder (AnnSig t sig) metavar ext t]
match scope metavarTypes lhs rhs =
  case (lhs, rhs) of
    (Var x, Var y) | x == y -> trace "matched same vars" return (MetaSubsts [])
    (MetaApp metavar args, Node (L2 (AnnSig _ rightType))) ->
      case Map.lookup metavar metavarTypes of
        Just (argTypes, leftType) | leftType == rightType ->
          withFreshNameBinderList
            argTypes
            Foil.emptyScope
            Foil.emptyNameMap
            Foil.NameBinderListEmpty
            $ \scope' metavarTypes' binderList ->
              trace
                "matching metavar"
                map
                ( \(term, MetaSubsts substs) ->
                    let metaAbs = MetaAbs binderList metavarTypes' term
                        subst = MetaSubst (metavar, metaAbs)
                     in MetaSubsts (subst : substs)
                )
                (matchMetavar scope' metavarTypes binderList scope args rhs)
        _ -> []
    -- AppSig t1 t2 -- left term
    -- AppSig a1 a2 -- right term
    -- AppSig (t1, a1) (t2, a2) -- node
    -- AppSig [s1, s2] [s3, s4] -- bimap _ (match ...) node
    -- [AppSig s1 s3, AppSig s1 s4, AppSig s2 s3, AppSig s2 s4] -- bitraverse _ (match ...) node
    -- [s1 + s3, s1 + s4, s2 + s3, s2 + s4] -- map (combineMetaSubsts' . biList) ...
    -- [[s13], [], [], [s24]]
    ( Node (L2 (AnnSig leftNode leftType))
      , Node (L2 (AnnSig rightNode rightType))
      ) ->
        case zipMatch2 leftNode rightNode of
          Just node
            | leftType == rightType ->
                let traversed =
                      bitraverse
                        (uncurry (matchScoped scope metavarTypes))
                        (uncurry (match scope metavarTypes))
                        node
                 in trace "terms matched, combine substitutions" $
                      concatMap (combineMetaSubsts . biList) traversed
          _ -> trace "term structs doesn't match" []
    (Node (L2 _term), Node (R2 _ext)) -> []
    (_, Var _) -> []
    (Var _, _) -> []
    _ -> []

matchScoped
  :: ( Bitraversable sig
     , ZipMatchK sig
     , Foil.Distinct n
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     , Bitraversable (AnnSig t sig)
     , ZipMatchK (Sum (AnnSig t sig) ext)
     , ZipMatchK (AnnSig t sig)
     , Eq t
     , Ord metavar
     )
  => Foil.Scope n
  -> Map metavar ([t], t)
  -> TypedScopedSOAS binder metavar sig n t
  -> ScopedAST binder (Sum (AnnSig t sig) ext) n
  -> [MetaSubsts binder (AnnSig t sig) metavar ext t]
matchScoped scope metavarTypes (ScopedAST binder lhs) (ScopedAST binder' rhs) =
  case trace "matching scoped terms" Foil.unifyPatterns binder binder' of
    -- \x.t1 = \x.t2
    Foil.SameNameBinders _ ->
      case trace "same name binders" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
           in match scope' metavarTypes lhs rhs
    -- \x.t1 = \y.t2
    Foil.RenameLeftNameBinder _ rename ->
      case trace "rename left binder" Foil.assertDistinct binder' of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder' scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) lhs
           in match scope' metavarTypes lhs' rhs
    Foil.RenameRightNameBinder _ rename ->
      case trace "rename right binder" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) rhs
           in match scope' metavarTypes lhs rhs'
    Foil.RenameBothBinders binders rename1 rename2 ->
      case trace "rename both binders" Foil.assertDistinct binders of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binders scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename1) lhs
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename2) rhs
           in match scope' metavarTypes lhs' rhs'
    Foil.NotUnifiable -> trace "not unifiable" []

-- M[x, x] = x + x
-- ∀ x. M[λy:t. x, λa:t. λa:t. a] = λy:t. x
matchMetavar
  :: forall metavar n m binder sig ext t
   . ( Foil.Distinct n
     , Foil.Distinct m
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK (Sum sig ext)
     , Bitraversable (AnnSig t sig)
     , ZipMatchK (Sum (AnnSig t sig) ext)
     , ZipMatchK (AnnSig t sig)
     , Eq t
     , Ord metavar
     )
  => Foil.Scope m
  -> Map metavar ([t], t)
  -> Foil.NameBinderList Foil.VoidS m
  -> Foil.Scope n
  -> [TypedSOAS binder metavar sig n t]
  -> AST binder (Sum (AnnSig t sig) ext) n
  -> [(AST binder (Sum (AnnSig t sig) ext) m, MetaSubsts binder (AnnSig t sig) metavar ext t)]
matchMetavar metavarScope metavarTypes metavarNameBinders scope args rhs =
  let projections = project metavarNameBinders args
      imitations = case rhs of
        Node node -> do
          traversed <-
            bitraverse
              (matchMetavarScoped metavarScope metavarTypes metavarNameBinders scope args)
              (matchMetavar metavarScope metavarTypes metavarNameBinders scope args)
              (mapSigWithTypes (,) (,) node)
          let term = Node (bimap fst fst traversed)
          substs <- combineMetaSubsts (biList (bimap snd snd traversed))
          return (term, substs)
        _ -> []
   in projections ++ imitations
 where
  project
    :: (Foil.Distinct i)
    => Foil.NameBinderList i m
    -> [TypedSOAS binder metavar sig n t]
    -> [(AST binder (Sum (AnnSig t sig) ext) m, MetaSubsts binder (AnnSig t sig) metavar ext t)]
  project Foil.NameBinderListEmpty [] = []
  project (Foil.NameBinderListCons x xs) (arg : args') =
    case Foil.assertDistinct x of
      Foil.Distinct ->
        case (Foil.assertExt xs, Foil.assertDistinct xs) of
          (Foil.Ext, Foil.Distinct) ->
            let substs = match scope metavarTypes arg rhs
                term = Var (Foil.sink (Foil.nameOf x))
             in map (term,) substs ++ project xs args'
  project _ _ = error "mismatched name list and argument list"

matchMetavarScoped
  :: forall n m metavar binder sig ext t
   . ( Foil.Distinct n
     , Foil.Distinct m
     , Foil.UnifiablePattern binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK (Sum sig ext)
     , Foil.SinkableK binder
     , Bitraversable (AnnSig t sig)
     , ZipMatchK (Sum (AnnSig t sig) ext)
     , ZipMatchK (AnnSig t sig)
     , Eq t
     , Ord metavar
     --  , TypedSignature sig
     )
  => Foil.Scope m
  -> Map metavar ([t], t)
  -> Foil.NameBinderList Foil.VoidS m
  -> Foil.Scope n
  -> [TypedSOAS binder metavar sig n t]
  -> ScopedAST binder (Sum (AnnSig t sig) ext) n
  -> [(ScopedAST binder (Sum (AnnSig t sig) ext) m, MetaSubsts binder (AnnSig t sig) metavar ext t)]
matchMetavarScoped metavarScope metavarTypes metavarNameBinders scope args (ScopedAST binder rhs) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      Foil.withRefreshedPattern @_ @_ @(AST binder sig)
        metavarScope
        binder
        $ \_extendSubst metavarBinder ->
          let metavarScope' = Foil.extendScopePattern metavarBinder metavarScope
              freshNameBinders = Foil.nameBinderListOf metavarBinder
              metavarNameBinders' =
                concatNameBinderLists freshNameBinders metavarNameBinders
              scope' = Foil.extendScopePattern binder scope
              args' = map Foil.sink args ++ map Var (Foil.namesOfPattern binder)
              result = matchMetavar metavarScope' metavarTypes metavarNameBinders' scope' args' rhs
           in map (first (ScopedAST metavarBinder)) result

withFreshNameBinderList
  :: (Foil.Distinct n)
  => [t]
  -> Foil.Scope n
  -> Foil.NameMap n t
  -> Foil.NameBinderList i n
  -> ( forall l
        . (Foil.Distinct l)
       => Foil.Scope l
       -> Foil.NameMap l t
       -> Foil.NameBinderList i l
       -> r
     )
  -> r
withFreshNameBinderList [] scope typesNameMap binders cont = cont scope typesNameMap binders
withFreshNameBinderList (typ : types) scope typesNameMap binders cont =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        binders' = push binder binders
        typesNameMap' = Foil.addNameBinder binder typ typesNameMap
     in withFreshNameBinderList types scope' typesNameMap' binders' cont

push :: Foil.NameBinder i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
push x Foil.NameBinderListEmpty = Foil.NameBinderListCons x Foil.NameBinderListEmpty
push x (Foil.NameBinderListCons y ys) = Foil.NameBinderListCons y (push x ys)

concatNameBinderLists :: Foil.NameBinderList i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
concatNameBinderLists lst Foil.NameBinderListEmpty = lst
concatNameBinderLists lst (Foil.NameBinderListCons x xs) =
  Foil.NameBinderListCons x (concatNameBinderLists lst xs)
