{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.SOAS (
  AnnSig (..),
  AnnBinder (..),
  TypedBinder (..),
  MetaAppSig (..),
  TypedSOAS,
  TypedScopedSOAS,
  SOAS,
  ScopedSOAS,
  MetaSubst (..),
  MetaSubsts (..),
  pattern MetaApp,
  MetaAbs (..),
  match,
  push,
  toNameMap,
  applyMetaSubsts,
)
where

import Control.Monad (guard)
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

-- | Second-order signature for metavariable applications.
-- This way, metavariables can be added to any other signature using 'Sum'.
data MetaAppSig metavar scope term
  = -- | A metavariable is always fully applied to a list of arguments.
    MetaAppSig metavar [term]
  deriving (Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig
deriveGenericK ''MetaAppSig
instance (ZipMatchK a) => ZipMatchK (MetaAppSig a)

data AnnSig ann sig scopedTerm term
  = AnnSig (sig scopedTerm term) ann
  deriving (GHC.Generic)

deriveGenericK ''AnnSig

data AnnBinder ann binder (n :: Foil.S) (l :: Foil.S)
  = AnnBinder (binder n l) ann
  deriving (GHC.Generic, Eq)

deriveGenericK ''AnnBinder

instance (Foil.CoSinkable binder) => Foil.CoSinkable (AnnBinder ann binder) where
  coSinkabilityProof rename (AnnBinder binder ann) cont =
    Foil.coSinkabilityProof rename binder (\rename' binder' -> cont rename' (AnnBinder binder' ann))

  withPattern f empty append scope (AnnBinder binder t) cont =
    Foil.withPattern f empty append scope binder (\f' binder' -> cont f' (AnnBinder binder' t))

instance (Foil.SinkableK binder) => Foil.SinkableK (AnnBinder ann binder)

instance
  (Foil.UnifiableInPattern (binder n l), Eq ann)
  => Foil.UnifiableInPattern ((AnnBinder ann binder) n l)
  where
  unifyInPattern (AnnBinder binder t) (AnnBinder binder' t') =
    t == t' && Foil.unifyInPattern binder binder'

instance (Foil.UnifiablePattern binder) => Foil.UnifiablePattern (AnnBinder t binder)

class TypedBinder binder t where
  addBinderTypes
    :: binder n l
    -> t
    -> Foil.NameMap n t
    -> Foil.NameMap l t

type TypedSOAS binder metavar sig n t =
  AST (AnnBinder t binder) (AnnSig t (Sum sig (MetaAppSig metavar))) n
type TypedScopedSOAS binder metavar sig n t =
  ScopedAST (AnnBinder t binder) (AnnSig t (Sum sig (MetaAppSig metavar))) n

-- | A second-order abstract syntax (SOAS) is generated from a signature
-- by adding parametrised metavariables.
--
-- Note that here we also parametrise SOAS by the type of binders (patterns) @binder@.
type SOAS binder metavar sig n = TypedSOAS binder metavar sig n ()

-- | A scoped version of 'SOAS'.
-- 'ScopedSOAS' is to 'SOAS' what 'ScopedAST' is to 'AST'.
type ScopedSOAS binder metavar sig n = TypedScopedSOAS binder metavar sig n ()

instance (Functor (sig a)) => Functor (AnnSig ann sig a) where
  fmap :: (a1 -> b) -> AnnSig ann sig a a1 -> AnnSig ann sig a b
  fmap f (AnnSig node ann) = AnnSig (fmap f node) ann

instance (Bifoldable sig) => Bifoldable (AnnSig ann sig) where
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> AnnSig ann sig a b -> m
  bifoldMap f g (AnnSig node _) = bifoldMap f g node

instance (Bifunctor sig) => Bifunctor (AnnSig ann sig) where
  bimap :: (a -> b) -> (c -> d) -> AnnSig ann sig a c -> AnnSig ann sig b d
  bimap f g (AnnSig node ann) = AnnSig (bimap f g node) ann

instance (Bitraversable sig) => Bitraversable (AnnSig ann sig) where
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> AnnSig ann sig a b -> f (AnnSig ann sig c d)
  bitraverse f g (AnnSig node ann) = AnnSig <$> bitraverse f g node <*> pure ann

instance (ZipMatchK ann, ZipMatchK sig) => ZipMatchK (AnnSig ann sig)

-- type TypedSOAS binder metavar sig n t = SOAS binder metavar (AnnSig t sig) n

-- | A convenient pattern synonym for parametrised metavariables.
pattern MetaApp
  :: metavar
  -> [TypedSOAS binder metavar sig n t]
  -> t -- Type annotation
  -> TypedSOAS binder metavar sig n t
pattern MetaApp metavar args ann =
  Node (AnnSig (R2 (MetaAppSig metavar args)) ann)

-- | A body of a metavariable substitution for one metavariable.
data MetaAbs binder sig t where
  MetaAbs
    :: Foil.NameBinderList Foil.VoidS n
    -- ^ A list of binders corresponding to metavariable arguments.
    -> AST binder sig n
    -- ^ Term to substitute the metavariable with.
    -> MetaAbs binder sig t

-- | A metavariable substitution is a pair of a metavariable name and its body.
newtype MetaSubst binder sig metavar t = MetaSubst
  { metaSubst :: (metavar, MetaAbs binder sig t)
  }

-- | A collection of metavariable substitutions (for simultaneous substitution
-- | of multiple metavariables).
newtype MetaSubsts binder sig metavar t = MetaSubsts
  { metaSubsts :: [MetaSubst binder sig metavar t]
  }

-- | Apply metavariable substitutions to a SOAS term.
applyMetaSubsts
  :: forall sig metavar metavar' n binder t
   . ( Bifunctor sig
     , Eq metavar
     , Foil.Distinct n
     , Foil.CoSinkable binder
     , Foil.SinkableK binder
     , metavar ~ metavar'
     )
  => Foil.Scope n
  -- ^ Scope of terms.
  -> MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig (MetaAppSig metavar'))) metavar t
  -- ^ Metavariable substitutions.
  -> TypedSOAS binder metavar sig n t
  -- ^ The original SOAS term.
  -> TypedSOAS binder metavar' sig n t
applyMetaSubsts scope substs = \case
  Var x -> Var x
  MetaApp metavar args ann ->
    case lookup metavar (metaSubst <$> metaSubsts substs) of
      Just (MetaAbs names body) ->
        let nameMap = toNameMap Foil.emptyNameMap names args'
            substs' = Foil.nameMapToSubstitution nameMap
            body' = substitute scope substs' body
         in body'
      Nothing -> MetaApp metavar args' ann
   where
    args' = map go args
  Node (AnnSig (L2 term) ann) ->
    let term' = bimap goScoped go term in Node (AnnSig (L2 term') ann)
  _ -> error "unreachable"
 where
  go = applyMetaSubsts scope substs
  goScoped (ScopedAST binder body) =
    let scope' = Foil.extendScopePattern binder scope
     in case Foil.assertDistinct binder of
          Foil.Distinct ->
            ScopedAST binder (applyMetaSubsts scope' substs body)

toNameMap
  :: Foil.NameMap m a
  -> Foil.NameBinderList m l
  -> [a]
  -> Foil.NameMap l a
toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) =
  toNameMap (Foil.addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

-- | Combine (compose) metavariable substitutions.
--
-- TODO: refactor
combineMetaSubsts
  :: ( Eq metavar
     , Bitraversable sig
     , ZipMatchK (Sum sig ext)
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK t
     , Foil.UnifiablePattern binder
     )
  => [MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t]
  -> [MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t]
combineMetaSubsts [] = []
combineMetaSubsts (subst : substs) = foldr (mapMaybe . combine) [subst] substs
 where
  combine (MetaSubsts xs) (MetaSubsts ys)
    | conflicts = trace "there are conflicts" Nothing
    | otherwise = trace "no conflicts" return (MetaSubsts (xs ++ ys))
   where
    conflicts = or $ do
      MetaSubst (m, MetaAbs binders body) <- xs
      MetaSubst (m', MetaAbs binders' body') <- ys
      guard (m == m')
      pure $
        case Foil.unifyPatterns binders binders' of
          Foil.SameNameBinders _ ->
            case Foil.assertDistinct binders of
              Foil.Distinct ->
                let scope = Foil.extendScopePattern binders Foil.emptyScope
                 in not (alphaEquiv scope body body')
          Foil.NotUnifiable -> True
          _ -> error "unexpected renaming"

-- | Match left-hand side (with metavariables) against the rigid right-hand side.
--
-- If matching is successful, it produces metavariable substitutions that when applied to LHS make it syntactically equal to RHS.
-- For example, matching
--   M[f x, g] = g (f x)
-- produces substitution
--   M[z₁, z₂] ↦ z₂ z₁
--
-- There may be more than one solution for matching, e.g.
--   M[f x, f x] = f x
-- can be solved with two different substitutions:
--   1. M[z₁, z₂] ↦ z₁
--   2. M[z₁, z₂] ↦ z₂
--
-- Hence, this function produces a list of possible substitutions.
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
     , TypedBinder binder t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     , ZipMatchK t
     )
  => Foil.Scope n
  -- ^ The current scope.
  -> Map metavar ([t], t) -- big theta
  -> Foil.NameMap n t -- big gamma
  -> TypedSOAS binder metavar sig n t
  -- ^ The left-hand side (with metavariables that we solve for).
  -> AST (AnnBinder t binder) (AnnSig t (Sum sig ext)) n
  -- ^ The right hand side (rigid). todo: should we annotate ext as well?
  -> [MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t]
match scope metavarTypes varTypes lhs rhs =
  trace "matching non-scoped lhs and rhs" $
    case (lhs, rhs) of
      (Var x, Var y) | x == y -> trace "matched same vars" return (MetaSubsts [])
      (Node (AnnSig (R2 (MetaAppSig metavar args)) metavarType), _) ->
        case trace "looking up metavar" Map.lookup metavar metavarTypes of
          -- todo: should we check here for type?
          Just (argTypes, _) ->
            withFreshNameBinderList
              argTypes
              Foil.emptyScope
              Foil.NameBinderListEmpty
              $ \scope' binderList ->
                trace
                  "matching metavar"
                  map
                  ( \(term, MetaSubsts substs) ->
                      let metaAbs = MetaAbs binderList term
                          subst = MetaSubst (metavar, metaAbs)
                       in MetaSubsts (subst : substs)
                  )
                  ( matchMetavar
                      scope'
                      metavarTypes
                      binderList
                      scope
                      varTypes
                      args
                      metavarType
                      rhs
                  )
          _ -> []
      -- AppSig t1 t2 -- left term
      -- AppSig a1 a2 -- right term
      -- AppSig (t1, a1) (t2, a2) -- node
      -- AppSig [s1, s2] [s3, s4] -- bimap _ (match ...) node
      -- [AppSig s1 s3, AppSig s1 s4, AppSig s2 s3, AppSig s2 s4] -- bitraverse _ (match ...) node
      -- [s1 + s3, s1 + s4, s2 + s3, s2 + s4] -- map (combineMetaSubsts' . biList) ...
      -- [[s13], [], [], [s24]]
      ( Node (AnnSig (L2 lhsNode) lhsReturnType)
        , Node (AnnSig (L2 rhsNode) rhsReturnType)
        ) ->
          case zipMatch2 lhsNode rhsNode of
            Just node
              | lhsReturnType == rhsReturnType ->
                  let
                    traversed =
                      bitraverse
                        (uncurry (matchScoped scope metavarTypes varTypes))
                        (uncurry (match scope metavarTypes varTypes))
                        node
                   in
                    trace "terms matched, combine substitutions" $
                      concatMap (combineMetaSubsts . biList) traversed
            _ -> trace "term structs doesn't match" []
      (Node (AnnSig (L2 _) _), Node (AnnSig (R2 _) _)) -> []
      (_, Var _) -> trace "vars didn't match: (_, Var _)" []
      (Var _, _) -> trace "vars didn't match: (Var _, _)" []

-- | Same as 'match' but for scoped terms.
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
     , TypedBinder binder t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     , ZipMatchK t
     )
  => Foil.Scope n
  -> Map metavar ([t], t)
  -> Foil.NameMap n t
  -> TypedScopedSOAS binder metavar sig n t
  -> ScopedAST (AnnBinder t binder) (AnnSig t (Sum sig ext)) n
  -> [MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t]
matchScoped
  scope
  metavarTypes
  varTypes
  (ScopedAST (AnnBinder binder ann) lhs)
  (ScopedAST (AnnBinder binder' ann') rhs) =
    case trace "matching scoped terms" Foil.unifyPatterns binder binder' of
      -- \x.t1 = \x.t2
      Foil.SameNameBinders _ ->
        case trace "same name binders" Foil.assertDistinct binder of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder scope
                varTypes' = addBinderTypes binder ann varTypes
             in match scope' metavarTypes varTypes' lhs rhs
      -- \x.t1 = \y.t2
      Foil.RenameLeftNameBinder _ rename ->
        case trace "rename left binder" Foil.assertDistinct binder' of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder' scope
                varTypes' = addBinderTypes binder' ann' varTypes
                lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) lhs
             in match scope' metavarTypes varTypes' lhs' rhs
      Foil.RenameRightNameBinder _ rename ->
        case trace "rename right binder" Foil.assertDistinct binder of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder scope
                varTypes' = addBinderTypes binder ann varTypes
                rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) rhs
             in match scope' metavarTypes varTypes' lhs rhs'
      Foil.RenameBothBinders{} -> error "not implemented"
      -- Foil.RenameBothBinders binders rename1 rename2 ->
      -- case trace "rename both binders" Foil.assertDistinct binders of
      --   Foil.Distinct -> undefined
      -- let scope' = Foil.extendScopePattern binders scope
      --     varTypes' = addBinderTypes binder binderTypeLhs varTypes
      --     lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename1) lhs
      --     rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename2) rhs
      --  in match scope' metavarTypes varTypes' lhs' rhs'
      Foil.NotUnifiable -> trace "not unifiable" []

-- | A special case of 'match', when LHS is a parametrised metavariable.
--
-- Note that here we do not have an explicit name for that metavariable,
-- and only consider its arguments.
-- This is helpful when recursively matching with fresh metavariables,
-- since we do not need to generate any actual fresh names for such metavariables,
-- saving in complexity and performance.
--
-- For each possible solution, this function produces a pair of
--
-- 1. Body of the metavariable substitution.
-- 2. Metavariable substitutions for the metavariables that occur in the parameters of the root metavariable on the LHS.
matchMetavar
  :: forall metavar n m binder sig ext t
   . ( Foil.Distinct n
     , Foil.Distinct m
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
     , TypedBinder binder t
     , ZipMatchK t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     , Foil.UnifiablePattern binder
     )
  => Foil.Scope m
  -- ^ Scope for the body of the metavariable substitution (at root of LHS).
  -> Map metavar ([t], t)
  -- ^ Map of metavariables to their types.
  -> Foil.NameBinderList Foil.VoidS m
  -- ^ List of binders for the metavariable parameters (size of the list should match the actual list of parameters).
  -> Foil.Scope n
  -- ^ Current scope.
  -> Foil.NameMap n t
  -- ^ Types of variables in the current scope.
  -> [TypedSOAS binder metavar sig n t]
  -- ^ A list of arguments of the parametrised metavariable on the left-hand side.
  -> t
  -- ^ The expected type of the right-hand side.
  -> AST (AnnBinder t binder) (AnnSig t (Sum sig ext)) n
  -- ^ The right-hand side term (rigid).
  -> [ ( AST (AnnBinder t binder) (AnnSig t (Sum sig ext)) m
       , MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t
       )
     ]
matchMetavar metavarScope metavarTypes metavarNameBinders scope varTypes args expectedType rhs =
  let projections = project metavarNameBinders args
      imitations = trace "imitate on: " $ case rhs of
        Var _ -> []
        Node sig -> do
          traversedSig <-
            bitraverse
              ( matchMetavarScoped
                  metavarScope
                  metavarTypes
                  metavarNameBinders
                  scope
                  varTypes
                  args
                  expectedType
              )
              ( matchMetavar
                  metavarScope
                  metavarTypes
                  metavarNameBinders
                  scope
                  varTypes
                  args
                  expectedType
              )
              sig
          let term = Node (bimap fst fst traversedSig)
          substs <- combineMetaSubsts (biList (bimap snd snd (trace "end imitate on: " traversedSig)))
          return (term, substs)
   in trace (show $ map length [projections, imitations]) $
        projections ++ imitations
 where
  project
    :: (Foil.Distinct i)
    => Foil.NameBinderList i m
    -> [TypedSOAS binder metavar sig n t]
    -> [ ( AST (AnnBinder t binder) (AnnSig t (Sum sig ext)) m
         , MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t
         )
       ]
  project Foil.NameBinderListEmpty [] = []
  project (Foil.NameBinderListCons x xs) (arg : args') =
    case Foil.assertDistinct x of
      Foil.Distinct ->
        case (Foil.assertExt xs, Foil.assertDistinct xs) of
          (Foil.Ext, Foil.Distinct) ->
            let substs = match scope metavarTypes varTypes arg rhs
                term = Var (Foil.sink (Foil.nameOf x))
             in map (term,) substs ++ project xs args'
  project _ _ = error "mismatched name list and argument list"

-- | Same as 'matchMetavar' but for scoped term.
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
     , TypedBinder binder t
     , ZipMatchK t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     )
  => Foil.Scope m
  -> Map metavar ([t], t)
  -> Foil.NameBinderList Foil.VoidS m
  -> Foil.Scope n
  -> Foil.NameMap n t
  -- ^ Types of variables in the current scope.
  -> [TypedSOAS binder metavar sig n t]
  -- ^ A list of arguments of the parametrised metavariable on the left-hand
  -- side.
  -> t
  -- ^ The expected type of the right-hand side.
  -> ScopedAST (AnnBinder t binder) (AnnSig t (Sum sig ext)) n
  -> [ ( ScopedAST (AnnBinder t binder) (AnnSig t (Sum sig ext)) m
       , MetaSubsts (AnnBinder t binder) (AnnSig t (Sum sig ext)) metavar t
       )
     ]
matchMetavarScoped
  metavarScope
  metavarTypes
  metavarNameBinders
  scope
  varTypes
  args
  expectedType
  (ScopedAST (AnnBinder binder binderType) rhs) =
    trace "matching metavar scoped" $
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
                  names = Foil.namesOfPattern binder
                  args' = map Foil.sink args ++ map Var names
                  varTypes' = addBinderTypes binder binderType varTypes
                  result =
                    matchMetavar
                      metavarScope'
                      metavarTypes
                      metavarNameBinders'
                      scope'
                      varTypes'
                      args'
                      expectedType
                      rhs
               in map (first (ScopedAST (AnnBinder metavarBinder binderType))) result

-- | Generate fresh name binders for a list of things.
-- This is useful to generate proper name binders of metavariable parameters.
withFreshNameBinderList
  :: (Foil.Distinct n)
  => [a]
  -> Foil.Scope n
  -> Foil.NameBinderList i n
  -> ( forall l
        . (Foil.Distinct l)
       => Foil.Scope l
       -> Foil.NameBinderList i l
       -> r
     )
  -> r
withFreshNameBinderList [] scope binders cont = cont scope binders
withFreshNameBinderList (_ : types) scope binders cont =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        binders' = push binder binders
     in withFreshNameBinderList types scope' binders' cont

-- | /O(n)/. Push a name binder into the end of a name binder list.
--
-- /Should be in "Control.Monad.Foil"./
push :: Foil.NameBinder i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
push x Foil.NameBinderListEmpty = Foil.NameBinderListCons x Foil.NameBinderListEmpty
push x (Foil.NameBinderListCons y ys) = Foil.NameBinderListCons y (push x ys)

-- | /O(n)/. Concatenate name binder lists.
--
-- Should be in "Control.Monad.Foil".
concatNameBinderLists :: Foil.NameBinderList i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
concatNameBinderLists lst Foil.NameBinderListEmpty = lst
concatNameBinderLists lst (Foil.NameBinderListCons x xs) =
  Foil.NameBinderListCons x (concatNameBinderLists lst xs)

-- swapSum
--   :: Sum sig1 sig2 scope term
--   -> Sum sig2 sig1 scope term
-- swapSum (L2 x) = R2 x
-- swapSum (R2 y) = L2 y

-- swapAnnSum
--   :: AnnSig t (Sum sig1 sig2) scope term
--   -> AnnSig t (Sum sig2 sig1) scope term
-- swapAnnSum (AnnSig sig t) = AnnSig (swapSum sig) t

-- transAST
--   :: (Bifunctor sig)
--   => (forall a b. sig a b -> sig' a b)
--   -> AST binder sig n
--   -> AST binder sig' n
-- transAST _phi (Var x) = Var x
-- transAST phi (Node node) =
--   Node (phi (bimap (transScopedAST phi) (transAST phi) node))

-- transScopedAST
--   :: (Bifunctor sig)
--   => (forall a b. sig a b -> sig' a b)
--   -> ScopedAST binder sig n
--   -> ScopedAST binder sig' n
-- transScopedAST phi (ScopedAST binder body) =
--   ScopedAST binder (transAST phi body)
