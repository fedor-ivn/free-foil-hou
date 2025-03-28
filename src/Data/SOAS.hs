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

module Data.SOAS where

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
import Data.Maybe (mapMaybe, maybeToList)
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

class TypedSignature sig metavar t where
  mapSigWithTypes
    :: Foil.NameMap n t
    -- ^ Типы переменных в текущем контексте
    -> Map metavar ([t], t)
    -- ^ Типы метапеременных
    -> (ScopedAST binder (AnnSig t sig') n -> (t, t) -> scopedTerm)
    -- ^ что делать с информацией о типах в подвыражениях со связанными переменными
    -> (AST binder (AnnSig t sig') n -> t -> term)
    -- ^ что делать с информацией о типах в простых подвыражениях
    -> sig
        (ScopedAST binder (AnnSig t sig') n)
        (AST binder (AnnSig t sig') n)
    -- ^ исходный узел (синтаксическая конструкция)
    -> t
    -- ^ (ожидаемый) тип узла
    -> Maybe (sig scopedTerm term)
    -- ^ результат

-- debugSig
--   :: AST binder (AnnSig t sig) n
--   -> String

class TypedBinder binder t where
  addBinderTypes
    :: binder n l
    -> t
    -> Foil.NameMap n t
    -> Foil.NameMap l t

  mapBinderWithTypes
    :: (Foil.NameBinder n l -> t -> a)
    -> t
    -> binder n l
    -> [a]

instance (Ord metavar, Eq typ) => TypedSignature (MetaAppSig metavar) metavar typ where
  mapSigWithTypes _ metavarTypes _ g (MetaAppSig metavar args) expectedType = do
    (argTypes, returnType) <- Map.lookup metavar metavarTypes
    guard (returnType == expectedType)
    guard (length args == length argTypes)
    let args' = zipWith g args argTypes
    return (MetaAppSig metavar args')

instance (TypedSignature sig mv typ) => TypedSignature (AnnSig t sig) mv typ where
  mapSigWithTypes varTypes metavarTypes f g (AnnSig sig t) expectedType = do
    sig' <- mapSigWithTypes varTypes metavarTypes f g sig expectedType
    return (AnnSig sig' t)

instance
  (TypedSignature sig1 mv typ, TypedSignature sig2 mv typ)
  => TypedSignature (Sum sig1 sig2) mv typ
  where
  mapSigWithTypes varTypes metavarTypes f g (L2 sig) expectedType =
    L2 <$> mapSigWithTypes varTypes metavarTypes f g sig expectedType
  mapSigWithTypes varTypes metavarTypes f g (R2 sig) expectedType =
    R2 <$> mapSigWithTypes varTypes metavarTypes f g sig expectedType

type TypedSOAS binder metavar sig n t =
  AST binder (AnnSig t (Sum sig (MetaAppSig metavar))) n
type TypedScopedSOAS binder metavar sig n t =
  ScopedAST binder (AnnSig t (Sum sig (MetaAppSig metavar))) n

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
    -> Foil.NameMap n t
    -- ^ Types of parameters.
    -> AST binder sig n
    -- ^ Term to substitute the metavariable with.
    -> MetaAbs binder sig t

-- | A metavariable substitution is a pair of a metavariable name and its body.
newtype MetaSubst binder sig metavar t = MetaSubst
  { getMetaSubst :: (metavar, MetaAbs binder sig t)
  }

-- | A collection of metavariable substitutions (for simultaneous substitution
-- | of multiple metavariables).
newtype MetaSubsts binder sig metavar t = MetaSubsts
  { getMetaSubsts :: [MetaSubst binder sig metavar t]
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
  -> MetaSubsts binder (AnnSig t (Sum sig (MetaAppSig metavar'))) metavar t
  -- ^ Metavariable substitutions.
  -> TypedSOAS binder metavar sig n t
  -- ^ The original SOAS term.
  -> TypedSOAS binder metavar' sig n t
applyMetaSubsts scope substs = \case
  Var x -> Var x
  MetaApp metavar args ann ->
    case lookup metavar (getMetaSubst <$> getMetaSubsts substs) of
      Just (MetaAbs names _types body) ->
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
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK t
     )
  => [MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t]
  -> [MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t]
combineMetaSubsts [] = []
combineMetaSubsts (subst : substs) = foldr (mapMaybe . combine) [subst] substs
 where
  combine (MetaSubsts xs) (MetaSubsts ys)
    | conflicts = trace "there are conflicts" Nothing
    | otherwise = trace "no conflicts" return (MetaSubsts (xs ++ ys))
   where
    conflicts = or $ do
      MetaSubst (m, MetaAbs binders _types body) <- xs
      MetaSubst (m', MetaAbs binders' _types' body') <- ys
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
     , TypedSignature sig metavar t
     , TypedSignature ext metavar t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     , ZipMatchK t
     )
  => Foil.Scope n
  -- ^ The current scope.
  -> Map metavar ([t], t) -- big theta
  -> Foil.NameMap n t -- big gamma
  -> (TypedSOAS binder metavar sig n t, t)
  -- ^ The left-hand side (with metavariables that we solve for).
  -> (AST binder (AnnSig t (Sum sig ext)) n, t)
  -- ^ The right hand side (rigid). todo: should we annotate ext as well?
  -> [MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t]
match scope metavarTypes varTypes (lhs, lhsReturnType) (rhs, rhsReturnType) =
  trace "matching non-scoped lhs and rhs" $
    case (lhs, rhs) of
      (Var x, Var y) | x == y -> trace "matched same vars" return (MetaSubsts [])
      (Node (AnnSig (R2 (MetaAppSig metavar args)) _metavarType), _) ->
        case trace "looking up metavar" Map.lookup metavar metavarTypes of
          Just (argTypes, leftType) | leftType == rhsReturnType ->
            withFreshNameBinderList
              argTypes
              Foil.emptyScope
              Foil.emptyNameMap
              Foil.NameBinderListEmpty
              $ \scope' metavarTypes' binderList ->
                let argsWithTypes = zip args argTypes
                 in trace
                      "matching metavar"
                      map
                      ( \(term, MetaSubsts substs) ->
                          let metaAbs = MetaAbs binderList metavarTypes' term
                              subst = MetaSubst (metavar, metaAbs)
                           in MetaSubsts (subst : substs)
                      )
                      ( matchMetavar
                          scope'
                          metavarTypes
                          binderList
                          scope
                          varTypes
                          argsWithTypes
                          (rhs, leftType)
                      )
          _ -> []
      -- AppSig t1 t2 -- left term
      -- AppSig a1 a2 -- right term
      -- AppSig (t1, a1) (t2, a2) -- node
      -- AppSig [s1, s2] [s3, s4] -- bimap _ (match ...) node
      -- [AppSig s1 s3, AppSig s1 s4, AppSig s2 s3, AppSig s2 s4] -- bitraverse _ (match ...) node
      -- [s1 + s3, s1 + s4, s2 + s3, s2 + s4] -- map (combineMetaSubsts' . biList) ...
      -- [[s13], [], [], [s24]]
      ( Node (AnnSig (L2 lhsNode) _)
        , Node (AnnSig (L2 rhsNode) _)
        ) ->
          do
            leftNodeWithTypes <-
              maybeToList $
                mapSigWithTypes varTypes metavarTypes (,) (,) lhsNode lhsReturnType
            rightNodeWithTypes <-
              maybeToList $
                mapSigWithTypes varTypes metavarTypes (,) (,) rhsNode rhsReturnType
            case zipMatch2 leftNodeWithTypes rightNodeWithTypes of
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

swapSum
  :: Sum sig1 sig2 scope term
  -> Sum sig2 sig1 scope term
swapSum (L2 x) = R2 x
swapSum (R2 y) = L2 y

swapAnnSum
  :: AnnSig t (Sum sig1 sig2) scope term
  -> AnnSig t (Sum sig2 sig1) scope term
swapAnnSum (AnnSig sig t) = AnnSig (swapSum sig) t

transAST
  :: (Bifunctor sig)
  => (forall a b. sig a b -> sig' a b)
  -> AST binder sig n
  -> AST binder sig' n
transAST _phi (Var x) = Var x
transAST phi (Node node) =
  Node (phi (bimap (transScopedAST phi) (transAST phi) node))

transScopedAST
  :: (Bifunctor sig)
  => (forall a b. sig a b -> sig' a b)
  -> ScopedAST binder sig n
  -> ScopedAST binder sig' n
transScopedAST phi (ScopedAST binder body) =
  ScopedAST binder (transAST phi body)

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
     , TypedSignature sig metavar t
     , TypedSignature ext metavar t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
     , ZipMatchK t
     )
  => Foil.Scope n
  -> Map metavar ([t], t)
  -> Foil.NameMap n t
  -> (TypedScopedSOAS binder metavar sig n t, (t, t))
  -> (ScopedAST binder (AnnSig t (Sum sig ext)) n, (t, t))
  -> [MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t]
matchScoped
  scope
  metavarTypes
  varTypes
  (ScopedAST binder lhs, (binderTypeLhs, lhsReturnType))
  (ScopedAST binder' rhs, (binderTypeRhs, rhsReturnType)) =
    case trace "matching scoped terms" Foil.unifyPatterns binder binder' of
      -- \x.t1 = \x.t2
      Foil.SameNameBinders _ ->
        case trace "same name binders" Foil.assertDistinct binder of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder scope
                varTypes' = addBinderTypes binder binderTypeLhs varTypes
             in match scope' metavarTypes varTypes' (lhs, lhsReturnType) (rhs, rhsReturnType)
      -- \x.t1 = \y.t2
      Foil.RenameLeftNameBinder _ rename ->
        case trace "rename left binder" Foil.assertDistinct binder' of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder' scope
                varTypes' = addBinderTypes binder' binderTypeRhs varTypes
                lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) lhs
             in match scope' metavarTypes varTypes' (lhs', lhsReturnType) (rhs, rhsReturnType)
      Foil.RenameRightNameBinder _ rename ->
        case trace "rename right binder" Foil.assertDistinct binder of
          Foil.Distinct ->
            let scope' = Foil.extendScopePattern binder scope
                varTypes' = addBinderTypes binder binderTypeLhs varTypes
                rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) rhs
             in match scope' metavarTypes varTypes' (lhs, lhsReturnType) (rhs', rhsReturnType)
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
     , TypedSignature sig metavar t
     , TypedSignature ext metavar t
     , TypedBinder binder t
     , ZipMatchK t
     , ZipMatchK (Sum sig (MetaAppSig metavar))
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
  -> [(TypedSOAS binder metavar sig n t, t)]
  -- ^ A list of arguments of the parametrised metavariable on the left-hand side.
  -> (AST binder (AnnSig t (Sum sig ext)) n, t)
  -- ^ The right-hand side term (rigid).
  -> [ ( AST binder (AnnSig t (Sum sig ext)) m
       , MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t
       )
     ]
matchMetavar metavarScope metavarTypes metavarNameBinders scope varTypes argsWithTypes (rhs, expectedType) =
  let projections = project metavarNameBinders argsWithTypes
      imitations = trace "imitate on: " $ case rhs of
        Var _ -> []
        Node sig -> do
          let maybeSig = mapSigWithTypes varTypes metavarTypes (,) (,) sig expectedType
          sigWithTypes <- maybeToList maybeSig
          traversedSig <-
            bitraverse
              (matchMetavarScoped metavarScope metavarTypes metavarNameBinders scope varTypes argsWithTypes)
              (matchMetavar metavarScope metavarTypes metavarNameBinders scope varTypes argsWithTypes)
              sigWithTypes
          let term = Node (bimap fst fst traversedSig)
          substs <- combineMetaSubsts (biList (bimap snd snd (trace "end imitate on: " traversedSig)))
          return (term, substs)
   in trace (show $ map length [projections, imitations]) $
        projections ++ imitations
 where
  project
    :: (Foil.Distinct i)
    => Foil.NameBinderList i m
    -> [(TypedSOAS binder metavar sig n t, t)]
    -> [ ( AST binder (AnnSig t (Sum sig ext)) m
         , MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t
         )
       ]
  project Foil.NameBinderListEmpty [] = []
  project (Foil.NameBinderListCons x xs) (argWithType : argsWithTypes') =
    case Foil.assertDistinct x of
      Foil.Distinct ->
        case (Foil.assertExt xs, Foil.assertDistinct xs) of
          (Foil.Ext, Foil.Distinct) ->
            let substs = match scope metavarTypes varTypes argWithType (rhs, expectedType)
                term = Var (Foil.sink (Foil.nameOf x))
             in map (term,) substs ++ project xs argsWithTypes'
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
     , TypedSignature sig metavar t
     , TypedSignature ext metavar t
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
  -> [(TypedSOAS binder metavar sig n t, t)]
  -> (ScopedAST binder (AnnSig t (Sum sig ext)) n, (t, t))
  -> [ ( ScopedAST binder (AnnSig t (Sum sig ext)) m
       , MetaSubsts binder (AnnSig t (Sum sig ext)) metavar t
       )
     ]
matchMetavarScoped
  metavarScope
  metavarTypes
  metavarNameBinders
  scope
  varTypes
  argsWithTypes
  (ScopedAST binder rhs, (binderType, bodyType)) =
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
                  names = mapBinderWithTypes (,) binderType binder
                  argsWithTypes' = map (first Foil.sink) argsWithTypes ++ map (first (Var . Foil.nameOf)) names
                  varTypes' = addBinderTypes binder binderType varTypes
                  result =
                    matchMetavar
                      metavarScope'
                      metavarTypes
                      metavarNameBinders'
                      scope'
                      varTypes'
                      argsWithTypes'
                      (rhs, bodyType)
               in map (first (ScopedAST metavarBinder)) result

-- | Generate fresh name binders for a list of things.
-- This is useful to generate proper name binders of metavariable parameters.
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
