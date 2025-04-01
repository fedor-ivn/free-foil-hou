{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Lambda.Huet (
  Constraint (..),
  Metavariables (..),
  Substitution (..),
  Substitutions (..),
  Problem (..),
  Solution (..),
  solve,
) where

import Control.Monad.Foil (
  CoSinkable,
  DExt,
  Distinct,
  DistinctEvidence (Distinct),
  NameBinder,
  NameBinderList (NameBinderListCons, NameBinderListEmpty),
  NameBinders,
  NameMap,
  S (VoidS),
  Scope,
  SinkableK,
  UnifyNameBinders (..),
  addNameBinder,
  addNameBinders,
  assertDistinct,
  emptyNameMap,
  emptyScope,
  extendScope,
  extendScopePattern,
  fromNameBinderRenaming,
  lookupName,
  nameOf,
  namesOfPattern,
  sink,
  unifyNameBinders,
  withFresh,
 )
import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.Internal (nameBindersList)
import Control.Monad.Foil.Relative (liftRM)
import Control.Monad.Free.Foil (AST (Node, Var), ScopedAST (ScopedAST), substitute)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.SOAS (
  AnnSig (AnnSig),
  TypedSOAS,
  TypedScopedSOAS,
  concatNameBinderLists,
  push,
  withFreshNameBinderList,
  pattern MetaApp,
  pattern Node',
 )
import Language.Lambda.Impl (
  FoilPattern (FoilAPattern),
  MetaTerm,
  TermSig (..),
  matchPattern,
  pattern App',
  pattern Lam',
  pattern MetaVar',
 )
import qualified Language.Lambda.Syntax.Abs as Raw

withVar :: (Distinct n) => Scope n -> (forall l. (DExt n l) => NameBinder n l -> Scope l -> r) -> r
withVar scope makeTerm = withFresh scope $ \x -> makeTerm x (extendScope x scope)

lam'
  :: (Distinct n)
  => Raw.Type
  -> Foil.Scope n
  -> Raw.Type
  -> (forall l. (DExt n l) => Foil.Name l -> Foil.Scope l -> MetaTerm metavar l Raw.Type)
  -> MetaTerm metavar n Raw.Type
lam' binderType scope returnType makeBody = withFresh scope $ \x ->
  let body = makeBody (nameOf x) (extendScope x scope)
   in Lam' (FoilAPattern x) binderType body (Raw.Fun binderType returnType)

type Sig typ metavar binder sig n =
  sig (TypedScopedSOAS binder metavar sig n typ) (TypedSOAS binder metavar sig n typ)

class (Eq metavar) => HuetPreunifiable typ metavar binder sig where
  -- | Bring the term to the normal form after possible substitutions.
  normalize
    :: (Distinct n)
    => Scope n
    -- ^ Current normalization scope
    -> Sig typ metavar binder sig n
    -- ^ The term to normalize
    -> typ
    -- ^ Type annotation for the term
    -> TypedSOAS binder metavar sig n typ
    -- ^ Normalized term (which may end up to be a variable or a metavariable)

  -- | Construct an imitation of a term. The imitation must be a rigid term.
  imitate
    :: (Distinct substitution)
    => Metavariables typ metavar
    -- ^ Metavariable context for the current branch; for metavariable creation
    -> NameBinderList VoidS substitution
    -- ^ Substitution parameters; for metavariable creation
    -> NameMap substitution typ
    -- ^ Types of substitution parameters; for metavariable creation
    -> NameMap rhs typ
    -- ^ Types of variables in the scope of the term to imitate
    -> Sig typ metavar binder sig rhs
    -- ^ The term to imitate
    -> Maybe (Metavariables typ metavar, Sig typ metavar binder sig substitution)
    -- ^ Constructed imitation of the term, possibly with fresh metavariables

-- >>> :set -XTypeApplications
-- >>> t = Raw.Base (Raw.VarIdent "t")
-- >>> flip = lam' t emptyScope (Raw.Fun (Raw.Fun t t) t) $ \x scope -> lam' (Raw.Fun t t) scope t $ \f _ -> App' (Var f) (Var (sink x)) t
-- >>> normalize' @_ @Raw.MetavarIdent Foil.emptyScope flip
-- λ x0 : t . λ x1 : t -> t . x1 x0
-- >>> withVar emptyScope $ \x scope -> show $ normalize' @_ @Raw.MetavarIdent scope (App' (sink flip) (Var (nameOf x)) (Raw.Fun t t))
-- "\955 x1 : t -> t . x1 x0"
-- >>> withVar emptyScope $ \x scope -> withVar scope $ \f scope' -> show $ normalize' @_ @Raw.MetavarIdent scope' (App' (App' (Foil.sink flip) (Var (sink (nameOf x))) (Raw.Fun t t)) (Var (nameOf f)) t)
-- "x1 x0"
normalize'
  :: (HuetPreunifiable typ metavar binder sig, Distinct n)
  => Scope n
  -> TypedSOAS binder metavar sig n typ
  -> TypedSOAS binder metavar sig n typ
normalize' scope node = case node of
  Var{} -> node
  MetaApp{} -> node
  Node' term typ -> normalize scope term typ

imitate'
  :: (HuetPreunifiable typ metavar binder sig, Distinct substitution)
  => Metavariables typ metavar
  -> NameBinderList VoidS substitution
  -> NameMap substitution typ
  -> NameMap rhs typ
  -> TypedSOAS binder metavar sig rhs typ
  -> Maybe (Metavariables typ metavar, TypedSOAS binder metavar sig substitution typ)
imitate' metas parameters parameterTypes forallTypes node = case node of
  Var{} -> Nothing
  MetaApp{} -> Nothing
  Node' term typ -> fmap (fmap (`Node'` typ)) (imitate metas parameters parameterTypes forallTypes term)

instance HuetPreunifiable Raw.Type Raw.MetavarIdent FoilPattern TermSig where
  normalize scope node typ = case node of
    LamSig binderType (ScopedAST binder body)
      | Foil.Distinct <- Foil.assertDistinct binder ->
          Lam' binder binderType (normalize' scope' body) typ
     where
      scope' = Foil.extendScopePattern binder scope
    AppSig function argument -> case normalize' scope function of
      Lam' binder _ body _ -> normalize' scope (substitute scope subst body)
       where
        subst = matchPattern binder argument
      function' -> App' function' (normalize' scope argument) typ
    MetavarSig{} -> Node' node typ

  imitate metas parameters parameterTypes forallTypes node = case node of
    LamSig binderType (ScopedAST (FoilAPattern binder) body) -> do
      let bodyType = typeOf (addNameBinder binder binderType forallTypes) body
      withFresh (extendScopePattern parameters emptyScope) $ \binder' -> do
        let parameters' = push binder' parameters
        let parameterTypes' = addNameBinder binder' binderType parameterTypes
        let (metas', body') = fresh parameters' parameterTypes' bodyType metas
        return (metas', LamSig binderType (ScopedAST (FoilAPattern binder') body'))
    AppSig function argument -> do
      (metas', function') <- imitate' metas parameters parameterTypes forallTypes function
      let argumentType = typeOf forallTypes argument
      let (metas'', argument') = fresh parameters parameterTypes argumentType metas'
      return (metas'', AppSig function' argument')
    MetavarSig{} -> error "wtf"

data Stream a = Stream a (Stream a) deriving (Eq, Functor)

instance Show (Stream a) where
  show _ = "Stream"

isFlexible :: MetaTerm metavar n Raw.Type -> Bool
isFlexible Var{} = False
isFlexible Lam'{} = False
isFlexible (App' function _ _) = isFlexible function
isFlexible MetaApp{} = True
isFlexible MetaVar'{} = error "wtf"

-- >>> [t, u] = Raw.Base . Raw.VarIdent <$> ["t", "u"]
-- >>> Raw.printTree $ typeOf emptyNameMap (lam' t emptyScope t $ \x _ -> Var x)
-- "t -> t"
-- >>> Raw.printTree $ withFresh emptyScope $ \a -> typeOf (addNameBinder a t emptyNameMap) (App' (lam' t (extendScope a emptyScope) t $ \x _ -> Var x) (Var (nameOf a)) t)
-- "t"
typeOf :: NameMap n typ -> AST binder (AnnSig typ sig) n -> typ
typeOf variables (Var name) = lookupName name variables
typeOf _ (Node (AnnSig _ typ)) = typ

data Constraint typ metavar binder sig where
  Constraint
    :: (Distinct n)
    => NameBinderList Foil.VoidS n
    -> NameMap n typ
    -> TypedSOAS binder metavar sig n typ
    -> TypedSOAS binder metavar sig n typ
    -> Constraint typ metavar binder sig

instance
  (Show typ, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (Constraint typ metavar binder sig)
  where
  show (Constraint binders binderTypes lhs rhs) =
    forall_ <> show lhs <> " = " <> show rhs
   where
    forall_
      | NameBinderListEmpty <- binders = ""
      | otherwise = "∀ " <> intercalate ", " (showBinder <$> namesOfPattern binders) <> ". "
    showBinder name = "x" <> show name <> ": " <> show (lookupName name binderTypes)

normalizeConstraint
  :: (HuetPreunifiable typ metavar binder sig)
  => Constraint typ metavar binder sig
  -> Constraint typ metavar binder sig
normalizeConstraint (Constraint binder types left right) =
  Constraint binder types (normalize' scope left) (normalize' scope right)
 where
  scope = extendScopePattern binder emptyScope

data Metavariables typ metavar = Metavariables
  { metavariables :: MetaTypes typ metavar
  , freshMetavariables :: Stream metavar
  }
  deriving (Show, Eq)

newtype MetaTypes typ metavar = MetaTypes {getMetaTypes :: [(metavar, MetaType typ)]} deriving (Eq)

lookupMetaType :: (Eq metavar) => metavar -> MetaTypes typ metavar -> Maybe (MetaType typ)
lookupMetaType meta (MetaTypes types) = lookup meta types

addMetaType :: metavar -> MetaType typ -> MetaTypes typ metavar -> MetaTypes typ metavar
addMetaType meta typ (MetaTypes types) = MetaTypes ((meta, typ) : types)

instance (Show metavar, Show typ) => Show (MetaTypes metavar typ) where
  show (MetaTypes []) = "{}"
  show (MetaTypes types) = "{ " <> intercalate ", " types' <> " }"
   where
    types' = fmap (\(name, typ) -> show name <> " : " <> show typ) types

data MetaType typ = MetaType [typ] typ deriving (Eq)

instance (Show typ) => Show (MetaType typ) where
  show (MetaType parameterTypes returnType) =
    "[" <> intercalate ", " (show <$> parameterTypes) <> "]" <> show returnType

someMetas :: Metavariables typ Raw.MetavarIdent
someMetas = Metavariables (MetaTypes []) freshMetavariables
 where
  freshMetavariables = fmap (\i -> Raw.MetavarIdent ("M" <> show i)) (nats (0 :: Integer))
  nats i = Stream i (nats (i + 1))

fresh
  :: NameBinderList VoidS n
  -> NameMap n typ
  -> typ
  -> Metavariables typ metavar
  -> (Metavariables typ metavar, TypedSOAS binder metavar sig n typ)
fresh parameters parameterTypes returnType (Metavariables metavariables (Stream meta freshMetavariables)) =
  (Metavariables metavariables' freshMetavariables, term)
 where
  parameterTypes' = fmap (`lookupName` parameterTypes) (namesOfPattern parameters)
  metavariables' = addMetaType meta (MetaType parameterTypes' returnType) metavariables
  term = MetaApp meta (Var <$> namesOfPattern parameters) returnType

fresh'
  :: MetaType typ
  -> NameBinderList VoidS n
  -> Metavariables typ metavar
  -> (Metavariables typ metavar, TypedSOAS binder metavar sig n typ)
fresh' metaType@(MetaType _ returnType) parameters (Metavariables metavariables (Stream meta freshMetavariables)) =
  (Metavariables metavariables' freshMetavariables, term)
 where
  metavariables' = addMetaType meta metaType metavariables
  term = MetaApp meta (Var <$> namesOfPattern parameters) returnType

data Substitution typ metavar binder sig where
  Substitution
    :: metavar
    -> NameBinderList VoidS n
    -> TypedSOAS binder metavar sig n typ
    -> Substitution typ metavar binder sig

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (Substitution typ metavar binder sig)
  where
  show (Substitution meta parameters body) =
    show meta <> "[" <> intercalate ", " parameters' <> "] ↦ " <> show body
   where
    parameters' = fmap (\name -> "x" <> show name) (namesOfPattern parameters)

applySubstitutionInTerm
  :: (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n)
  => Substitution typ metavar binder sig
  -> Scope n
  -> TypedSOAS binder metavar sig n typ
  -> TypedSOAS binder metavar sig n typ
applySubstitutionInTerm substitution scope node = case node of
  Var{} -> node
  MetaApp meta arguments _
    | Substitution expectedMeta parameters body <- substitution
    , meta == expectedMeta ->
        let nameMap = toNameMap Foil.emptyNameMap parameters arguments
            substs' = Foil.nameMapToSubstitution nameMap
         in substitute scope substs' body
  MetaApp meta parameters typ ->
    MetaApp meta (applySubstitutionInTerm substitution scope <$> parameters) typ
  Node' term typ -> Node' (bimap goScoped go term) typ
   where
    go = applySubstitutionInTerm substitution scope
    goScoped (ScopedAST binder term)
      | Distinct <- assertDistinct binder =
          ScopedAST binder (applySubstitutionInTerm substitution scope' term)
     where
      scope' = extendScopePattern binder scope

toNameMap
  :: Foil.NameMap m a
  -> Foil.NameBinderList m l
  -> [a]
  -> Foil.NameMap l a
toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) =
  toNameMap (Foil.addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

applySubstitutionInConstraint
  :: (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig)
  => Substitution typ metavar binder sig
  -> Constraint typ metavar binder sig
  -> Constraint typ metavar binder sig
applySubstitutionInConstraint substitution (Constraint forall_ forallTypes left right) =
  Constraint forall_ forallTypes (go left) (go right)
 where
  go = applySubstitutionInTerm substitution (extendScopePattern forall_ emptyScope)

newtype Substitutions typ metavar binder sig = Substitutions [Substitution typ metavar binder sig]

makeSubstitutions :: Metavariables typ metavar -> Substitutions typ metavar binder sig
makeSubstitutions metas = Substitutions (uncurry go <$> getMetaTypes (metavariables metas))
 where
  go name (MetaType arguments returnType) =
    withFreshNameBinderList arguments emptyScope emptyNameMap NameBinderListEmpty $ \_ _ binder ->
      let body = MetaApp name (Var <$> namesOfPattern binder) returnType
       in Substitution name binder body

applySubstitutionInSubstitutions
  :: (Eq metavar, CoSinkable binder, SinkableK binder, Bifunctor sig)
  => Substitution typ metavar binder sig
  -> Substitutions typ metavar binder sig
  -> Substitutions typ metavar binder sig
applySubstitutionInSubstitutions substitution (Substitutions substitutions) =
  Substitutions (go <$> substitutions)
 where
  go (Substitution meta parameters body)
    | Distinct <- assertDistinct parameters =
        let scope = extendScopePattern parameters emptyScope
            body' = applySubstitutionInTerm substitution scope body
         in Substitution meta parameters body'

instance
  (Show metavar, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (Substitutions typ metavar binder sig)
  where
  show (Substitutions []) = "{ }"
  show (Substitutions substitutions) = "{ " <> intercalate ", " (show <$> substitutions) <> " }"

data Problem typ metavar binder sig = Problem
  { problemMetavariables :: Metavariables typ metavar
  , problemConstraints :: [Constraint typ metavar binder sig]
  }

deriving instance
  (Show metavar, Show typ, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (Problem typ metavar binder sig)

data Solution typ metavar binder sig = Solution
  { solutionMetavariables :: Metavariables typ metavar
  , solutionConstraints :: [Constraint typ metavar binder sig]
  , solutionSubstitutions :: Substitutions typ metavar binder sig
  }

deriving instance
  (Show metavar, Show typ, forall n. Show (TypedSOAS binder metavar sig n typ))
  => Show (Solution typ metavar binder sig)

withSubstitutions :: Problem typ metavar binder sig -> Solution typ metavar binder sig
withSubstitutions (Problem metas constraints) =
  Solution metas constraints (makeSubstitutions metas)

pickFlexRigid
  :: Solution Raw.Type metavar FoilPattern TermSig
  -> Maybe
      ( Constraint Raw.Type metavar FoilPattern TermSig
      , Solution Raw.Type metavar FoilPattern TermSig
      )
pickFlexRigid (Solution metas constraints substitutions) = go id constraints
 where
  go _ [] = Nothing
  go previous (constraint@(Constraint _ _ left right) : rest)
    | isFlexible left /= isFlexible right =
        Just (constraint, Solution metas (previous rest) substitutions)
    | otherwise = go ((constraint :) . previous) rest

splitProblems
  :: [Solution Raw.Type metavar FoilPattern TermSig]
  -> ( [Solution Raw.Type metavar FoilPattern TermSig]
       -> [Solution Raw.Type metavar FoilPattern TermSig]
     , [ ( Constraint Raw.Type metavar FoilPattern TermSig
         , Solution Raw.Type metavar FoilPattern TermSig
         )
       ]
     )
splitProblems = go id id
 where
  go solved unsolved [] = (solved, unsolved [])
  go solved unsolved (problem : problems) = case pickFlexRigid problem of
    Just flexRigid -> go solved ((flexRigid :) . unsolved) problems
    Nothing -> go ((problem :) . solved) unsolved problems

data Decomposition typ metavar binder sig
  = Failed
  | Flexible
  | Decomposed [Constraint typ metavar binder sig]

unifyWithBinder
  :: (CoSinkable binder, SinkableK binder, Bifunctor sig, Distinct n)
  => NameBinderList VoidS n
  -> NameMap n typ
  -> TypedSOAS binder metavar sig l typ
  -> TypedSOAS binder metavar sig r typ
  -> [typ]
  -> NameBinders n s
  -> (NameBinder n l -> NameBinder n s)
  -> (NameBinder n r -> NameBinder n s)
  -> Constraint typ metavar binder sig
unifyWithBinder forall_ types left right unifiedBinderTypes unifiedBinder leftRenaming rightRenaming
  | Distinct <- assertDistinct unifiedBinder =
      Constraint
        (concatNameBinderLists (nameBindersList unifiedBinder) forall_)
        (addNameBinders unifiedBinder unifiedBinderTypes types)
        (liftRM scope' (fromNameBinderRenaming leftRenaming) left)
        (liftRM scope' (fromNameBinderRenaming rightRenaming) right)
 where
  scope' = extendScopePattern unifiedBinder (extendScopePattern forall_ emptyScope)

data UnifyNameBinders' (pattern_ :: S -> S -> Type) n l r where
  RenameBothBinders'
    :: NameBinders n lr
    -> (NameBinder n l -> NameBinder n lr)
    -> (NameBinder n r -> NameBinder n lr)
    -> UnifyNameBinders' pattern_ n l r
  NotUnifiable' :: UnifyNameBinders' pattern_ n l r

unifyNameBinders'
  :: forall i l r pattern_
   . (Distinct i)
  => NameBinder i l
  -> NameBinder i r
  -> UnifyNameBinders' pattern_ i l r
unifyNameBinders' l r = case unifyNameBinders l r of
  NotUnifiable -> NotUnifiable'
  RenameBothBinders b l r -> RenameBothBinders' b l r
  RenameLeftNameBinder b l -> RenameBothBinders' b l id
  RenameRightNameBinder b r -> RenameBothBinders' b id r
  SameNameBinders b -> RenameBothBinders' b id id

decompose
  :: Metavariables Raw.Type metavar
  -> Constraint Raw.Type metavar FoilPattern TermSig
  -> Decomposition Raw.Type metavar FoilPattern TermSig
decompose _ (Constraint forall_ types left right) = case (left, right) of
  (MetaApp{}, _) -> Flexible
  (_, MetaApp{}) -> Flexible
  (Var left', Var right') | left' == right' -> Decomposed []
  ( Lam' (FoilAPattern leftBinder) leftType leftBody _
    , Lam' (FoilAPattern rightBinder) rightType rightBody _
    )
      | leftType == rightType
      , RenameBothBinders' unifiedBinder leftRenaming rightRenaming <-
          unifyNameBinders' leftBinder rightBinder ->
          Decomposed
            [ unifyWithBinder forall_ types leftBody rightBody [leftType] unifiedBinder leftRenaming rightRenaming
            ]
  (App' leftFunction leftArgument _, App' rightFunction rightArgument _) ->
    Decomposed
      [ Constraint forall_ types leftFunction rightFunction
      , Constraint forall_ types leftArgument rightArgument
      ]
  _ -> Failed

decomposeRigidRigid
  :: Metavariables Raw.Type metavar
  -> Constraint Raw.Type metavar FoilPattern TermSig
  -> Decomposition Raw.Type metavar FoilPattern TermSig
decomposeRigidRigid metas constraint@(Constraint _ _ left right)
  | isFlexible left = Flexible
  | isFlexible right = Flexible
  | otherwise = decompose metas constraint

decomposeAll
  :: (Constraint typ metavar binder sig -> Decomposition typ metavar binder sig)
  -> [Constraint typ metavar binder sig]
  -> Maybe [Constraint typ metavar binder sig]
decomposeAll _ [] = Just []
decomposeAll f (constraint : rest) = case f constraint of
  Failed -> Nothing
  Flexible -> (constraint :) <$> decomposeAll f rest
  Decomposed constraints -> decomposeAll f (constraints <> rest)

decomposeProblems
  :: [Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig]
  -> [Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig]
decomposeProblems problems = do
  Solution metas constraints substitutions <- problems
  constraints' <- maybeToList (decomposeAll (decomposeRigidRigid metas) (normalizeConstraint <$> constraints))
  return (Solution metas constraints' substitutions)

-- >>> (_M, t) = (Raw.MetavarIdent "M", Raw.Base (Raw.VarIdent "t"))
-- >>> metas = Metavariables (MetaTypes [(_M, MetaType [] t)]) (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (Constraint NameBinderListEmpty emptyNameMap (MetaApp _M [] t) (lam' t emptyScope t $ \x _ -> Var x))
-- Just M[] ↦ λ x0 : t . M0 [x0]
imitationRule
  :: (HuetPreunifiable typ metavar binder sig)
  => Metavariables typ metavar
  -> Constraint typ metavar binder sig
  -> Maybe (Metavariables typ metavar, Substitution typ metavar binder sig)
imitationRule metas (Constraint _ forallTypes left right) = case (left, right) of
  (MetaApp meta _ _, rhs) -> go meta rhs
  (rhs, MetaApp meta _ _) -> go meta rhs
  _ -> Nothing
 where
  go meta rhs = do
    MetaType parameterTypes _ <- lookupMetaType meta (metavariables metas)
    withFreshNameBinderList parameterTypes emptyScope emptyNameMap NameBinderListEmpty $
      \_ parameterTypes' parameters -> do
        (metas', imitation) <- imitate' metas parameters parameterTypes' forallTypes rhs
        return (metas', Substitution meta parameters imitation)

-- >>> (t, u, v) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"), Raw.Base (Raw.VarIdent "v"))
-- >>> parameterTypes = [v]
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t t (Var (nameOf x))
-- ["x0"]
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t u (Var (nameOf x))
-- []
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t (Raw.Fun u t) (Var (nameOf x))
-- ["x0 M0 [x0]"]
reduce
  :: Metavariables Raw.Type metavar
  -> NameBinderList VoidS n
  -> [Raw.Type]
  -> Raw.Type
  -> Raw.Type
  -> MetaTerm metavar n Raw.Type
  -> [(Metavariables Raw.Type metavar, MetaTerm metavar n Raw.Type)]
reduce metas parameters parameterTypes expectedType actualType term =
  reflexive <> typed
 where
  reflexive
    | expectedType == actualType = [(metas, term)]
    | otherwise = []
  typed = case actualType of
    Raw.Base{} -> []
    Raw.Fun argumentType returnType ->
      reduce metas' parameters parameterTypes expectedType returnType (App' term argument returnType)
     where
      (metas', argument) = fresh' (MetaType parameterTypes argumentType) parameters metas

-- >>> (t, u) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"))
-- >>> _M = Raw.MetavarIdent "M"
-- >>> (xType, yType, zType) = (Raw.Fun t (Raw.Fun u t), Raw.Fun t u, t)
-- >>> metas = Metavariables (MetaTypes [(_M, MetaType [xType, yType, zType] t)]) (freshMetavariables someMetas)
-- >>> fmap snd $ withVar emptyScope $ \x xScope -> withVar xScope $ \y yScope -> withVar yScope $ \z zScope -> project metas (Constraint (NameBinderListCons x (NameBinderListCons y (NameBinderListCons z NameBinderListEmpty))) (addNameBinder z zType (addNameBinder y yType (addNameBinder x xType emptyNameMap))) (MetaApp _M [Var (sink (nameOf x)), Var (sink (nameOf y)), Var (nameOf z)] t) (Var (nameOf z)))
-- [M[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] M1 [x0, x1, x2],M[x0,x1,x2] ↦ x2]
project
  :: (Eq metavar)
  => Metavariables Raw.Type metavar
  -> Constraint Raw.Type metavar FoilPattern TermSig
  -> [(Metavariables Raw.Type metavar, Substitution Raw.Type metavar FoilPattern TermSig)]
project metas (Constraint _ _ left right) = case (left, right) of
  (MetaApp{}, MetaApp{}) -> []
  (MetaApp meta _ _, _) -> go meta
  (_, MetaApp meta _ _) -> go meta
  _ -> []
 where
  go meta = do
    MetaType parameterTypes rhsType <- maybeToList (lookupMetaType meta (metavariables metas))
    withFreshNameBinderList parameterTypes emptyScope emptyNameMap NameBinderListEmpty $ \_ _ parameters -> do
      (parameter, parameterType) <- zip (namesOfPattern parameters) parameterTypes
      (metas', projection) <- reduce metas parameters parameterTypes rhsType parameterType (Var parameter)
      return (metas', Substitution meta parameters projection)

-- >>> (_M, _X, t) = (Raw.MetavarIdent "M", Raw.MetavarIdent "X", Raw.Base (Raw.VarIdent "t"))
-- >>> metas = Metavariables (MetaTypes [(_M, MetaType [] (Raw.Fun t t))]) (freshMetavariables someMetas)
-- >>> fmap snd $ withFresh emptyScope $ \a -> introduce metas (Constraint (NameBinderListCons a NameBinderListEmpty) (addNameBinder a t emptyNameMap) (App' (MetaApp _M [] (Raw.Fun t t)) (MetaApp _X [] t) t) (Var (nameOf a)))
-- [M[] ↦ λ x0 : t . M0 [x0]]
introduce
  :: (Eq metavar)
  => Metavariables Raw.Type metavar
  -> Constraint Raw.Type metavar FoilPattern TermSig
  -> [(Metavariables Raw.Type metavar, Substitution Raw.Type metavar FoilPattern TermSig)]
introduce metas (Constraint _ _ left right) = go left <> go right
 where
  go Var{} = []
  go Lam'{} = []
  go (App' (MetaApp meta _ _) _ _) = maybeToList $ do
    MetaType parameterTypes (Raw.Fun binderType returnType) <- lookupMetaType meta (metavariables metas)
    withFreshNameBinderList parameterTypes emptyScope emptyNameMap NameBinderListEmpty $ \scope _ parameters ->
      withFresh scope $ \binder ->
        let metaType = MetaType (parameterTypes <> [binderType]) returnType
            (metas', body) = fresh' metaType (push binder parameters) metas
            substitution = Lam' (FoilAPattern binder) binderType body (Raw.Fun binderType returnType)
         in return (metas', Substitution meta parameters substitution)
  go (App' function _ _) = go function
  go MetaApp{} = []
  go MetaVar'{} = error "wtf"

step
  :: Constraint Raw.Type Raw.MetavarIdent FoilPattern TermSig
  -> Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig
  -> [Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig]
step constraint (Solution metas constraints substitutions) =
  decomposed <> solved
 where
  imitations = imitationRule metas constraint
  projections = project metas constraint
  introductions = introduce metas constraint
  solved = do
    (metas', substitution) <- maybeToList imitations <> projections <> introductions
    let constraints' = applySubstitutionInConstraint substitution <$> (constraint : constraints)
    return (Solution metas' constraints' (applySubstitutionInSubstitutions substitution substitutions))

  -- `F[] X[] = a b` does not decompose semantically, but it decomposes
  -- structurally. Try structural decomposition once we dealt with the semantics
  decomposed = do
    _ <- introductions
    decomposition <- maybeToList (decomposeAll (decompose metas) [constraint])
    return (Solution metas (decomposition <> constraints) substitutions)

-- >>> t = Raw.Base (Raw.VarIdent "t")
-- >>> (_F, _X) = (Raw.MetavarIdent "F", Raw.MetavarIdent "X")
--
-- >>> metas = Metavariables (MetaTypes [(_F, MetaType [Raw.Fun t t, t] (Raw.Fun t t)), (_X, MetaType [Raw.Fun t t, t] t)]) (freshMetavariables someMetas)
-- >>> constraint = withVar emptyScope $ \a aScope -> withVar aScope $ \b bScope -> Constraint (NameBinderListCons a (NameBinderListCons b NameBinderListEmpty)) (addNameBinder b t (addNameBinder a (Raw.Fun t t) emptyNameMap)) (App' (Var (sink (nameOf a))) (Var (nameOf b)) t) (App' (MetaApp _F [Var (sink (nameOf a)), Var (nameOf b)] (Raw.Fun t t)) (MetaApp _X [Var (sink (nameOf a)), Var (nameOf b)] t) t)
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ MetavarIdent "F"[x0, x1] ↦ λ x2 : t . x0 x1, MetavarIdent "X"[x0, x1] ↦ X [x0, x1] },{ MetavarIdent "F"[x0, x1] ↦ x0, MetavarIdent "X"[x0, x1] ↦ x1 },{ MetavarIdent "F"[x0, x1] ↦ λ x2 : t . x2, MetavarIdent "X"[x0, x1] ↦ x0 x1 },{ MetavarIdent "F"[x0, x1] ↦ λ x2 : t . x0 x2, MetavarIdent "X"[x0, x1] ↦ x1 }]
--
-- >>> metas = Metavariables (MetaTypes [(_F, MetaType [Raw.Fun t (Raw.Fun t t), t, t] (Raw.Fun t t)), (_X, MetaType [Raw.Fun t (Raw.Fun t t), t, t] t)]) (freshMetavariables someMetas)
-- >>> constraint = withVar emptyScope $ \a aScope -> withVar aScope $ \b bScope -> withVar bScope $ \c cScope -> Constraint (NameBinderListCons a (NameBinderListCons b (NameBinderListCons c NameBinderListEmpty))) (addNameBinder c t (addNameBinder b t (addNameBinder a (Raw.Fun t (Raw.Fun t t)) emptyNameMap))) (App' (App' (Var (sink $ nameOf a)) (Var (sink $ nameOf b)) (Raw.Fun t t)) (Var (nameOf c)) t) (App' (MetaApp _F [Var $ sink $ nameOf a, Var $ sink $ nameOf b, Var $ nameOf c] (Raw.Fun t t)) (MetaApp _X [Var $ sink $ nameOf a, Var $ sink $ nameOf b, Var $ nameOf c] t) t)
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ MetavarIdent "F"[x0, x1, x2] ↦ x0 x1, MetavarIdent "X"[x0, x1, x2] ↦ x2 },{ MetavarIdent "F"[x0, x1, x2] ↦ λ x3 : t . x0 x1 x2, MetavarIdent "X"[x0, x1, x2] ↦ X [x0, x1, x2] },{ MetavarIdent "F"[x0, x1, x2] ↦ λ x3 : t . x0 x3 x2, MetavarIdent "X"[x0, x1, x2] ↦ x1 },{ MetavarIdent "F"[x0, x1, x2] ↦ λ x3 : t . x0 x1 x3, MetavarIdent "X"[x0, x1, x2] ↦ x2 },{ MetavarIdent "F"[x0, x1, x2] ↦ λ x3 : t . x3, MetavarIdent "X"[x0, x1, x2] ↦ x0 x1 x2 }]
--
-- >>> metas = Metavariables (MetaTypes [(_F, MetaType [Raw.Fun t (Raw.Fun t t), Raw.Fun t t, t] t), (_X, MetaType [Raw.Fun t t, t] (Raw.Fun t (Raw.Fun t t)))]) (freshMetavariables someMetas)
-- >>> constraint = withVar emptyScope $ \a aScope -> withVar aScope $ \b bScope -> Constraint (NameBinderListCons a (NameBinderListCons b NameBinderListEmpty)) (addNameBinder b t (addNameBinder a (Raw.Fun t t) emptyNameMap)) (MetaApp _F [MetaApp _X [Var $ sink $ nameOf a, Var $ nameOf b] (Raw.Fun t (Raw.Fun t t)), Var $ sink $ nameOf a, Var $ nameOf b] t) (App' (Var $ sink $ nameOf a) (Var $ nameOf b) t)
-- >>> take 5 $ solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ MetavarIdent "F"[x0, x1, x2] ↦ x1 x2, MetavarIdent "X"[x0, x1] ↦ X [x0, x1] },{ MetavarIdent "F"[x0, x1, x2] ↦ x1 (x0 M1 [x0, x1, x2] M2 [x0, x1, x2]), MetavarIdent "X"[x0, x1] ↦ λ x2 : t . λ x3 : t . x1 },{ MetavarIdent "F"[x0, x1, x2] ↦ x0 M0 [x0, x1, x2] M1 [x0, x1, x2], MetavarIdent "X"[x0, x1] ↦ λ x2 : t . λ x3 : t . x0 x1 },{ MetavarIdent "F"[x0, x1, x2] ↦ x0 M0 [x0, x1, x2] x2, MetavarIdent "X"[x0, x1] ↦ λ x2 : t . x0 },{ MetavarIdent "F"[x0, x1, x2] ↦ x0 M0 [x0, x1, x2] (x1 x2), MetavarIdent "X"[x0, x1] ↦ λ x2 : t . λ x3 : t . x3 }]
solve
  :: [Problem Raw.Type Raw.MetavarIdent FoilPattern TermSig]
  -> [Solution Raw.Type Raw.MetavarIdent FoilPattern TermSig]
solve = go . fmap withSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
