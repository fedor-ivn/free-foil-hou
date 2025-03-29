{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
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
  DExt,
  Distinct,
  DistinctEvidence (Distinct),
  NameBinder,
  NameBinderList (NameBinderListCons, NameBinderListEmpty),
  NameBinders,
  NameMap,
  S (VoidS),
  Scope,
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
  withFreshBinder,
 )
import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.Internal (nameBindersList)
import Control.Monad.Foil.Relative (liftRM)
import Control.Monad.Free.Foil (AST (Node, Var), substitute)
import qualified Control.Monad.Free.Foil as Foil
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.SOAS (AnnSig (AnnSig), concatNameBinderLists, push, withFreshNameBinderList, pattern MetaApp)
import Language.Lambda.Impl (
  FoilPattern (FoilAPattern),
  MetaTerm,
  fromMetaTerm,
  fromTerm,
  matchPattern,
  toBinders,
  pattern App',
  pattern Lam',
  pattern MetaVar',
 )
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Print as Raw

data Stream a = Stream a (Stream a) deriving (Eq, Functor)

instance Show (Stream a) where
  show _ = "Stream"

isFlexible :: MetaTerm metavar n Raw.Type -> Bool
isFlexible Var{} = False
isFlexible Lam'{} = False
isFlexible (App' function _ _) = isFlexible function
isFlexible MetaApp{} = True
isFlexible MetaVar'{} = error "wtf"

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

-- >>> :set -XTypeApplications
-- >>> t = Raw.Base (Raw.VarIdent "t")
-- >>> flip = lam' t emptyScope (Raw.Fun (Raw.Fun t t) t) $ \x scope -> lam' (Raw.Fun t t) scope t $ \f _ -> App' (Var f) (Var (sink x)) t
-- >>> eval @_ @Raw.MetavarIdent Foil.emptyScope flip
-- λ x0 : t . λ x1 : t -> t . x1 x0
-- >>> withVar emptyScope $ \x scope -> show $ eval @_ @Raw.MetavarIdent scope (App' (sink flip) (Var (nameOf x)) (Raw.Fun t t))
-- "\955 x1 : t -> t . x1 x0"
-- >>> withVar emptyScope $ \x scope -> withVar scope $ \f scope' -> show $ eval @_ @Raw.MetavarIdent scope' (App' (App' (Foil.sink flip) (Var (sink (nameOf x))) (Raw.Fun t t)) (Var (nameOf f)) t)
-- "x1 x0"
eval :: (Distinct n) => Scope n -> MetaTerm metavar n Raw.Type -> MetaTerm metavar n Raw.Type
eval scope node = case node of
  Var{} -> node
  Lam' binder binderType body typ
    | Foil.Distinct <- Foil.assertDistinct binder ->
        Lam' binder binderType (eval scope' body) typ
   where
    scope' = Foil.extendScopePattern binder scope
  App' function argument typ -> case eval scope function of
    Lam' binder _ body _ -> eval scope (Foil.substitute scope subst body)
     where
      subst = matchPattern binder argument
    function' -> App' function' (eval scope argument) typ
  MetaVar'{} -> error "wtf"
  MetaApp{} -> node

-- >>> [t, u] = Raw.Base . Raw.VarIdent <$> ["t", "u"]
-- >>> Raw.printTree $ typeOf emptyNameMap (lam' t emptyScope t $ \x _ -> Var x)
-- "t -> t"
-- >>> Raw.printTree $ withFresh emptyScope $ \a -> typeOf (addNameBinder a t emptyNameMap) (App' (lam' t (extendScope a emptyScope) t $ \x _ -> Var x) (Var (nameOf a)) t)
-- "t"
typeOf :: NameMap n Raw.Type -> MetaTerm metavar n Raw.Type -> Raw.Type
typeOf variables (Var name) = lookupName name variables
typeOf _ (Node (AnnSig _ typ)) = typ

data Constraint where
  Constraint
    :: (Distinct n)
    => NameBinderList Foil.VoidS n
    -> Foil.NameMap n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> Constraint

instance Show Constraint where
  show (Constraint binders binderTypes lhs rhs) =
    Raw.printTree $
      Raw.AUnificationConstraint (toBinders binders binderTypes) (fromMetaTerm' lhs) (fromMetaTerm' rhs)
   where
    fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm

evalConstraint :: Constraint -> Constraint
evalConstraint (Constraint binder types left right) =
  Constraint binder types (eval scope left) (eval scope right)
 where
  scope = extendScopePattern binder emptyScope

substituteConstraint :: Substitution -> Constraint -> Constraint
substituteConstraint substitution (Constraint forall_ forallTypes left right) =
  Constraint forall_ forallTypes (go left) (go right)
 where
  go = substituteMetavar substitution (extendScopePattern forall_ emptyScope)

data Metavariables = Metavariables
  { metavariables :: MetaTypes
  , freshMetavariables :: Stream Raw.MetavarIdent
  }
  deriving (Show, Eq)

newtype MetaTypes = MetaTypes {getMetaTypes :: [(Raw.MetavarIdent, MetaType)]} deriving (Eq)

lookupMetaType :: Raw.MetavarIdent -> MetaTypes -> Maybe MetaType
lookupMetaType meta (MetaTypes types) = lookup meta types

addMetaType :: Raw.MetavarIdent -> MetaType -> MetaTypes -> MetaTypes
addMetaType meta typ (MetaTypes types) = MetaTypes ((meta, typ) : types)

instance Show MetaTypes where
  show (MetaTypes []) = "{}"
  show (MetaTypes types) = "{ " <> intercalate ", " types' <> " }"
   where
    types' = fmap (\(Raw.MetavarIdent name, typ) -> name <> " : " <> show typ) types

data MetaType = MetaType [Raw.Type] Raw.Type deriving (Eq)

instance Show MetaType where
  show (MetaType parameterTypes returnType) =
    "[" <> intercalate ", " (Raw.printTree <$> parameterTypes) <> "]" <> Raw.printTree returnType

someMetas :: Metavariables
someMetas = Metavariables (MetaTypes []) freshMetavariables
 where
  freshMetavariables = fmap (\i -> Raw.MetavarIdent ("M" <> show i)) (nats (0 :: Integer))
  nats i = Stream i (nats (i + 1))

fresh :: MetaType -> NameBinderList VoidS n -> Metavariables -> (Metavariables, MetaTerm Raw.MetavarIdent n Raw.Type)
fresh metaType@(MetaType _ typ) parameters (Metavariables metavariables (Stream meta freshMetavariables)) =
  (Metavariables metavariables' freshMetavariables, term)
 where
  metavariables' = addMetaType meta metaType metavariables
  term = MetaApp meta (makeArguments parameters) typ

data Substitution where
  Substitution
    :: Raw.MetavarIdent
    -> NameBinderList VoidS n
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> Substitution

instance Show Substitution where
  show (Substitution (Raw.MetavarIdent meta) parameters body) =
    meta <> "[" <> intercalate "," (go parameters) <> "] ↦ " <> show body
   where
    go :: NameBinderList n l -> [String]
    go NameBinderListEmpty = []
    go (NameBinderListCons binder rest) = "x" <> show (nameOf binder) : go rest

substituteMetavar
  :: (Distinct n)
  => Substitution
  -> Scope n
  -> MetaTerm Raw.MetavarIdent n Raw.Type
  -> MetaTerm Raw.MetavarIdent n Raw.Type
substituteMetavar substitution scope node = case node of
  Var{} -> node
  Lam' binder binderType body typ
    | Distinct <- assertDistinct binder ->
        let scope' = extendScopePattern binder scope
            body' = substituteMetavar substitution scope' body
         in Lam' binder binderType body' typ
  App' function argument typ -> App' (go function) (go argument) typ
   where
    go = substituteMetavar substitution scope
  MetaVar'{} -> error "wtf"
  MetaApp meta arguments _
    | Substitution expectedMeta parameters body <- substitution
    , meta == expectedMeta ->
        let nameMap = toNameMap Foil.emptyNameMap parameters arguments
            substs' = Foil.nameMapToSubstitution nameMap
         in substitute scope substs' body
  MetaApp meta parameters typ -> MetaApp meta (substituteMetavar substitution scope <$> parameters) typ

toNameMap
  :: Foil.NameMap m a
  -> Foil.NameBinderList m l
  -> [a]
  -> Foil.NameMap l a
toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) =
  toNameMap (Foil.addNameBinder binder x nameMap) rest xs
toNameMap _ _ _ = error "mismatched name list and argument list"

newtype Substitutions = Substitutions [Substitution]

makeSubstitutions :: Metavariables -> Substitutions
makeSubstitutions metas = Substitutions (uncurry go <$> getMetaTypes (metavariables metas))
 where
  go name (MetaType arguments returnType) =
    withFreshNameBinderList arguments emptyScope emptyNameMap NameBinderListEmpty $ \_ _ binder ->
      let body = MetaApp name (makeArguments binder) returnType
       in Substitution name binder body

makeArguments :: (Distinct n) => NameBinderList n l -> [AST binder sig l]
makeArguments = fmap Var . namesOfPattern

applySubstitution :: Substitution -> Substitutions -> Substitutions
applySubstitution substitution (Substitutions substitutions) =
  Substitutions (go <$> substitutions)
 where
  go (Substitution meta parameters body)
    | Distinct <- assertDistinct parameters =
        let scope = extendScopePattern parameters emptyScope
            body' = substituteMetavar substitution scope body
         in Substitution meta parameters body'

instance Show Substitutions where
  show (Substitutions []) = "{ }"
  show (Substitutions substitutions) = "{ " <> intercalate ", " (show <$> substitutions) <> " }"

data Problem = Problem
  { problemMetavariables :: Metavariables
  , problemConstraints :: [Constraint]
  }
  deriving (Show)

data Solution = Solution
  { solutionMetavariables :: Metavariables
  , solutionConstraints :: [Constraint]
  , solutionSubstitutions :: Substitutions
  }
  deriving (Show)

withSubstitutions :: Problem -> Solution
withSubstitutions (Problem metas constraints) = Solution metas constraints (makeSubstitutions metas)

pickFlexRigid :: Solution -> Maybe (Constraint, Solution)
pickFlexRigid (Solution metas constraints substitutions) = go id constraints
 where
  go _ [] = Nothing
  go previous (constraint@(Constraint _ _ left right) : rest)
    | isFlexible left /= isFlexible right = Just (constraint, Solution metas (previous rest) substitutions)
    | otherwise = go ((constraint :) . previous) rest

splitProblems :: [Solution] -> ([Solution] -> [Solution], [(Constraint, Solution)])
splitProblems = go id id
 where
  go solved unsolved [] = (solved, unsolved [])
  go solved unsolved (problem : problems) = case pickFlexRigid problem of
    Just flexRigid -> go solved ((flexRigid :) . unsolved) problems
    Nothing -> go ((problem :) . solved) unsolved problems

data Decomposition
  = Failed
  | Flexible
  | Decomposed [Constraint]
  deriving (Show)

unifyWithBinder
  :: (Distinct n)
  => NameBinderList VoidS n
  -> NameMap n Raw.Type
  -> MetaTerm Raw.MetavarIdent l Raw.Type
  -> MetaTerm Raw.MetavarIdent r Raw.Type
  -> [Raw.Type]
  -> NameBinders n s
  -> (NameBinder n l -> NameBinder n s)
  -> (NameBinder n r -> NameBinder n s)
  -> Constraint
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

decompose :: Metavariables -> Constraint -> Decomposition
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

decomposeRigidRigid :: Metavariables -> Constraint -> Decomposition
decomposeRigidRigid metas constraint@(Constraint _ _ left right)
  | isFlexible left = Flexible
  | isFlexible right = Flexible
  | otherwise = decompose metas constraint

decomposeAll :: (Constraint -> Decomposition) -> [Constraint] -> Maybe [Constraint]
decomposeAll _ [] = Just []
decomposeAll f (constraint : rest) = case f constraint of
  Failed -> Nothing
  Flexible -> (constraint :) <$> decomposeAll f rest
  Decomposed constraints -> decomposeAll f (constraints <> rest)

decomposeProblems :: [Solution] -> [Solution]
decomposeProblems problems = do
  Solution metas constraints substitutions <- problems
  constraints' <- maybeToList (decomposeAll (decomposeRigidRigid metas) (evalConstraint <$> constraints))
  return (Solution metas constraints' substitutions)

-- >>> (_M, t) = (Raw.MetavarIdent "M", Raw.Base (Raw.VarIdent "t"))
-- >>> metas = Metavariables (MetaTypes [(_M, MetaType [] t)]) (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (Constraint NameBinderListEmpty emptyNameMap (MetaApp _M [] t) (lam' t emptyScope t $ \x _ -> Var x))
-- Just M[] ↦ λ x0 : t . M0 [x0]
imitate :: Metavariables -> Constraint -> Maybe (Metavariables, Substitution)
imitate metas (Constraint _ forallTypes left right) = case (left, right) of
  (MetaApp meta _ _, rhs) -> go meta rhs
  (rhs, MetaApp meta _ _) -> go meta rhs
  _ -> Nothing
 where
  go meta rhs = do
    MetaType parameterTypes _ <- lookupMetaType meta (metavariables metas)
    withFreshNameBinderList parameterTypes emptyScope emptyNameMap NameBinderListEmpty $ \scope _ parameters -> do
      (metas', imitation) <- go' scope metas forallTypes parameters parameterTypes rhs
      return (metas', Substitution meta parameters imitation)

  go'
    :: Scope l
    -> Metavariables
    -> NameMap r Raw.Type
    -> NameBinderList VoidS l
    -> [Raw.Type]
    -> MetaTerm Raw.MetavarIdent r Raw.Type
    -> Maybe (Metavariables, MetaTerm Raw.MetavarIdent l Raw.Type)
  go' scope metas forallTypes parameters parameterTypes rhs = case rhs of
    Var{} -> Nothing
    MetaApp{} -> Nothing
    Lam' (FoilAPattern binder) binderType body typ -> do
      let bodyType = typeOf (addNameBinder binder binderType forallTypes) body
      withFreshBinder scope $ \binder' -> do
        let metaType = MetaType (parameterTypes <> [binderType]) bodyType
        let (metas', body') = fresh metaType (push binder' parameters) metas
        return (metas', Lam' (FoilAPattern binder') binderType body' typ)
    App' function argument typ -> do
      (metas', function') <- go' scope metas forallTypes parameters parameterTypes function
      let argumentType = typeOf forallTypes argument
      let (metas'', argument') = fresh (MetaType parameterTypes argumentType) parameters metas'
      return (metas'', App' function' argument' typ)
    MetaVar'{} -> error "wtf"

-- >>> (t, u, v) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"), Raw.Base (Raw.VarIdent "v"))
-- >>> parameterTypes = [v]
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t t (Var (nameOf x))
-- ["x0"]
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t u (Var (nameOf x))
-- []
-- >>> withFresh emptyScope $ \x -> show . snd <$> reduce someMetas (NameBinderListCons x NameBinderListEmpty) parameterTypes t (Raw.Fun u t) (Var (nameOf x))
-- ["x0 M0 [x0]"]
reduce
  :: Metavariables
  -> NameBinderList VoidS n
  -> [Raw.Type]
  -> Raw.Type
  -> Raw.Type
  -> MetaTerm Raw.MetavarIdent n Raw.Type
  -> [(Metavariables, MetaTerm Raw.MetavarIdent n Raw.Type)]
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
      (metas', argument) = fresh (MetaType parameterTypes argumentType) parameters metas

-- >>> (t, u) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"))
-- >>> _M = Raw.MetavarIdent "M"
-- >>> (xType, yType, zType) = (Raw.Fun t (Raw.Fun u t), Raw.Fun t u, t)
-- >>> metas = Metavariables (MetaTypes [(_M, MetaType [xType, yType, zType] t)]) (freshMetavariables someMetas)
-- >>> fmap snd $ withVar emptyScope $ \x xScope -> withVar xScope $ \y yScope -> withVar yScope $ \z zScope -> project metas (Constraint (NameBinderListCons x (NameBinderListCons y (NameBinderListCons z NameBinderListEmpty))) (addNameBinder z zType (addNameBinder y yType (addNameBinder x xType emptyNameMap))) (MetaApp _M [Var (sink (nameOf x)), Var (sink (nameOf y)), Var (nameOf z)] t) (Var (nameOf z)))
-- [M[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] M1 [x0, x1, x2],M[x0,x1,x2] ↦ x2]
project :: Metavariables -> Constraint -> [(Metavariables, Substitution)]
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
introduce :: Metavariables -> Constraint -> [(Metavariables, Substitution)]
introduce metas (Constraint _ _ left right) = go left <> go right
 where
  go Var{} = []
  go Lam'{} = []
  go (App' (MetaApp meta _ _) _ _) = maybeToList $ do
    MetaType parameterTypes (Raw.Fun binderType returnType) <- lookupMetaType meta (metavariables metas)
    withFreshNameBinderList parameterTypes emptyScope emptyNameMap NameBinderListEmpty $ \scope _ parameters ->
      withFresh scope $ \binder ->
        let metaType = MetaType (parameterTypes <> [binderType]) returnType
            (metas', body) = fresh metaType (push binder parameters) metas
            substitution = Lam' (FoilAPattern binder) binderType body (Raw.Fun binderType returnType)
         in return (metas', Substitution meta parameters substitution)
  go (App' function _ _) = go function
  go MetaApp{} = []
  go MetaVar'{} = error "wtf"

step :: Constraint -> Solution -> [Solution]
step constraint (Solution metas constraints substitutions) =
  decomposed <> solved
 where
  imitations = imitate metas constraint
  projections = project metas constraint
  introductions = introduce metas constraint

  solved = do
    (metas', substitution) <- maybeToList imitations <> projections <> introductions
    let constraints' = substituteConstraint substitution <$> (constraint : constraints)
    return (Solution metas' constraints' (applySubstitution substitution substitutions))

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
-- [{ F[x0,x1] ↦ λ x2 : t . x0 x1, X[x0,x1] ↦ X [x0, x1] },{ F[x0,x1] ↦ x0, X[x0,x1] ↦ x1 },{ F[x0,x1] ↦ λ x2 : t . x2, X[x0,x1] ↦ x0 x1 },{ F[x0,x1] ↦ λ x2 : t . x0 x2, X[x0,x1] ↦ x1 }]
--
-- >>> metas = Metavariables (MetaTypes [(_F, MetaType [Raw.Fun t (Raw.Fun t t), t, t] (Raw.Fun t t)), (_X, MetaType [Raw.Fun t (Raw.Fun t t), t, t] t)]) (freshMetavariables someMetas)
-- >>> constraint = withVar emptyScope $ \a aScope -> withVar aScope $ \b bScope -> withVar bScope $ \c cScope -> Constraint (NameBinderListCons a (NameBinderListCons b (NameBinderListCons c NameBinderListEmpty))) (addNameBinder c t (addNameBinder b t (addNameBinder a (Raw.Fun t (Raw.Fun t t)) emptyNameMap))) (App' (App' (Var (sink $ nameOf a)) (Var (sink $ nameOf b)) (Raw.Fun t t)) (Var (nameOf c)) t) (App' (MetaApp _F [Var $ sink $ nameOf a, Var $ sink $ nameOf b, Var $ nameOf c] (Raw.Fun t t)) (MetaApp _X [Var $ sink $ nameOf a, Var $ sink $ nameOf b, Var $ nameOf c] t) t)
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ F[x0,x1,x2] ↦ x0 x1, X[x0,x1,x2] ↦ x2 },{ F[x0,x1,x2] ↦ λ x3 : t . x0 x1 x2, X[x0,x1,x2] ↦ X [x0, x1, x2] },{ F[x0,x1,x2] ↦ λ x3 : t . x0 x3 x2, X[x0,x1,x2] ↦ x1 },{ F[x0,x1,x2] ↦ λ x3 : t . x0 x1 x3, X[x0,x1,x2] ↦ x2 },{ F[x0,x1,x2] ↦ λ x3 : t . x3, X[x0,x1,x2] ↦ x0 x1 x2 }]
--
-- >>> metas = Metavariables (MetaTypes [(_F, MetaType [Raw.Fun t (Raw.Fun t t), Raw.Fun t t, t] t), (_X, MetaType [Raw.Fun t t, t] (Raw.Fun t (Raw.Fun t t)))]) (freshMetavariables someMetas)
-- >>> constraint = withVar emptyScope $ \a aScope -> withVar aScope $ \b bScope -> Constraint (NameBinderListCons a (NameBinderListCons b NameBinderListEmpty)) (addNameBinder b t (addNameBinder a (Raw.Fun t t) emptyNameMap)) (MetaApp _F [MetaApp _X [Var $ sink $ nameOf a, Var $ nameOf b] (Raw.Fun t (Raw.Fun t t)), Var $ sink $ nameOf a, Var $ nameOf b] t) (App' (Var $ sink $ nameOf a) (Var $ nameOf b) t)
-- >>> take 5 $ solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ F[x0,x1,x2] ↦ x1 x2, X[x0,x1] ↦ X [x0, x1] },{ F[x0,x1,x2] ↦ x1 (x0 M1 [x0, x1, x2] M2 [x0, x1, x2]), X[x0,x1] ↦ λ x2 : t . λ x3 : t . x1 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] M1 [x0, x1, x2], X[x0,x1] ↦ λ x2 : t . λ x3 : t . x0 x1 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] x2, X[x0,x1] ↦ λ x2 : t . x0 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] (x1 x2), X[x0,x1] ↦ λ x2 : t . λ x3 : t . x3 }]
solve :: [Problem] -> [Solution]
solve = go . fmap withSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
