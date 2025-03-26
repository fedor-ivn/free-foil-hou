{-# LANGUAGE DeriveFunctor #-}
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

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Print as Raw

data Stream a = Stream a (Stream a) deriving (Eq, Functor)

instance Show (Stream a) where
  show _ = "Stream"

someParameters :: [Raw.VarIdent]
someParameters = fmap (\n -> Raw.VarIdent ("x" <> show n)) [0 :: Integer ..]

isFlexible :: Raw.Term -> Bool
isFlexible Raw.Var{} = False
isFlexible Raw.Lam{} = False
isFlexible Raw.Let{} = False
isFlexible (Raw.App function _) = isFlexible function
isFlexible Raw.MetaVar{} = True

-- >>> (x, f, t) = (Raw.VarIdent "x", Raw.VarIdent "f", Raw.Base (Raw.VarIdent "t"))
-- >>> Raw.printTree $ eval (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Lam (Raw.APattern f) (Raw.Fun t t) (Raw.AScopedTerm (Raw.App (Raw.Var f) (Raw.Var x))))))
-- "\955 x : t . \955 f : t -> t . f x"
-- >>> Raw.printTree $ eval (Raw.App (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Lam (Raw.APattern f) (Raw.Fun t t) (Raw.AScopedTerm (Raw.App (Raw.Var f) (Raw.Var x)))))) (Raw.Var (Raw.VarIdent "a")))
-- "\955 f : t -> t . f a"
-- >>> Raw.printTree $ eval (Raw.App (Raw.App (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Lam (Raw.APattern f) (Raw.Fun t t) (Raw.AScopedTerm (Raw.App (Raw.Var f) (Raw.Var x)))))) (Raw.Var (Raw.VarIdent "A"))) (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Var (Raw.VarIdent "B")))))
-- "B"
eval :: Raw.Term -> Raw.Term
eval node = case node of
  Raw.Var{} -> node
  Raw.Lam binder binderType (Raw.AScopedTerm body) ->
    Raw.Lam binder binderType (Raw.AScopedTerm (eval body))
  Raw.Let (Raw.APattern binder) value (Raw.AScopedTerm body) ->
    eval (substituteVars [(binder, value)] body)
  Raw.App function argument ->
    case eval function of
      Raw.Lam (Raw.APattern binder) _typ (Raw.AScopedTerm body) -> eval (substituteVars [(binder, argument)] body)
      function' -> Raw.App function' argument
  Raw.MetaVar{} -> node

-- Naive rename
--
-- >>> (x, y, z) = (Raw.VarIdent "x", Raw.VarIdent "y", Raw.VarIdent "z")
-- >>> Raw.printTree $ rename x y (Raw.App (Raw.Var x) (Raw.Var z))
-- "y z"
-- >>> Raw.printTree $ rename x y (Raw.Lam (Raw.APattern x) (Raw.Base (Raw.VarIdent "t")) (Raw.AScopedTerm (Raw.Var x)))
-- "\955 x : t . x"
rename :: Raw.VarIdent -> Raw.VarIdent -> Raw.Term -> Raw.Term
rename from to = substituteVars [(from, Raw.Var to)]

-- >>> (a, x, t, u) = (Raw.VarIdent "a", Raw.VarIdent "x", Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"))
-- >>> Raw.printTree <$> typeOf [(x, t)] [] (Raw.Lam (Raw.APattern x) u (Raw.AScopedTerm (Raw.Var x)))
-- Just "u -> u"
-- >>> Raw.printTree <$> typeOf [(a, t)] [] (Raw.App (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Var x))) (Raw.Var a))
-- Just "t"
typeOf :: [(Raw.VarIdent, Raw.Type)] -> [(Raw.MetaVarIdent, MetaType)] -> Raw.Term -> Maybe Raw.Type
typeOf variables metavariables node = case node of
  Raw.Var var -> lookup var variables
  Raw.Lam (Raw.APattern binder) binderType (Raw.AScopedTerm body) ->
    Raw.Fun binderType <$> typeOf ((binder, binderType) : variables) metavariables body
  Raw.Let (Raw.APattern binder) value (Raw.AScopedTerm body) -> do
    binderType <- typeOf variables metavariables value
    typeOf ((binder, binderType) : variables) metavariables body
  Raw.App function argument -> do
    Raw.Fun argumentType returnType <- typeOf variables metavariables function
    argumentType' <- typeOf variables metavariables argument
    if argumentType == argumentType'
      then Just returnType
      else Nothing
  Raw.MetaVar metavariable _arguments ->
    fmap (\(MetaType _ typ) -> typ) (lookup metavariable metavariables)

substituteVars :: [(Raw.VarIdent, Raw.Term)] -> Raw.Term -> Raw.Term
substituteVars substitutions node = case node of
  Raw.Var variable -> fromMaybe node (lookup variable substitutions)
  Raw.Lam (Raw.APattern binder) typ (Raw.AScopedTerm body)
    | isJust (lookup binder substitutions) -> node
    | otherwise -> Raw.Lam (Raw.APattern binder) typ (Raw.AScopedTerm (go body))
  Raw.Let (Raw.APattern binder) value (Raw.AScopedTerm body) ->
    Raw.Let (Raw.APattern binder) (go value) (Raw.AScopedTerm body')
   where
    body'
      | isJust (lookup binder substitutions) = body
      | otherwise = go body
  Raw.App function argument -> Raw.App (go function) (go argument)
  Raw.MetaVar meta arguments -> Raw.MetaVar meta (go <$> arguments)
 where
  go = substituteVars substitutions

substituteMetavar :: Substitution -> Raw.Term -> Raw.Term
substituteMetavar substitution node = case node of
  Raw.Var{} -> node
  Raw.Lam binder typ (Raw.AScopedTerm body) ->
    Raw.Lam binder typ (Raw.AScopedTerm (go body))
  Raw.App function argument -> Raw.App (go function) (go argument)
  Raw.Let binder value (Raw.AScopedTerm body) ->
    Raw.Let binder (go value) (Raw.AScopedTerm (go body))
  Raw.MetaVar meta arguments
    | Substitution expected parameters body <- substitution
    , meta == expected ->
        substituteVars (zip parameters arguments) body
    | otherwise -> Raw.MetaVar meta (go <$> arguments)
 where
  go = substituteMetavar substitution

data Constraint = Constraint
  { constraintForall :: [(Raw.VarIdent, Raw.Type)]
  , constraintLhs :: Raw.Term
  , constraintRhs :: Raw.Term
  }
  deriving (Eq)

evalConstraint :: Constraint -> Constraint
evalConstraint (Constraint forall_ left right) = Constraint forall_ (eval left) (eval right)

substituteConstraint :: Substitution -> Constraint -> Constraint
substituteConstraint substitution (Constraint forall_ left right) =
  Constraint forall_ (substituteMetavar substitution left) (substituteMetavar substitution right)

instance Show Constraint where
  show (Constraint vars left right) =
    "forall "
      <> intercalate ", " (fmap (\(Raw.VarIdent x, typ) -> x <> ": " <> Raw.printTree typ) vars)
      <> ". "
      <> Raw.printTree left
      <> " = "
      <> Raw.printTree right

data Metavariables = Metavariables
  { metavariables :: [(Raw.MetaVarIdent, MetaType)]
  , freshMetavariables :: Stream Raw.MetaVarIdent
  }
  deriving (Show, Eq)

data MetaType = MetaType [Raw.Type] Raw.Type deriving (Eq)

instance Show MetaType where
  show (MetaType parameterTypes returnType) =
    "[" <> intercalate ", " (Raw.printTree <$> parameterTypes) <> "]" <> Raw.printTree returnType

someMetas :: Metavariables
someMetas = Metavariables [] freshMetavariables
 where
  freshMetavariables = fmap (\i -> Raw.MetaVarIdent ("M" <> show i)) (nats (0 :: Integer))
  nats i = Stream i (nats (i + 1))

fresh :: [(Raw.VarIdent, Raw.Type)] -> Raw.Type -> Metavariables -> (Metavariables, Raw.Term)
fresh parameters typ (Metavariables metavariables (Stream metavar freshMetavariables)) =
  (Metavariables ((metavar, metavarType) : metavariables) freshMetavariables, Raw.MetaVar metavar parameters')
 where
  parameters' = Raw.Var . fst <$> parameters
  metavarType = MetaType (snd <$> parameters) typ

data Substitution = Substitution Raw.MetaVarIdent [Raw.VarIdent] Raw.Term

instance Show Substitution where
  show (Substitution (Raw.MetaVarIdent meta) parameters body) =
    meta <> "[" <> intercalate "," (fmap (\(Raw.VarIdent var) -> var) parameters) <> "] ↦ " <> Raw.printTree body

newtype Substitutions = Substitutions [Substitution]

makeSubstitutions :: Metavariables -> Substitutions
makeSubstitutions metas = Substitutions substitutions
 where
  substitutions = do
    (name, MetaType arguments _returnType) <- metavariables metas
    let parameters = take (length arguments) someParameters
    let reflexive = Raw.MetaVar name (Raw.Var <$> parameters)
    return (Substitution name parameters reflexive)

substitute :: Substitution -> Substitutions -> Substitutions
substitute substitution (Substitutions substitutions) =
  Substitutions (go <$> substitutions)
 where
  go (Substitution meta parameters body) =
    Substitution meta parameters (substituteMetavar substitution body)

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
  go previous (constraint@(Constraint _ left right) : rest)
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

-- >>> (t, u) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"))
-- >>> (a, b, x, y, _M) = (Raw.VarIdent "A", Raw.VarIdent "B", Raw.VarIdent "x", Raw.VarIdent "y", Raw.MetaVarIdent "M")
-- >>> decompose someMetas (Constraint [(x, t)] (Raw.Var x) (Raw.Var x))
-- Decomposed []
-- >>> decompose someMetas (Constraint [(x, t), (y, u)] (Raw.Var x) (Raw.Var y))
-- Failed
-- >>> decompose someMetas (Constraint [(x, Raw.Fun u t), (y, u), (a, Raw.Fun u t), (b, u)] (Raw.App (Raw.Var x) (Raw.Var y)) (Raw.App (Raw.Var a) (Raw.Var b)))
-- Decomposed [forall x: u -> t, y: u, A: u -> t, B: u. y = B,forall x: u -> t, y: u, A: u -> t, B: u. x = A]
-- >>> decompose someMetas (Constraint [] (Raw.Lam (Raw.APattern x) t (Raw.AScopedTerm (Raw.Var x))) (Raw.Lam (Raw.APattern y) t (Raw.AScopedTerm (Raw.Var y))))
-- Decomposed [forall x: t. x = x]
-- >>> decompose someMetas (Constraint [(x, t), (y, u)] (Raw.App (Raw.Var x) (Raw.Var y)) (Raw.Var x))
-- Failed
-- >>> decompose someMetas (Constraint [(x, t), (y, t)] (Raw.App (Raw.MetaVar _M []) (Raw.Var x)) (Raw.Var y))
-- Failed
-- >>> decompose someMetas (Constraint [(x, t), (y, t)] (Raw.MetaVar _M []) (Raw.Var y))
-- Flexible
decompose :: Metavariables -> Constraint -> Decomposition
decompose _ (Constraint forall_ left right) = case (left, right) of
  (Raw.MetaVar{}, _) -> Flexible
  (_, Raw.MetaVar{}) -> Flexible
  (Raw.Var{}, Raw.Var{}) | left == right -> Decomposed []
  ( Raw.Lam (Raw.APattern leftBinder) leftBinderType (Raw.AScopedTerm leftBody)
    , Raw.Lam (Raw.APattern rightBinder) rightBinderType (Raw.AScopedTerm rightBody)
    )
      | leftBinderType == rightBinderType ->
          Decomposed [Constraint forall' leftBody rightBody']
     where
      forall' = (leftBinder, leftBinderType) : forall_
      rightBody' = rename rightBinder leftBinder rightBody
  (Raw.App leftFunction leftArgument, Raw.App rightFunction rightArgument) ->
    Decomposed
      [ Constraint forall_ leftArgument rightArgument
      , Constraint forall_ leftFunction rightFunction
      ]
  _ -> Failed

decomposeRigidRigid :: Metavariables -> Constraint -> Decomposition
decomposeRigidRigid metas constraint@(Constraint _ left right)
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

-- >>> (x, y, _M) = (Var "x", Var "y", Metavar "M")
-- >>> (t, u, v) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"), Raw.Base (Raw.VarIdent "v"))
-- >>> metas = Metavariables [(Metavar "M", ([u], t))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Constant "B" t))
-- Just M[x0] ↦ B: t
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Constant "B" t) (Meta _M [Variable x]))
-- Just M[x0] ↦ B: t
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Variable x))
-- Nothing
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Lambda x t (Variable x)))
-- Just M[x0] ↦ \x: t. M0[x,x0]
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Apply (Constant "B" (Raw.Fun t t)) (Variable x)))
-- Just M[x0] ↦ (B: t -> t) (M0[x0])
imitate :: Metavariables -> Constraint -> Maybe (Metavariables, Substitution)
imitate metas (Constraint forall_ left right) = case (left, right) of
  (Raw.MetaVar meta _, rhs) -> go meta rhs
  (rhs, Raw.MetaVar meta _) -> go meta rhs
  _ -> Nothing
 where
  go meta rhs = do
    MetaType parameterTypes _ <- lookup meta (metavariables metas)
    let parameters = zip someParameters parameterTypes
    (metas', imitiation) <- go' metas parameters rhs
    return (metas', Substitution meta (fst <$> parameters) imitiation)

  go' metas parameters rhs = case rhs of
    Raw.Var{} -> Nothing
    Raw.Lam (Raw.APattern binder) binderType (Raw.AScopedTerm body) -> do
      bodyType <- typeOf ((binder, binderType) : forall_) (metavariables metas) body
      let (metas', body') = fresh ((binder, binderType) : parameters) bodyType metas
      return (metas', Raw.Lam (Raw.APattern binder) binderType (Raw.AScopedTerm body'))
    Raw.Let _ _ (Raw.AScopedTerm body) -> go' metas parameters body
    Raw.App function argument -> do
      (metas', function') <- go' metas parameters function
      argumentType <- typeOf forall_ (metavariables metas) argument
      let (metas'', argument') = fresh parameters argumentType metas'
      return (metas'', Raw.App function' argument')
    Raw.MetaVar{} -> Nothing

-- >>> (t, u, v) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"), Raw.Base (Raw.VarIdent "v"))
-- >>> parameters = [(Raw.VarIdent "x", v)]
-- >>> y = Raw.Var (Raw.VarIdent "y")
-- >>> Raw.printTree . snd <$> reduce someMetas parameters t t y
-- ["y"]
-- >>> Raw.printTree . snd <$> reduce someMetas parameters t u y
-- []
-- >>> Raw.printTree . snd <$> reduce someMetas parameters t (Raw.Fun u t) y
-- ["y M0 [x]"]
reduce :: Metavariables -> [(Raw.VarIdent, Raw.Type)] -> Raw.Type -> Raw.Type -> Raw.Term -> [(Metavariables, Raw.Term)]
reduce metas parameters expectedType actualType term = reflexive <> typed
 where
  reflexive
    | expectedType == actualType = [(metas, term)]
    | otherwise = []
  typed = case actualType of
    Raw.Base{} -> []
    Raw.Fun argumentType returnType -> do
      let (metas', argument) = fresh parameters argumentType metas
      reduce metas' parameters expectedType returnType (Raw.App term argument)

-- >>> (t, u) = (Raw.Base (Raw.VarIdent "t"), Raw.Base (Raw.VarIdent "u"))
-- >>> _M = Metavar "M"
-- >>> (xType, yType, zType) = (Raw.Fun t (Raw.Fun u t), Raw.Fun t u, t)
-- >>> metas = Metavariables [(_M, ([xType, yType, zType], t))] (freshMetavariables someMetas)
-- >>> snd <$> project metas (Constraint [] (Meta _M [Constant "X" xType, Constant "Y" yType, Constant "Z" zType]) (Constant "A" t))
-- [M[x0,x1,x2] ↦ ((x0) (M0[x0,x1,x2])) (M1[x0,x1,x2]),M[x0,x1,x2] ↦ x2]
project :: Metavariables -> Constraint -> [(Metavariables, Substitution)]
project metas (Constraint _ left right) = case (left, right) of
  (Raw.MetaVar{}, Raw.MetaVar{}) -> []
  (Raw.MetaVar meta _, _) -> go meta
  (_, Raw.MetaVar meta _) -> go meta
  (_, _) -> []
 where
  go meta = do
    MetaType parameterTypes rhsType <- maybeToList (lookup meta (metavariables metas))
    let parameters = zip someParameters parameterTypes
    (parameter, parameterType) <- parameters
    (metas', projection) <- reduce metas parameters rhsType parameterType (Raw.Var parameter)
    return (metas', Substitution meta (fst <$> parameters) projection)

-- >>> (_M, _X, t) = (Metavar "M", Metavar "X", Raw.Base (Raw.VarIdent "t"))
-- >>> metas = Metavariables [(_M, ([], Raw.Fun t t))] (freshMetavariables someMetas)
-- >>> snd <$> introduce metas (Constraint [] (Apply (Meta _M []) (Meta _X [])) (Apply (Constant "a" (Raw.Fun t t)) (Constant "b" t)))
-- [M[] ↦ \y0: t. M0[y0]]
introduce :: Metavariables -> Constraint -> [(Metavariables, Substitution)]
introduce metas (Constraint _ left right) = go left <|> go right
 where
  go Raw.Var{} = []
  go Raw.Lam{} = []
  go (Raw.Let _ _ (Raw.AScopedTerm inner)) = go inner
  go (Raw.App (Raw.MetaVar meta _) _) = maybeToList $ do
    MetaType parameterTypes (Raw.Fun binderType bodyType) <- lookup meta (metavariables metas)
    let binder = Raw.VarIdent "y0"
    let parameters = zip someParameters parameterTypes
    let (metas', body) = fresh ((binder, binderType) : parameters) bodyType metas
    let substitution = Raw.Lam (Raw.APattern binder) binderType (Raw.AScopedTerm body)
    return (metas', Substitution meta (fst <$> parameters) substitution)
  go (Raw.App function _) = go function
  go Raw.MetaVar{} = []

-- >>> (a, b, t) = (Raw.VarIdent "a", Raw.VarIdent "b", Raw.Base (Raw.VarIdent "t"))
-- >>> (_F, _X) = (Raw.MetaVarIdent "F", Raw.MetaVarIdent "X")
-- >>> left = Raw.App (Raw.Var a) (Raw.Var b)
-- >>> right = Raw.App (Raw.MetaVar _F []) (Raw.MetaVar _X [])
-- >>> metas = Metavariables [(_F, MetaType [] (Raw.Fun t t)), (_X, MetaType [] t)] (freshMetavariables someMetas)
-- >>> constraint = Constraint [(a, Raw.Fun t t), (b, t)] left right
-- >>> flexRigid = pickFlexRigid (Solution metas [constraint] (makeSubstitutions metas))
-- >>> uncurry step =<< maybeToList flexRigid
-- [Solution {solutionMetavariables = Metavariables {metavariables = [(MetaVarIdent "F",[]t -> t),(MetaVarIdent "X",[]t)], freshMetavariables = Stream}, solutionConstraints = [forall a: t -> t, b: t. b = X [],forall a: t -> t, b: t. a = F []], solutionSubstitutions = { F[] ↦ F [], X[] ↦ X [] }},Solution {solutionMetavariables = Metavariables {metavariables = [(MetaVarIdent "M0",[t]t),(MetaVarIdent "F",[]t -> t),(MetaVarIdent "X",[]t)], freshMetavariables = Stream}, solutionConstraints = [forall a: t -> t, b: t. a b = (λ y0 : t . M0 [y0]) X []], solutionSubstitutions = { F[] ↦ λ y0 : t . M0 [y0], X[] ↦ X [] }}]
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
    return (Solution metas' constraints' (substitute substitution substitutions))

  -- `F[] X[] = a b` does not decompose semantically, but it decomposes
  -- structurally. Try structural decomposition once we dealt with the semantics
  decomposed = do
    _ <- introductions
    decomposition <- maybeToList (decomposeAll (decompose metas) [constraint])
    return (Solution metas (decomposition <> constraints) substitutions)

-- >>> t = Raw.Base (Raw.VarIdent "t")
-- >>> (_F, _X) = (Raw.MetaVarIdent "F", Raw.MetaVarIdent "X")
-- >>> (a, b, c) = (Raw.VarIdent "a", Raw.VarIdent "b", Raw.VarIdent "c")
--
-- >>> left = Raw.App (Raw.Var a) (Raw.Var b)
-- >>> right = Raw.App (Raw.MetaVar _F [Raw.Var a, Raw.Var b]) (Raw.MetaVar _X [Raw.Var a, Raw.Var b])
-- >>> metas = Metavariables [(_F, MetaType [Raw.Fun t t, t] (Raw.Fun t t)), (_X, MetaType [Raw.Fun t t, t] t)] (freshMetavariables someMetas)
-- >>> constraint = Constraint [(a, Raw.Fun t t), (b, t)] left right
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ F[x0,x1] ↦ λ y0 : t . x0 x1, X[x0,x1] ↦ X [x0, x1] },{ F[x0,x1] ↦ x0, X[x0,x1] ↦ x1 },{ F[x0,x1] ↦ λ y0 : t . x0 y0, X[x0,x1] ↦ x1 },{ F[x0,x1] ↦ λ y0 : t . y0, X[x0,x1] ↦ x0 x1 }]
--
-- >>> left = Raw.App (Raw.App (Raw.Var a) (Raw.Var b)) (Raw.Var c)
-- >>> right = Raw.App (Raw.MetaVar _F [Raw.Var a, Raw.Var b, Raw.Var c]) (Raw.MetaVar _X [Raw.Var a, Raw.Var b, Raw.Var c])
-- >>> metas = Metavariables [(_F, MetaType [Raw.Fun t (Raw.Fun t t), t, t] (Raw.Fun t t)), (_X, MetaType [Raw.Fun t (Raw.Fun t t), t, t] t)] (freshMetavariables someMetas)
-- >>> constraint = Constraint [(a, Raw.Fun t (Raw.Fun t t)), (b, t), (c, t)] left right
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ F[x0,x1,x2] ↦ x0 x1, X[x0,x1,x2] ↦ x2 },{ F[x0,x1,x2] ↦ λ y0 : t . x0 x1 x2, X[x0,x1,x2] ↦ X [x0, x1, x2] },{ F[x0,x1,x2] ↦ λ y0 : t . y0, X[x0,x1,x2] ↦ x0 x1 x2 },{ F[x0,x1,x2] ↦ λ y0 : t . x0 y0 x2, X[x0,x1,x2] ↦ x1 },{ F[x0,x1,x2] ↦ λ y0 : t . x0 x1 y0, X[x0,x1,x2] ↦ x2 }]
--
-- >>> left = Raw.MetaVar _F [Raw.MetaVar _X [Raw.Var a, Raw.Var b], Raw.Var a, Raw.Var b]
-- >>> right = Raw.App (Raw.Var a) (Raw.Var b)
-- >>> metas = Metavariables [(_F, MetaType [Raw.Fun t (Raw.Fun t t), Raw.Fun t t, t] t), (_X, MetaType [Raw.Fun t t, t] (Raw.Fun t (Raw.Fun t t)))] (freshMetavariables someMetas)
-- >>> take 5 $ solutionSubstitutions <$> solve [Problem metas [Constraint [(a, Raw.Fun t t), (b, t)] left right]]
-- [{ F[x0,x1,x2] ↦ x1 x2, X[x0,x1] ↦ X [x0, x1] },{ F[x0,x1,x2] ↦ x1 (x0 M1 [x0, x1, x2] M2 [x0, x1, x2]), X[x0,x1] ↦ λ y0 : t . λ y0 : t . x1 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] M1 [x0, x1, x2], X[x0,x1] ↦ λ y0 : t . λ y0 : t . x0 x1 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] x2, X[x0,x1] ↦ λ y0 : t . x0 },{ F[x0,x1,x2] ↦ x0 M0 [x0, x1, x2] x2, X[x0,x1] ↦ λ y0 : t . λ y0 : t . x0 y0 }]
solve :: [Problem] -> [Solution]
solve = go . fmap withSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
