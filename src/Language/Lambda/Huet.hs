{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Lambda.Huet where

import Data.List (intercalate)
import Data.Maybe (maybeToList)

data Stream a = Stream a (Stream a) deriving (Eq, Functor)

instance Show (Stream a) where
  show _ = "Stream"

data Type = Base String | Function Type Type deriving (Eq)

instance Show Type where
  show (Base typ) = typ
  show (Function from to) = "(" <> show from <> ") -> " <> show to

newtype Var = Var String deriving (Eq, Show)
newtype Metavar = Metavar String deriving (Eq, Show)

someParameters :: [Var]
someParameters = fmap (\n -> Var ("x" <> show n)) [0 :: Integer ..]

data Ast
  = Constant String Type
  | Variable Var
  | Lambda Var Type Ast
  | Apply Ast Ast
  | Meta Metavar [Ast]
  deriving (Eq)

-- Naive rename
--
-- >>> (x, y, z) = (Var "x", Var "y", Var "z")
-- >>> rename x y (Apply (Variable x) (Variable z))
-- (y) (z)
-- >>> rename x y (Lambda x (Base "A") (Variable x))
-- \x: A. x
rename :: Var -> Var -> Ast -> Ast
rename from to (Variable variable)
  | variable == from = Variable to
  | otherwise = Variable variable
rename _ _ (Constant constant typ) = Constant constant typ
rename from to (Lambda binder typ body) = Lambda binder typ body'
 where
  body'
    | binder == from = body
    | otherwise = rename from to body
rename from to (Apply left right) = Apply (rename from to left) (rename from to right)
rename from to (Meta metavariable arguments) = Meta metavariable (rename from to <$> arguments)

-- >>> x = Var "x"
-- >>> typeOf [(x, Base "X")] [] (Lambda x (Base "Y") (Variable x))
-- Just (Y) -> Y
-- >>> typeOf [] [] (Apply (Lambda x (Base "A") (Variable x)) (Constant "A" (Base "A")))
-- Just A
typeOf :: [(Var, Type)] -> [(Metavar, ([Type], Type))] -> Ast -> Maybe Type
typeOf _ _ (Constant _ typ) = Just typ
typeOf variables _ (Variable var) = lookup var variables
typeOf variables metavariables (Lambda binder binderType body) =
  Function binderType <$> typeOf ((binder, binderType) : variables) metavariables body
typeOf variables metavariables (Apply function argument) = do
  Function argumentType returnType <- typeOf variables metavariables function
  argumentType' <- typeOf variables metavariables argument
  if argumentType == argumentType'
    then Just returnType
    else Nothing
typeOf _ metavariables (Meta metavariable _arguments) =
  snd <$> lookup metavariable metavariables

substituteVar :: Var -> Ast -> Ast -> Ast
substituteVar _ _ node@Constant{} = node
substituteVar expected substitution (Variable variable)
  | variable == expected = substitution
  | otherwise = Variable variable
substituteVar expected substitution node@(Lambda binder typ body)
  | binder == expected = node
  | otherwise = Lambda binder typ (substituteVar expected substitution body)
substituteVar expected substitution (Apply function argument) =
  Apply (substituteVar expected substitution function) (substituteVar expected substitution argument)
substituteVar expected substitution (Meta meta arguments) =
  Meta meta (substituteVar expected substitution <$> arguments)

substituteMetavar :: Substitution -> Ast -> Ast
substituteMetavar _ node@Constant{} = node
substituteMetavar _ node@Variable{} = node
substituteMetavar substitution (Lambda binder typ body) =
  Lambda binder typ (substituteMetavar substitution body)
substituteMetavar substitution (Apply function argument) =
  Apply
    (substituteMetavar substitution function)
    (substituteMetavar substitution argument)
substituteMetavar substitution@(Substitution expected parameters body) (Meta meta arguments)
  | meta == expected = foldr (uncurry substituteVar) body (zip parameters arguments)
  | otherwise = Meta meta (substituteMetavar substitution <$> arguments)

instance Show Ast where
  show (Constant constant typ) = constant <> ": " <> show typ
  show (Variable (Var variable)) = variable
  show (Lambda (Var binder) typ body) = "\\" <> binder <> ": " <> show typ <> ". " <> show body
  show (Apply left right) = "(" <> show left <> ") (" <> show right <> ")"
  show (Meta (Metavar metavariable) arguments) = metavariable <> show arguments

data Constraint = Constraint
  { constraintForall :: [(Var, Type)]
  , constraintLhs :: Ast
  , constraintRhs :: Ast
  }
  deriving (Eq)

substituteConstraint :: Substitution -> Constraint -> Constraint
substituteConstraint substitution (Constraint forall_ left right) =
  Constraint forall_ (substituteMetavar substitution left) (substituteMetavar substitution right)

instance Show Constraint where
  show (Constraint vars left right) =
    "forall "
      <> intercalate ", " (fmap (\(Var x, typ) -> x <> ": " <> show typ) vars)
      <> ". "
      <> show left
      <> " = "
      <> show right

data FlexRigid = FlexRigid
  { flexRigidForall :: [(Var, Type)]
  , flexRigidMetavariable :: Metavar
  , flexRigidArguments :: [Ast]
  , flexRigidRhs :: Ast
  }
  deriving (Show)

toFlexRigid :: Constraint -> Maybe FlexRigid
toFlexRigid (Constraint _ Meta{} Meta{}) = Nothing
toFlexRigid (Constraint forall_ (Meta meta arguments) rhs) = Just (FlexRigid forall_ meta arguments rhs)
toFlexRigid (Constraint forall_ rhs (Meta meta arguments)) = Just (FlexRigid forall_ meta arguments rhs)
toFlexRigid _ = Nothing

data Metavariables = Metavariables
  { metavariables :: [(Metavar, ([Type], Type))]
  , freshMetavariables :: Stream Metavar
  }
  deriving (Show, Eq)

someMetas :: Metavariables
someMetas = Metavariables [] freshMetavariables
 where
  freshMetavariables = fmap (\i -> Metavar ("M" <> show i)) (nats (0 :: Integer))
  nats i = Stream i (nats (i + 1))

fresh :: [(Var, Type)] -> Type -> Metavariables -> (Ast, Metavariables)
fresh parameters typ (Metavariables metavariables (Stream metavar freshMetavariables)) =
  (Meta metavar parameters', Metavariables ((metavar, metavarType) : metavariables) freshMetavariables)
 where
  parameters' = Variable . fst <$> parameters
  metavarType = (snd <$> parameters, typ)

data Substitution = Substitution Metavar [Var] Ast

instance Show Substitution where
  show (Substitution (Metavar meta) parameters body) =
    meta <> "[" <> intercalate "," (fmap (\(Var var) -> var) parameters) <> "] ↦ " <> show body

newtype Substitutions = Substitutions [Substitution]

emptySubstitutions :: Substitutions
emptySubstitutions = Substitutions []

addSubstitution :: Substitution -> Substitutions -> Substitutions
addSubstitution substitution (Substitutions substitutions) =
  Substitutions (substitution : substitutions)

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

withoutSubstitutions :: Problem -> Solution
withoutSubstitutions (Problem metas constraints) = Solution metas constraints emptySubstitutions

pickFlexRigid :: Solution -> Maybe FlexRigid
pickFlexRigid (Solution _ constraints _) = go constraints
 where
  go [] = Nothing
  go (constraint : rest) = case toFlexRigid constraint of
    Just flexRigid -> Just flexRigid
    Nothing -> go rest

splitProblems :: [Solution] -> ([Solution] -> [Solution], [(FlexRigid, Solution)])
splitProblems = go id id
 where
  go solved unsolved [] = (solved, unsolved [])
  go solved unsolved (problem : problems) = case pickFlexRigid problem of
    Just flexRigid -> go solved (((flexRigid, problem) :) . unsolved) problems
    Nothing -> go ((problem :) . solved) unsolved problems

data Decomposition
  = Failed
  | Nondecomposable Constraint
  | Decomposed [Constraint]
  deriving (Show)

-- >>> (t, u) = (Base "t", Base "u")
-- >>> (x, y) = (Var "x", Var "y")
-- >>> decompose (Constraint [(x, t)] (Variable x) (Variable x))
-- Decomposed []
-- >>> decompose (Constraint [(x, t), (y, u)] (Variable x) (Variable y))
-- Failed
-- >>> decompose (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Apply (Constant "A" t) (Constant "B" u)))
-- Decomposed [forall x: t, y: u. x = A: t,forall x: t, y: u. y = B: u]
-- >>> decompose (Constraint [] (Lambda x t (Variable x)) (Lambda y t (Variable y)))
-- Decomposed [forall x: t. x = x]
-- >>> decompose (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Variable x))
-- Failed
decompose :: Constraint -> Decomposition
decompose constraint@(Constraint forall_ left right) = case (left, right) of
  (Meta{}, _) -> Nondecomposable constraint
  (_, Meta{}) -> Nondecomposable constraint
  (Constant{}, Constant{}) | left == right -> Decomposed []
  (Variable{}, Variable{}) | left == right -> Decomposed []
  (Lambda leftBinder leftBinderType leftBody, Lambda rightBinder rightBinderType rightBody)
    | leftBinderType == rightBinderType ->
        Decomposed [Constraint forall' leftBody rightBody']
   where
    forall' = (leftBinder, leftBinderType) : forall_
    rightBody' = rename rightBinder leftBinder rightBody
  (Apply leftFunction leftArgument, Apply rightFunction rightArgument) ->
    Decomposed
      [ Constraint forall_ leftFunction rightFunction
      , Constraint forall_ leftArgument rightArgument
      ]
  _ -> Failed

decomposeConstraints :: [Constraint] -> Maybe [Constraint]
decomposeConstraints [] = Just []
decomposeConstraints (constraint : rest) = case decompose constraint of
  Failed -> Nothing
  Nondecomposable constraint' -> (constraint' :) <$> decomposeConstraints rest
  Decomposed constraints -> decomposeConstraints (constraints <> rest)

decomposeProblems :: [Solution] -> [Solution]
decomposeProblems problems = do
  Solution metas constraints substitutions <- problems
  constraints' <- maybeToList (decomposeConstraints constraints)
  return (Solution metas constraints' substitutions)

data MatchContext = MatchContext
  { matchMetavariables :: Metavariables
  , matchParameters :: [(Var, Type)]
  , matchForall :: [(Var, Type)]
  , matchRhs :: Ast
  , matchRhsType :: Type
  }
  deriving (Show)

-- >>> (x, y) = (Var "x", Var "y")
-- >>> (t, u, v) = (Base "t", Base "u", Base "v")
-- >>> metas = Metavariables [(Metavar "M", ([u], t))] (freshMetavariables someMetas)
-- >>> context = MatchContext someMetas [(y, v)]
-- >>> snd <$> imitate (context [] (Constant "B" t) t)
-- Just B: t
-- >>> snd <$> imitate (context [(x, t)] (Variable x) t)
-- Nothing
-- >>> snd <$> imitate (context [(x, u)] (Lambda x t (Variable x)) (Function t t))
-- Just \x: t. M0[x,y]
imitate :: MatchContext -> Maybe (Metavariables, Ast)
imitate (MatchContext metas parameters forall_ rhs rhsType) = case rhs of
  Constant constant typ -> Just (metas, Constant constant typ)
  Variable _ -> Nothing
  Lambda binder binderType _ -> do
    Function _ returnType <- Just rhsType
    let (body, metas') = fresh ((binder, binderType) : parameters) returnType metas
    Just (metas', Lambda binder binderType body)
  Apply function argument -> do
    functionType <- typeOf forall_ (metavariables metas) function
    argumentType <- typeOf forall_ (metavariables metas) argument
    let (function', metas') = fresh parameters functionType metas
    let (argument', metas'') = fresh parameters argumentType metas'
    Just (metas'', Apply function' argument')
  Meta{} -> error "impossible" -- TODO: remove this possibility

-- >>> (t, u, v) = (Base "t", Base "u", Base "v")
-- >>> parameters = [(Var "x", v)]
-- >>> y = Variable (Var "y")
-- >>> snd <$> reduce someMetas parameters t t y
-- [y]
-- >>> snd <$> reduce someMetas parameters t u y
-- []
-- >>> snd <$> reduce someMetas parameters t (Function u t) y
-- [(y) (M0[x])]
reduce :: Metavariables -> [(Var, Type)] -> Type -> Type -> Ast -> [(Metavariables, Ast)]
reduce metas parameters expectedType actualType term = reflexive <> function
 where
  reflexive
    | expectedType == actualType = [(metas, term)]
    | otherwise = []
  function
    | Function argumentType returnType <- actualType = do
        let (argument, metas') = fresh parameters argumentType metas
        reduce metas' parameters expectedType returnType (Apply term argument)
    | otherwise = []

-- >>> (t, u) = (Base "t", Base "u")
-- >>> x = (Var "x", Function t (Function u t))
-- >>> y = (Var "y", Function t u)
-- >>> z = (Var "z", t)
-- >>> snd <$> project (MatchContext someMetas [x, y, z] [] (Constant "A" t) t)
-- [((x) (M0[x,y,z])) (M1[x,y,z]),z]
project :: MatchContext -> [(Metavariables, Ast)]
project (MatchContext metas parameters _ _ rhsType) = do
  (parameter, parameterType) <- parameters
  reduce metas parameters rhsType parameterType (Variable parameter)

-- >>> t = Base "t"
-- >>> x = Var "x"
-- >>> (x, xType) = (Var "x", Function (Base "A") (Base "A"))
-- >>> snd <$> match (MatchContext someMetas [(x, Function t t)] [] (Constant "A" t) t)
-- [A: t,(x) (M0[x])]
match :: MatchContext -> [(Metavariables, Ast)]
match context = maybeToList (imitate context) <> project context

step :: FlexRigid -> Solution -> [Solution]
step (FlexRigid forall_ meta _ rhs) (Solution metas constraints substitutions) = do
  (parameterTypes, rhsType) <- maybeToList (lookup meta (metavariables metas))
  let parameters = zip someParameters parameterTypes

  (metas', body) <- match (MatchContext metas parameters forall_ rhs rhsType)
  let substitution = Substitution meta (fst <$> parameters) body
  let constraints' = substituteConstraint substitution <$> constraints
  return (Solution metas' constraints' (addSubstitution substitution substitutions))

-- >>> t = Base "t"
-- >>> left = Apply (Constant "a" (Function t t)) (Constant "b" t)
-- >>> right = Meta (Metavar "F") [Meta (Metavar "X") []]
-- >>> metas = Metavariables [(Metavar "F", ([t], t)), (Metavar "X", ([], t))] (freshMetavariables someMetas)
-- >>> constraint = Constraint [] left right
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ M1[x0] ↦ b: t, M0[x0] ↦ a: (t) -> t, F[x0] ↦ (M0[x0]) (M1[x0]) },{ X[] ↦ b: t, M1[x0] ↦ x0, M0[x0] ↦ a: (t) -> t, F[x0] ↦ (M0[x0]) (M1[x0]) },{ M1[] ↦ b: t, M0[] ↦ a: (t) -> t, X[] ↦ (M0[]) (M1[]), F[x0] ↦ x0 }]
solve :: [Problem] -> [Solution]
solve = go . fmap withoutSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
