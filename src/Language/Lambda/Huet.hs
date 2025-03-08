{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Lambda.Huet where

import Data.List (intercalate)
import Data.Maybe (maybeToList)

data Stream a = Stream a (Stream a) deriving (Eq, Functor)

instance Show (Stream a) where
  show _ = "Stream"

data Type = Base String | Function Type Type deriving (Eq)

newtype Var = Var String deriving (Eq, Show)
newtype Metavar = Metavar String deriving (Eq, Show)

data Ast
  = Constant String Type
  | Variable Var
  | Lambda Var Type Ast
  | Apply Ast Ast
  | Meta Metavar [Ast]
  deriving (Eq)

data Constraint = Constraint
  { constraintForall :: [(Var, Type)]
  , constraintLhs :: Ast
  , constraintRhs :: Ast
  }
  deriving (Eq)

data FlexRigid = FlexRigid
  { flexRigidForall :: [(Var, Type)]
  , flexRigidMetavariable :: Metavar
  , flexRigidArguments :: [Ast]
  , flexRigidRhs :: Ast
  }
  deriving (Show)

data Metavariables = Metavariables
  { metavariables :: [(Metavar, ([Type], Type))]
  , freshMetavariables :: Stream Metavar
  }
  deriving (Show, Eq)

data Problem = Problem
  { problemMetavariables :: Metavariables
  , problemConstraints :: [Constraint]
  }
  deriving (Show)

data Substitution = Substitution Metavar [Var] Ast

newtype Substitutions = Substitutions [Substitution]

data Solution = Solution
  { solutionMetavariables :: Metavariables
  , solutionConstraints :: [Constraint]
  , solutionSubstitutions :: Substitutions
  }
  deriving (Show)

instance Show Type where
  show (Base typ) = typ
  show (Function from to) = "(" <> show from <> ") -> " <> show to

instance Show Ast where
  show (Constant constant typ) = constant <> ": " <> show typ
  show (Variable (Var variable)) = variable
  show (Lambda (Var binder) typ body) = "\\" <> binder <> ": " <> show typ <> ". " <> show body
  show (Apply left right) = "(" <> show left <> ") (" <> show right <> ")"
  show (Meta (Metavar metavariable) arguments) = metavariable <> show arguments

instance Show Constraint where
  show (Constraint vars left right) =
    "forall "
      <> intercalate ", " (fmap (\(Var x, typ) -> x <> ": " <> show typ) vars)
      <> ". "
      <> show left
      <> " = "
      <> show right

instance Show Substitution where
  show (Substitution (Metavar meta) parameters body) =
    meta <> "[" <> intercalate "," (fmap (\(Var var) -> var) parameters) <> "] ↦ " <> show body

instance Show Substitutions where
  show (Substitutions []) = "{ }"
  show (Substitutions substitutions) = "{ " <> intercalate ", " (show <$> substitutions) <> " }"

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

substituteConstraint :: Substitution -> Constraint -> Constraint
substituteConstraint substitution (Constraint forall_ left right) =
  Constraint forall_ (substituteMetavar substitution left) (substituteMetavar substitution right)

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

someMetas :: Metavariables
someMetas = Metavariables [] freshMetavariables
 where
  freshMetavariables = fmap (\i -> Metavar ("M" <> show i)) (nats (0 :: Integer))
  nats i = Stream i (nats (i + 1))

toFlexRigid :: Constraint -> Maybe FlexRigid
toFlexRigid (Constraint _ Meta{} Meta{}) = Nothing
toFlexRigid (Constraint forall_ (Meta meta arguments) rhs) = Just (FlexRigid forall_ meta arguments rhs)
toFlexRigid (Constraint forall_ rhs (Meta meta arguments)) = Just (FlexRigid forall_ meta arguments rhs)
toFlexRigid _ = Nothing

someParameters :: [Var]
someParameters = fmap (\n -> Var ("x" <> show n)) [0 :: Integer ..]

makeParameters :: FlexRigid -> [Var]
makeParameters FlexRigid{flexRigidArguments} = take (length flexRigidArguments) someParameters

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

fresh :: [(Var, Type)] -> Type -> Metavariables -> (Ast, Metavariables)
fresh parameters typ (Metavariables metavariables (Stream metavar freshMetavariables)) =
  (Meta metavar parameters', Metavariables ((metavar, metavarType) : metavariables) freshMetavariables)
 where
  parameters' = Variable . fst <$> parameters
  metavarType = (snd <$> parameters, typ)

emptySubstitutions :: Substitutions
emptySubstitutions = Substitutions []

addSubstitution :: Substitution -> Substitutions -> Substitutions
addSubstitution substitution (Substitutions substitutions) =
  Substitutions (substitution : substitutions)

data Decomposition
  = Failed
  | Nondecomposable Constraint
  | Decomposed [Constraint]
  deriving (Show)

-- >>> (x, _X) = (Var "x", Base "X")
-- >>> (y, _Y) = (Var "y", Base "Y")
-- >>> decompose (Constraint [(x, _X)] (Variable x) (Variable x))
-- Decomposed []
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Variable x) (Variable y))
-- Failed
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Apply (Constant "A" (Function (Base "B") (Base "B"))) (Constant "B" (Base "B"))))
-- Decomposed [forall x: X, y: Y. x = A: (B) -> B,forall x: X, y: Y. y = B: B]
-- >>> decompose (Constraint [] (Lambda x _X (Variable x)) (Lambda y _X (Variable y)))
-- Decomposed [forall x: X. x = x]
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Variable x))
-- Failed
decompose :: Constraint -> Decomposition
decompose constraint@(Constraint _ Meta{} _) = Nondecomposable constraint
decompose constraint@(Constraint _ _ Meta{}) = Nondecomposable constraint
decompose (Constraint _ left@Constant{} right@Constant{}) | left == right = Decomposed []
decompose (Constraint _ (Variable left) (Variable right)) | left == right = Decomposed []
decompose (Constraint context (Lambda leftBinder typ left) (Lambda rightBinder _ right)) =
  Decomposed [Constraint ((leftBinder, typ) : context) left (rename rightBinder leftBinder right)]
decompose (Constraint context (Apply leftF leftA) (Apply rightF rightA)) =
  Decomposed [Constraint context leftF rightF, Constraint context leftA rightA]
decompose _ = Failed

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

-- >>> (x, y) = (Var "x", Var "y")
-- >>> metas = Metavariables [(Metavar "M", ([Base "Y"], Base "B"))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (FlexRigid [] (Metavar "M") (error "params") (Constant "B" (Base "B"))) [y]
-- Just B: B
-- >>> metas = Metavariables [(Metavar "M", ([Base "Y"], Base "X"))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (FlexRigid [(x, Base "X")] (Metavar "M") (error "params") (Variable x)) [y]
-- Nothing
-- >>> metas = Metavariables [(Metavar "M", ([Base "Y"], Function (Base "A") (Base "A")))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (FlexRigid [(x, Base "A")] (Metavar "M") (error "params") (Lambda x (Base "A") (Variable x))) [y]
-- Just \x: A. M0[x,y]
imitate :: Metavariables -> FlexRigid -> [Var] -> Maybe (Metavariables, Ast)
imitate metas (FlexRigid _ _ _ (Constant constant typ)) _ = Just (metas, Constant constant typ)
imitate _ (FlexRigid _ _ _ Variable{}) _ = Nothing
imitate metas (FlexRigid _ meta _ (Lambda binder binderType _)) parameters = do
  (parameterTypes, Function _ returnType) <- lookup meta (metavariables metas)
  let typedParameters = zip parameters parameterTypes
  let (body', metas') = fresh ((binder, binderType) : typedParameters) returnType metas
  Just (metas', Lambda binder binderType body')
imitate metas (FlexRigid context meta _ (Apply function argument)) parameters = do
  (parameterTypes, _) <- lookup meta (metavariables metas)
  let typedParameters = zip parameters parameterTypes
  functionType <- typeOf context (metavariables metas) function
  argumentType <- typeOf context (metavariables metas) argument
  let (left, metas') = fresh typedParameters functionType metas
  let (right, metas'') = fresh typedParameters argumentType metas'
  Just (metas'', Apply left right)
imitate _ (FlexRigid _ _ _ Meta{}) _ = error "impossible" -- TODO: remove this possibility

-- >>> parameters = [(Var "x", Base "X")]
-- >>> snd <$> reduce parameters someMetas (Base "A") (Base "A") (Constant "B" (Base "B"))
-- [B: B]
-- >>> snd <$> reduce parameters someMetas (Base "A") (Base "B") (Constant "B" (Base "B"))
-- []
-- >>> snd <$> reduce parameters someMetas (Base "A") (Function (Base "B") (Base "A")) (Constant "B" (Base "B"))
-- [(B: B) (M0[x])]
reduce :: [(Var, Type)] -> Metavariables -> Type -> Type -> Ast -> [(Metavariables, Ast)]
reduce parameters metas expectedType actualType term = reflexive <> function
 where
  reflexive
    | expectedType == actualType = [(metas, term)]
    | otherwise = []
  function
    | Function argumentType returnType <- actualType = do
        let (argument, metas') = fresh parameters argumentType metas
        reduce parameters metas' expectedType returnType (Apply term argument)
    | otherwise = []

-- >>> (x, xType) = (Var "x", Function (Base "A") (Function (Base "A") (Base "A")))
-- >>> (y, yType) = (Var "y", (Function (Base "A") (Base "B")))
-- >>> (z, zType) = (Var "z", Base "A")
-- >>> metas = Metavariables [(Metavar "M", ([xType, yType, zType], Base "A"))] (freshMetavariables someMetas)
-- >>> snd <$> project metas (FlexRigid [] (Metavar "M") (error "args") (Constant "A" (Base "A"))) [x, y, z]
-- [((x) (M0[x,y,z])) (M1[x,y,z]),z]
project :: Metavariables -> FlexRigid -> [Var] -> [(Metavariables, Ast)]
project metas (FlexRigid _ meta _ _) parameters = do
  (parameterTypes, expectedType) <- maybeToList (lookup meta (metavariables metas))
  let typedParameters = zip parameters parameterTypes
  (var, actualType) <- typedParameters
  reduce typedParameters metas expectedType actualType (Variable var)

-- >>> (x, xType) = (Var "x", Function (Base "A") (Base "A"))
-- >>> metas = Metavariables [(Metavar "M", ([xType], Base "A"))] (freshMetavariables someMetas)
-- >>> snd <$> match metas (FlexRigid [] (Metavar "M") (error "args") (Constant "A" (Base "A"))) [x]
-- [A: A,(x) (M0[x])]
match :: Metavariables -> FlexRigid -> [Var] -> [(Metavariables, Ast)]
match metas constraint parameters =
  maybeToList (imitate metas constraint parameters)
    <> project metas constraint parameters

step :: FlexRigid -> Solution -> [Solution]
step flexRigid (Solution metas constraints substitutions) = do
  let meta = flexRigidMetavariable flexRigid
  let parameters = makeParameters flexRigid
  (metas', body) <- match metas flexRigid parameters
  let substitution = Substitution meta parameters body
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
