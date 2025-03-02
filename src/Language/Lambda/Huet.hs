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
  = Constant String
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

data Solution = Solution
  { solutionMetavariables :: Metavariables
  , solutionConstraints :: [Constraint]
  , solutionSubstitutions :: [(Metavar, ([Var], Ast))]
  }
  deriving (Show)

instance Show Type where
  show (Base typ) = typ
  show (Function from to) = "(" <> show from <> ") -> " <> show to

instance Show Ast where
  show (Constant constant) = constant
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

-- Naive rename
--
-- >>> (x, y, z) = (Var "x", Var "y", Var "z")
-- >>> rename x y (Apply (Variable x) (Variable z))
-- (y) (z)
-- >>> rename x y (Lambda x (Variable x))
-- \x. x
rename :: Var -> Var -> Ast -> Ast
rename from to (Variable variable)
  | variable == from = Variable to
  | otherwise = Variable variable
rename _ _ (Constant constant) = Constant constant
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

substituteMetavar :: Metavar -> [Var] -> Ast -> Ast -> Ast
substituteMetavar _ _ _ node@Constant{} = node
substituteMetavar _ _ _ node@Variable{} = node
substituteMetavar expected parameters substitution (Lambda binder typ body) =
  Lambda binder typ (substituteMetavar expected parameters substitution body)
substituteMetavar expected parameters substitution (Apply function argument) =
  Apply
    (substituteMetavar expected parameters substitution function)
    (substituteMetavar expected parameters substitution argument)
substituteMetavar expected parameters substitution (Meta meta arguments)
  | meta == expected = foldr (uncurry substituteVar) substitution (zip parameters arguments)
  | otherwise = Meta meta (substituteMetavar expected parameters substitution <$> arguments)

substituteConstraint :: Metavar -> [Var] -> Ast -> Constraint -> Constraint
substituteConstraint meta parameters substitution (Constraint forall_ left right) =
  Constraint forall_ (f left) (f right)
 where
  f = substituteMetavar meta parameters substitution

-- >>> x = Var "x"
-- >>> typeOf [(x, Base "X")] [] (Lambda x (Base "Y") (Variable x))
-- Just (Y) -> Y
-- >>> typeOf [] [] (Apply (Lambda x (Base "A") (Variable x)) (Constant "A"))
-- Just A
typeOf :: [(Var, Type)] -> [(Metavar, ([Type], Type))] -> Ast -> Maybe Type
typeOf _ _ (Constant x) = Just (Base x) -- whatever
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
someParameters = fmap (\n -> Var ("x" <> show n)) [0 ..]

makeParameters :: FlexRigid -> [Var]
makeParameters FlexRigid{flexRigidArguments} = take (length flexRigidArguments) someParameters

withoutSubstitutions :: Problem -> Solution
withoutSubstitutions (Problem metas constraints) = Solution metas constraints []

pickFlexRigid :: Solution -> Maybe FlexRigid
pickFlexRigid (Solution _ constraints _) = go constraints
 where
  go [] = Nothing
  go (constraint : constraints) = case toFlexRigid constraint of
    Just flexRigid -> Just flexRigid
    Nothing -> go constraints

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
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Apply (Constant "A") (Constant "B")))
-- Decomposed [forall x: X, y: Y. x = A,forall x: X, y: Y. y = B]
-- >>> decompose (Constraint [] (Lambda x _X (Variable x)) (Lambda y _X (Variable y)))
-- Decomposed [forall x: X. x = x]
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Variable x))
-- Failed
decompose :: Constraint -> Decomposition
decompose constraint@(Constraint _ Meta{} _) = Nondecomposable constraint
decompose constraint@(Constraint _ _ Meta{}) = Nondecomposable constraint
decompose (Constraint _ (Constant left) (Constant right)) | left == right = Decomposed []
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
-- >>> snd <$> imitate metas (FlexRigid [] (Metavar "M") (error "params") (Constant "B")) [y]
-- Just B
-- >>> metas = Metavariables [(Metavar "M", ([Base "Y"], Base "X"))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (FlexRigid [(x, Base "X")] (Metavar "M") (error "params") (Variable x)) [y]
-- Nothing
-- >>> metas = Metavariables [(Metavar "M", ([Base "Y"], Function (Base "A") (Base "A")))] (freshMetavariables someMetas)
-- >>> snd <$> imitate metas (FlexRigid [(x, Base "A")] (Metavar "M") (error "params") (Lambda x (Base "A") (Variable x))) [y]
-- Just \x: A. M0[x,y]
imitate :: Metavariables -> FlexRigid -> [Var] -> Maybe (Metavariables, Ast)
imitate metas (FlexRigid _ _ _ (Constant constant)) _ = Just (metas, Constant constant)
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
-- >>> snd <$> reduce parameters someMetas (Base "A") (Base "A") (Constant "B")
-- [B]
-- >>> snd <$> reduce parameters someMetas (Base "A") (Base "B") (Constant "B")
-- []
-- >>> snd <$> reduce parameters someMetas (Base "A") (Function (Base "B") (Base "A")) (Constant "B")
-- [(B) (M0[x])]
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
-- >>> snd <$> project metas (FlexRigid [] (Metavar "M") (error "args") (Constant "A")) [x, y, z]
-- [((x) (M0[x,y,z])) (M1[x,y,z]),z]
project :: Metavariables -> FlexRigid -> [Var] -> [(Metavariables, Ast)]
project metas (FlexRigid _ meta _ _) parameters = do
  (parameterTypes, expectedType) <- maybeToList (lookup meta (metavariables metas))
  let typedParameters = zip parameters parameterTypes
  (var, actualType) <- typedParameters
  reduce typedParameters metas expectedType actualType (Variable var)

-- >>> x = (Var "x", Function (Base "A") (Base "A"))
-- >>> snd <$> match (FlexRigid [] [x] someMetas (Constant "A"))
-- [A,(x) (M0[x])]
match :: Metavariables -> FlexRigid -> [Var] -> [(Metavariables, Ast)]
match metas constraint parameters = maybeToList (imitate metas constraint parameters) <> project metas constraint parameters

step :: FlexRigid -> Solution -> [Solution]
step flexRigid (Solution metas constraints substitutions) = do
  let meta = flexRigidMetavariable flexRigid
  let parameters = makeParameters flexRigid
  (metas', substitution) <- match metas flexRigid parameters
  let constraints' = substituteConstraint meta parameters substitution <$> constraints
  return (Solution metas' constraints' ((meta, (parameters, substitution)) : substitutions))

-- >>> (f, x) = (Var "f", Var "x")
-- >>> t = Base "t"
-- >>> left = Apply (Variable f) (Variable x)
-- >>> right = Meta (Metavar "F") [Meta (Metavar "X") [], Variable f, Variable x]
-- >>> metas = Metavariables [(Metavar "F", ([t, Function t t, t], t)), (Metavar "X", ([], t))] (freshMetavariables someMetas)
-- >>> constraint = Constraint [(f, (Function t t)), (x, t)] left right
-- >>> solve [Problem metas [constraint]]
-- [Solution {solutionMetavariables = Metavariables {metavariables = [(Metavar "M0",([t,(t) -> t,t],t)),(Metavar "F",([t,(t) -> t,t],t)),(Metavar "X",([],t))], freshMetavariables = Stream}, solutionConstraints = [], solutionSubstitutions = [(Metavar "M0",([Var "x0",Var "x1",Var "x2"],x2)),(Metavar "F",([Var "x0",Var "x1",Var "x2"],(x1) (M0[x0,x1,x2])))]},Solution {solutionMetavariables = Metavariables {metavariables = [(Metavar "M1",([t,(t) -> t,t],t)),(Metavar "M0",([t,(t) -> t,t],(t) -> t)),(Metavar "F",([t,(t) -> t,t],t)),(Metavar "X",([],t))], freshMetavariables = Stream}, solutionConstraints = [], solutionSubstitutions = [(Metavar "M1",([Var "x0",Var "x1",Var "x2"],x2)),(Metavar "M0",([Var "x0",Var "x1",Var "x2"],x1)),(Metavar "F",([Var "x0",Var "x1",Var "x2"],(M0[x0,x1,x2]) (M1[x0,x1,x2])))]}]
solve :: [Problem] -> [Solution]
solve = go . fmap withoutSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
