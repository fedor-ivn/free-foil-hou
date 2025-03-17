{-# LANGUAGE DeriveFunctor #-}

module Language.Lambda.Huet where

import Control.Applicative ((<|>))
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

isFlexible :: Ast -> Bool
isFlexible Constant{} = False
isFlexible Variable{} = False
isFlexible Lambda{} = False
isFlexible (Apply function _) = isFlexible function
isFlexible Meta{} = True

-- >>> (x, f, t) = (Var "x", Var "f", Base "t")
-- >>> eval (Lambda x t (Lambda f (Function t t) (Apply (Variable f) (Variable x))))
-- \x: t. \f: (t) -> t. (f) (x)
-- >>> eval (Apply (Lambda x t (Lambda f (Function t t) (Apply (Variable f) (Variable x)))) (Constant "A" t))
-- \f: (t) -> t. (f) (A: t)
-- >>> eval (Apply (Apply (Lambda x t (Lambda f (Function t t) (Apply (Variable f) (Variable x)))) (Constant "A" t)) (Lambda x t (Constant "B" t)))
-- B: t
eval :: Ast -> Ast
eval node = case node of
  Constant _ _ -> node
  Variable _ -> node
  Lambda binder binderType body -> Lambda binder binderType (eval body)
  Apply function argument ->
    case eval function of
      Lambda binder _typ body -> eval (substituteVar binder argument body)
      function' -> Apply function' argument
  Meta _ _ -> node

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

evalConstraint :: Constraint -> Constraint
evalConstraint (Constraint forall_ left right) = Constraint forall_ (eval left) (eval right)

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

fresh :: [(Var, Type)] -> Type -> Metavariables -> (Metavariables, Ast)
fresh parameters typ (Metavariables metavariables (Stream metavar freshMetavariables)) =
  (Metavariables ((metavar, metavarType) : metavariables) freshMetavariables, Meta metavar parameters')
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

-- >>> (t, u) = (Base "t", Base "u")
-- >>> (x, y, _M) = (Var "x", Var "y", Metavar "M")
-- >>> decompose (Constraint [(x, t)] (Variable x) (Variable x))
-- Decomposed []
-- >>> decompose (Constraint [(x, t), (y, u)] (Variable x) (Variable y))
-- Failed
-- >>> decompose (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Apply (Constant "A" t) (Constant "B" u)))
-- Decomposed [forall x: t, y: u. y = B: u,forall x: t, y: u. x = A: t]
-- >>> decompose (Constraint [] (Lambda x t (Variable x)) (Lambda y t (Variable y)))
-- Decomposed [forall x: t. x = x]
-- >>> decompose (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Variable x))
-- Failed
-- >>> decompose (Constraint [(x, t), (y, t)] (Apply (Meta _M []) (Variable x)) (Variable y))
-- Failed
-- >>> decompose (Constraint [(x, t), (y, t)] (Meta _M []) (Variable y))
-- Flexible
decompose :: Constraint -> Decomposition
decompose (Constraint forall_ left right) = case (left, right) of
  (Meta{}, _) -> Flexible
  (_, Meta{}) -> Flexible
  (Constant{}, Constant{}) | left == right -> Decomposed []
  (Variable{}, Variable{}) | left == right -> Decomposed []
  (Lambda leftBinder leftBinderType leftBody, Lambda rightBinder rightBinderType rightBody)
    | leftBinderType == rightBinderType ->
        Decomposed [Constraint forall' leftBody rightBody']
   where
    forall' = (leftBinder, leftBinderType) : forall_
    rightBody' = rename rightBinder leftBinder rightBody
  (Apply leftFunction leftArgument, Apply rightFunction rightArgument) ->
    Decomposed [Constraint forall_ leftArgument rightArgument, Constraint forall_ leftFunction rightFunction]
  _ -> Failed

-- >>> (t, u) = (Base "t", Base "u")
-- >>> (x, y, _M) = (Var "x", Var "y", Metavar "M")
-- >>> decomposeRigidRigid (Constraint [(x, t)] (Variable x) (Variable x))
-- Decomposed []
-- >>> decomposeRigidRigid (Constraint [(x, t), (y, u)] (Variable x) (Variable y))
-- Failed
-- >>> decomposeRigidRigid (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Apply (Constant "A" t) (Constant "B" u)))
-- Decomposed [forall x: t, y: u. y = B: u,forall x: t, y: u. x = A: t]
-- >>> decomposeRigidRigid (Constraint [] (Lambda x t (Variable x)) (Lambda y t (Variable y)))
-- Decomposed [forall x: t. x = x]
-- >>> decomposeRigidRigid (Constraint [(x, t), (y, u)] (Apply (Variable x) (Variable y)) (Variable x))
-- Failed
-- >>> decomposeRigidRigid (Constraint [(x, t), (y, t)] (Apply (Meta _M []) (Variable x)) (Variable y))
-- Flexible
decomposeRigidRigid :: Constraint -> Decomposition
decomposeRigidRigid constraint@(Constraint _ left right)
  | isFlexible left = Flexible
  | isFlexible right = Flexible
  | otherwise = decompose constraint

decomposeAll :: (Constraint -> Decomposition) -> [Constraint] -> Maybe [Constraint]
decomposeAll _ [] = Just []
decomposeAll f (constraint : rest) = case f constraint of
  Failed -> Nothing
  Flexible -> (constraint :) <$> decomposeAll f rest
  Decomposed constraints -> decomposeAll f (constraints <> rest)

decomposeProblems :: [Solution] -> [Solution]
decomposeProblems problems = do
  Solution metas constraints substitutions <- problems
  constraints' <- maybeToList (decomposeAll decomposeRigidRigid (evalConstraint <$> constraints))
  return (Solution metas constraints' substitutions)

data MatchContext = MatchContext
  { matchMetavariables :: Metavariables
  , matchParameters :: [(Var, Type)]
  , matchForall :: [(Var, Type)]
  , matchRhs :: Ast
  , matchRhsType :: Type
  }
  deriving (Show)

-- >>> (x, y, _M) = (Var "x", Var "y", Metavar "M")
-- >>> (t, u, v) = (Base "t", Base "u", Base "v")
-- >>> metas = Metavariables [(Metavar "M", ([u], t))] (freshMetavariables someMetas)
-- >>> context = MatchContext someMetas [(y, v)]
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Constant "B" t))
-- Just M[x0] ↦ B: t
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Constant "B" t) (Meta _M [Variable x]))
-- Just M[x0] ↦ B: t
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Variable x))
-- Nothing
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Lambda x t (Variable x)))
-- Just M[x0] ↦ \x: t. M0[x,x0]
-- >>> snd <$> imitate metas (Constraint [(x, t)] (Meta _M [Variable x]) (Apply (Constant "B" (Function t t)) (Variable x)))
-- Just M[x0] ↦ (B: (t) -> t) (M0[x0])
imitate :: Metavariables -> Constraint -> Maybe (Metavariables, Substitution)
imitate metas (Constraint forall_ left right) = case (left, right) of
  (Meta meta _, rhs) -> go meta rhs
  (rhs, Meta meta _) -> go meta rhs
  _ -> Nothing
 where
  go meta rhs = do
    (parameterTypes, _) <- lookup meta (metavariables metas)
    let parameters = zip someParameters parameterTypes
    (metas', imitiation) <- go' metas parameters rhs
    return (metas', Substitution meta (fst <$> parameters) imitiation)

  go' metas parameters rhs = case rhs of
    Variable{} -> Nothing
    Constant{} -> Just (metas, rhs)
    Lambda binder binderType body -> do
      bodyType <- typeOf ((binder, binderType) : forall_) (metavariables metas) body
      let (metas', body') = fresh ((binder, binderType) : parameters) bodyType metas
      return (metas', Lambda binder binderType body')
    Apply function argument -> do
      (metas', function') <- go' metas parameters function
      argumentType <- typeOf forall_ (metavariables metas) argument
      let (metas'', argument') = fresh parameters argumentType metas'
      return (metas'', Apply function' argument')
    Meta{} -> Nothing

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
        let (metas', argument) = fresh parameters argumentType metas
        reduce metas' parameters expectedType returnType (Apply term argument)
    | otherwise = []

-- >>> (t, u) = (Base "t", Base "u")
-- >>> _M = Metavar "M"
-- >>> (xType, yType, zType) = (Function t (Function u t), Function t u, t)
-- >>> metas = Metavariables [(_M, ([xType, yType, zType], t))] (freshMetavariables someMetas)
-- >>> snd <$> project metas (Constraint [] (Meta _M [Constant "X" xType, Constant "Y" yType, Constant "Z" zType]) (Constant "A" t))
-- [M[x0,x1,x2] ↦ ((x0) (M0[x0,x1,x2])) (M1[x0,x1,x2]),M[x0,x1,x2] ↦ x2]
project :: Metavariables -> Constraint -> [(Metavariables, Substitution)]
project metas (Constraint _ left right) = case (left, right) of
  (Meta{}, Meta{}) -> []
  (Meta meta _, _) -> go meta
  (_, Meta meta _) -> go meta
  (_, _) -> []
 where
  go meta = do
    (parameterTypes, rhsType) <- maybeToList (lookup meta (metavariables metas))
    let parameters = zip someParameters parameterTypes
    (parameter, parameterType) <- parameters
    (metas', projection) <- reduce metas parameters rhsType parameterType (Variable parameter)
    return (metas', Substitution meta (fst <$> parameters) projection)

-- >>> (_M, _X, t) = (Metavar "M", Metavar "X", Base "t")
-- >>> metas = Metavariables [(_M, ([], Function t t))] (freshMetavariables someMetas)
-- >>> snd <$> introduce metas (Constraint [] (Apply (Meta _M []) (Meta _X [])) (Apply (Constant "a" (Function t t)) (Constant "b" t)))
-- Just M[] ↦ \x0: t. M0[x0]
introduce :: Metavariables -> Constraint -> Maybe (Metavariables, Substitution)
introduce metas (Constraint _ left right) = go left <|> go right
 where
  go Variable{} = Nothing
  go Constant{} = Nothing
  go Lambda{} = Nothing
  go (Apply (Meta meta _) _) = do
    (parameterTypes, Function binderType bodyType) <- lookup meta (metavariables metas)
    let (binder : someParameters') = someParameters
    let parameters = zip someParameters' parameterTypes
    let (metas', substitution) = fresh ((binder, binderType) : parameters) bodyType metas
    return (metas', Substitution meta (fst <$> parameters) (Lambda binder binderType substitution))
  go (Apply function _) = go function
  go Meta{} = Nothing

-- >>> t = Base "t"
-- >>> left = Apply (Constant "a" (Function t t)) (Constant "b" t)
-- >>> right = Apply (Meta (Metavar "F") []) (Meta (Metavar "X") [])
-- >>> metas = Metavariables [(Metavar "F", ([], Function t t)), (Metavar "X", ([], t))] (freshMetavariables someMetas)
-- >>> constraint = Constraint [] left right
-- >>> flexRigid = pickFlexRigid (Solution metas [constraint] emptySubstitutions)
-- >>> uncurry step =<< maybeToList flexRigid
-- [Solution {solutionMetavariables = Metavariables {metavariables = [(Metavar "F",([],(t) -> t)),(Metavar "X",([],t))], freshMetavariables = Stream}, solutionConstraints = [forall . b: t = X[],forall . a: (t) -> t = F[]], solutionSubstitutions = { }},Solution {solutionMetavariables = Metavariables {metavariables = [(Metavar "M0",([t],t)),(Metavar "F",([],(t) -> t)),(Metavar "X",([],t))], freshMetavariables = Stream}, solutionConstraints = [forall . (a: (t) -> t) (b: t) = (\x0: t. M0[x0]) (X[])], solutionSubstitutions = { F[] ↦ \x0: t. M0[x0] }}]
step :: Constraint -> Solution -> [Solution]
step constraint (Solution metas constraints substitutions) =
  maybeToList decomposed <> solved
 where
  imitations = imitate metas constraint
  projections = project metas constraint
  introductions = introduce metas constraint

  solved = do
    (metas', substitution) <- maybeToList imitations <> projections <> maybeToList introductions
    let constraints' = substituteConstraint substitution <$> (constraint : constraints)
    return (Solution metas' constraints' (addSubstitution substitution substitutions))

  -- `F[] X[] = a b` does not decompose semantically, but it decomposes
  -- structurally. Try structural decomposition once we dealt with the semantics
  decomposed = do
    _ <- introductions
    decomposition <- decomposeAll decompose [constraint]
    return (Solution metas (decomposition <> constraints) substitutions)

-- >>> t = Base "t"
-- >>> left = Apply (Constant "a" (Function t t)) (Constant "b" t)
-- >>> right = Apply (Meta (Metavar "F") []) (Meta (Metavar "X") [])
-- >>> metas = Metavariables [(Metavar "F", ([], Function t t)), (Metavar "X", ([], t))] (freshMetavariables someMetas)
-- >>> constraint = Constraint [] left right
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ M1[x0] ↦ b: t, M0[x0] ↦ (a: (t) -> t) (M1[x0]), F[] ↦ \x0: t. M0[x0] },{ F[] ↦ a: (t) -> t, X[] ↦ b: t },{ M1[] ↦ b: t, X[] ↦ (a: (t) -> t) (M1[]), M0[x0] ↦ x0, F[] ↦ \x0: t. M0[x0] },{ X[] ↦ b: t, M1[x0] ↦ x0, M0[x0] ↦ (a: (t) -> t) (M1[x0]), F[] ↦ \x0: t. M0[x0] }]
--
-- >>> left = Apply (Apply (Constant "a" (Function t (Function t t))) (Constant "b" t)) (Constant "c" t)
-- >>> right = Apply (Meta (Metavar "F") []) (Meta (Metavar "X") [])
-- >>> metas = Metavariables [(Metavar "F", ([], Function t (Function t t))), (Metavar "X", ([], t)), (Metavar "Y", ([], t))] (freshMetavariables someMetas)
-- >>> constraint = Constraint [] left right
-- >>> solutionSubstitutions <$> solve [Problem metas [constraint]]
-- [{ M0[] ↦ b: t, F[] ↦ (a: (t) -> (t) -> t) (M0[]), X[] ↦ c: t },{ M1[x0] ↦ b: t, M2[x0] ↦ c: t, M0[x0] ↦ ((a: (t) -> (t) -> t) (M1[x0])) (M2[x0]), F[] ↦ \x0: t. M0[x0] },{ M1[x0] ↦ b: t, X[] ↦ c: t, M2[x0] ↦ x0, M0[x0] ↦ ((a: (t) -> (t) -> t) (M1[x0])) (M2[x0]), F[] ↦ \x0: t. M0[x0] },{ X[] ↦ b: t, M1[x0] ↦ x0, M2[x0] ↦ c: t, M0[x0] ↦ ((a: (t) -> (t) -> t) (M1[x0])) (M2[x0]), F[] ↦ \x0: t. M0[x0] }]
--
-- >>> (_F, _X) = (Metavar "F", Metavar "X")
-- >>> a = Constant "a" (Function t t)
-- >>> b = Constant "b" t
-- >>> left = Meta _F [Meta _X [], b]
-- >>> right = Apply a b
-- >>> metas = Metavariables [(_F, ([Function t (Function t t), t], t)), (_X, ([], Function t (Function t t)))] (freshMetavariables someMetas)
-- >>> take 5 $ solutionSubstitutions <$> solve [Problem metas [Constraint [] left right]]
-- [{ M0[x0,x1] ↦ x1, F[x0,x1] ↦ (a: (t) -> t) (M0[x0,x1]) },{ M0[x0,x1] ↦ b: t, F[x0,x1] ↦ (a: (t) -> t) (M0[x0,x1]) },{ M4[x0,x1] ↦ b: t, M3[x0,x1] ↦ (a: (t) -> t) (M4[x0,x1]), M2[x1] ↦ \x0: t. M3[x0,x1], X[] ↦ \x0: t. M2[x0], F[x0,x1] ↦ ((x0) (M0[x0,x1])) (M1[x0,x1]) },{ M2[x0] ↦ a: (t) -> t, M1[x0,x1] ↦ b: t, X[] ↦ \x0: t. M2[x0], F[x0,x1] ↦ ((x0) (M0[x0,x1])) (M1[x0,x1]) },{ M2[x0] ↦ a: (t) -> t, M1[x0,x1] ↦ x1, X[] ↦ \x0: t. M2[x0], F[x0,x1] ↦ ((x0) (M0[x0,x1])) (M1[x0,x1]) }]
solve :: [Problem] -> [Solution]
solve = go . fmap withoutSubstitutions
 where
  go [] = []
  go problems = do
    let problems' = decomposeProblems problems
    let (solutions, unsolved) = splitProblems problems'
    solutions (go (uncurry step =<< unsolved))
