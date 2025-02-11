{-# LANGUAGE DeriveFunctor #-}

module Language.Lambda.Huet where

import Data.List (intercalate)
import Data.Maybe (maybeToList)

data Type = Base String | Function Type Type deriving (Eq)

instance Show Type where
  show (Base typ) = typ
  show (Function from to) = "(" <> show from <> ") -> " <> show to

newtype Var = Var String deriving (Eq, Show)
newtype Metavar = Metavar String deriving (Eq, Show)

data Ast
  = Constant String
  | Variable Var
  | Lambda Var Type Ast
  | Apply Ast Ast
  | Meta Metavar [Ast]
  deriving (Eq)

instance Show Ast where
  show (Constant constant) = constant
  show (Variable (Var variable)) = variable
  show (Lambda (Var binder) typ body) = "\\" <> binder <> ": " <> show typ <> ". " <> show body
  show (Apply left right) = "(" <> show left <> ") (" <> show right <> ")"
  show (Meta (Metavar metavariable) arguments) = metavariable <> show arguments

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

data Constraint = Constraint [(Var, Type)] Ast Ast deriving (Eq)

instance Show Constraint where
  show (Constraint vars left right) =
    "forall "
      <> intercalate ", " (fmap (\(Var x, typ) -> x <> ": " <> show typ) vars)
      <> ". "
      <> show left
      <> " = "
      <> show right

data Stream a = Stream a (Stream a) deriving (Eq, Show, Functor)

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

-- >>> (x, _X) = (Var "x", Base "X")
-- >>> (y, _Y) = (Var "y", Base "Y")
-- >>> decompose (Constraint [(x, _X)] (Variable x) (Variable x))
-- Just []
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Variable x) (Variable y))
-- Nothing
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Apply (Constant "A") (Constant "B")))
-- Just [forall x: X, y: Y. x = A,forall x: X, y: Y. y = B]
-- >>> decompose (Constraint [] (Lambda x _X (Variable x)) (Lambda y _X (Variable y)))
-- Just [forall x: X. x = x]
-- >>> decompose (Constraint [(x, _X), (y, _Y)] (Apply (Variable x) (Variable y)) (Variable x))
-- Nothing
decompose :: Constraint -> Maybe [Constraint]
decompose (Constraint _ (Constant left) (Constant right)) | left == right = Just []
decompose (Constraint _ (Variable left) (Variable right)) | left == right = Just []
decompose (Constraint context (Lambda leftBinder typ left) (Lambda rightBinder _ right)) =
  Just [Constraint ((leftBinder, typ) : context) left (rename rightBinder leftBinder right)]
decompose (Constraint context (Apply leftF leftA) (Apply rightF rightA)) =
  Just [Constraint context leftF rightF, Constraint context leftA rightA]
decompose _ = Nothing

-- >>> (x, y) = (Var "x", Var "y")
-- >>> snd <$> imitate [] someMetas (Base "A") (Constant "B")
-- Just B
-- >>> snd <$> imitate [(x, Base "X")] someMetas (Base "A") (Variable x)
-- Nothing
-- >>> snd <$> imitate [(y, Base "Y")] someMetas (Function (Base "A") (Base "A")) (Lambda x (Base "A") (Variable x))
-- Just \x: A. M0[x,y]
imitate :: [(Var, Type)] -> Metavariables -> Type -> Ast -> Maybe (Metavariables, Ast)
imitate _ metas _ (Constant constant) = Just (metas, Constant constant)
imitate _ _ _ (Variable _) = Nothing
imitate parameters metas (Function _binderType returnType) (Lambda binder binderType _) =
  Just (metas', Lambda binder binderType body)
 where
  (body, metas') = fresh ((binder, binderType) : parameters) returnType metas
imitate _ _ _ Lambda{} = Nothing -- indicate type error somehow? or remove the `Type` parameter and infer the inner type in a different way?
imitate parameters metaas typ (Apply _ _) =
  Just (metas'', Apply left right)
 where
  (left, metas') = fresh parameters (Function (Base "?") typ) metaas
  (right, metas'') = fresh parameters (Base "?") metas'
imitate _ _ _ (Meta _ _) = error "impossible" -- TODO: remove this possibility

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

-- >>> x = (Var "x", Function (Base "A") (Function (Base "A") (Base "A")))
-- >>> y = (Var "y", (Function (Base "A") (Base "B")))
-- >>> z = (Var "z", Base "A")
-- >>> snd <$> project [x, y, z] someMetas (Base "A")
-- [((x) (M0[x,y,z])) (M1[x,y,z]),z]
project :: [(Var, Type)] -> Metavariables -> Type -> [(Metavariables, Ast)]
project parameters metas expectedType = do
  (var, actualType) <- parameters
  reduce parameters metas expectedType actualType (Variable var)

-- >>> x = (Var "x", Function (Base "A") (Base "A"))
-- >>> snd <$> match [x] someMetas (Base "A") (Constant "A")
-- [A,(x) (M0[x])]
match :: [(Var, Type)] -> Metavariables -> Type -> Ast -> [(Metavariables, Ast)]
match parameters metas typ term =
  maybeToList (imitate parameters metas typ term)
    <> project parameters metas typ
