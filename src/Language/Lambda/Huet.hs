{-# LANGUAGE DeriveFunctor #-}

module Language.Lambda.Huet where

import Data.List (intercalate)

data Type = Base String | Function Type Type deriving (Eq)

instance Show Type where
  show (Base typ) = typ
  show (Function from to) = "(" <> show from <> ") -> " <> show to

newtype Var = Var String deriving (Eq, Show)
newtype Metavar = Metavar String deriving (Eq, Show)

data Ast
  = Constant String
  | Variable Var
  | Lambda Var Ast
  | Apply Ast Ast
  | Meta Metavar [Ast]
  deriving (Eq)

instance Show Ast where
  show (Constant constant) = constant
  show (Variable (Var variable)) = variable
  show (Lambda (Var binder) body) = "\\" <> binder <> ". " <> show body
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
rename from to (Lambda binder body) = Lambda binder body'
 where
  body'
    | binder == from = body
    | otherwise = rename from to body
rename from to (Apply left right) = Apply (rename from to left) (rename from to right)
rename from to (Meta metavariable arguments) = Meta metavariable (rename from to <$> arguments)

data Constraint = Constraint [Var] Ast Ast deriving (Eq)

instance Show Constraint where
  show (Constraint vars left right) =
    "forall " <> intercalate ", " (fmap (\(Var x) -> x) vars) <> ". " <> show left <> " = " <> show right

-- >>> (x, y) = (Var "x", Var "y")
-- >>> decompose (Constraint [x] (Variable (x)) (Variable (x)))
-- Just []
-- >>> decompose (Constraint [x, y] (Variable (x)) (Variable (y)))
-- Nothing
-- >>> decompose (Constraint [x, y] (Apply (Variable (x)) (Variable (y))) (Apply (Constant "A") (Constant "B")))
-- Just [forall x, y. x = A,forall x, y. y = B]
-- >>> decompose (Constraint [] (Lambda (x) (Variable (x))) (Lambda (y) (Variable (y))))
-- Just [forall x. x = x]
-- >>> decompose (Constraint [x, y] (Apply (Variable (x)) (Variable (y))) (Variable (x)))
-- Nothing
decompose :: Constraint -> Maybe [Constraint]
decompose (Constraint _ (Constant left) (Constant right)) | left == right = Just []
decompose (Constraint _ (Variable left) (Variable right)) | left == right = Just []
decompose (Constraint context (Lambda leftBinder left) (Lambda rightBinder right)) =
  Just [Constraint (leftBinder : context) left (rename rightBinder leftBinder right)]
decompose (Constraint context (Apply leftF leftA) (Apply rightF rightA)) =
  Just [Constraint context leftF rightF, Constraint context leftA rightA]
decompose _ = Nothing

data Stream a = Stream a (Stream a) deriving (Eq, Show, Functor)

metavariables :: Stream Metavar
metavariables = fmap (\i -> Metavar ("M" <> show i)) (nats (0 :: Integer))
 where
  nats i = Stream i (nats (i + 1))

-- >>> (x, y) = (Var "x", Var "y")
-- >>> snd <$> imitate metavariables [] (Constant "B")
-- Just B
-- >>> snd <$> imitate metavariables [x] (Variable x)
-- Nothing
-- >>> snd <$> imitate metavariables [y] (Lambda x (Variable x))
-- Just \x. M0[x,y]
imitate :: Stream Metavar -> [Var] -> Ast -> Maybe (Stream Metavar, Ast)
imitate metas _ (Constant constant) = Just (metas, Constant constant)
imitate _ _ (Variable _) = Nothing
imitate (Stream meta metas) parameters (Lambda binder _) =
  Just (metas, Lambda binder (Meta meta (Variable binder : (Variable <$> parameters))))
imitate (Stream left (Stream right metas)) parameters (Apply _ _) =
  Just (metas, Apply (Meta left arguments) (Meta right arguments))
 where
  arguments = Variable <$> parameters
imitate _ _ (Meta _ _) = error "impossible" -- TODO: remove this possibility
