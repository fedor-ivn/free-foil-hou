{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Lambda.Typed (
  Type (..),
  Variable (..),
  Metavariable (..),
  Atom (..),
  Expression (..),
  pattern Var,
  pattern Metavar,
  typeOf,
  Heading (..),
  NormalTerm' (..),
  NormalTerm,
  RigidTerm,
  FlexibleTerm,
  asNormalTerm,
  DisagreementSet (..),
  Stream (..),
  someVariables,
  someMetavariables,
  solve,
) where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Prelude hiding (head)

data Type
  = Base String
  | Function Type Type
  deriving (Eq)

instance Show Type where
  show (Base typ) = typ
  show (Function parameter returnType) = parameter' <> " -> " <> show returnType
   where
    parameter'
      | Base _ <- parameter = show parameter
      | Function _ _ <- parameter = "(" <> show parameter <> ")"

dropParameters :: Int -> Type -> Type
dropParameters _ typ@(Base _) = typ
dropParameters n typ@(Function _ rest)
  | n <= 0 = typ
  | otherwise = dropParameters (n - 1) rest

takeParameters :: Int -> Type -> [Type]
takeParameters _ (Base _) = []
takeParameters n (Function parameter rest)
  | n <= 0 = []
  | otherwise = parameter : takeParameters (n - 1) rest

countParameters :: Type -> Int
countParameters (Base _) = 0
countParameters (Function _ rest) = 1 + countParameters rest

getExtraParameters :: Type -> Type -> Maybe [Type]
getExtraParameters returnType wholeType
  | remainingType == returnType = Just extraParameters
  | otherwise = Nothing
 where
  extraParametersCount = countParameters wholeType - countParameters returnType
  extraParameters = takeParameters extraParametersCount wholeType
  remainingType = dropParameters extraParametersCount wholeType

newtype Variable = Variable String deriving (Eq)

instance Show Variable where
  show (Variable variable) = variable

newtype Metavariable = Metavariable String deriving (Eq)

instance Show Metavariable where
  show (Metavariable metavariable) = metavariable

data Atom
  = AVar Variable
  | AMetavar Metavariable
  deriving (Eq)

instance Show Atom where
  show (AVar variable) = show variable
  show (AMetavar metavariable) = show metavariable

data Expression
  = Atom Atom
  | Application Expression Expression
  | Lambda Variable Type Expression
  deriving (Eq, Show)

pattern Var :: Variable -> Expression
pattern Var variable = Atom (AVar variable)

pattern Metavar :: Metavariable -> Expression
pattern Metavar variable = Atom (AMetavar variable)

-- >>> t = Base "t"
-- >>> u = Base "u"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
--
-- >>> typeOf (const (Just t)) (Var x)
-- Just t
-- >>> typeOf (const Nothing) (Lambda x t (Var x))
-- Just t -> t
-- >>> typeOf (const (Just u)) (Lambda x t (Var y))
-- Just t -> u
-- >>> typeOf (const (Just t)) (Application (Lambda x t (Var x)) (Var x))
-- Just t
-- >>> typeOf (const (Just u)) (Application (Lambda x t (Var x)) (Var x))
-- Nothing
typeOf :: (Atom -> Maybe Type) -> Expression -> Maybe Type
typeOf typeOfAtom (Atom atom) = typeOfAtom atom
typeOf typeOfAtom (Application function argument) = do
  functionType <- typeOf typeOfAtom function
  argumentType <- typeOf typeOfAtom argument
  case functionType of
    Function argumentType' returnType
      | argumentType == argumentType' -> Just returnType
    _ -> Nothing
typeOf typeOfAtom (Lambda boundVariable typ body) = do
  returnType <- flip typeOf body $ \variable ->
    if variable == AVar boundVariable
      then Just typ
      else typeOfAtom variable
  Just (Function typ returnType)

data Heading head = Heading
  { binder :: [(Variable, Type)]
  , head :: head
  }
  deriving (Eq)

instance (Show head) => Show (Heading head) where
  show (Heading binder head) = binder' <> show head
   where
    binder'
      | null binder = ""
      | otherwise = "λ" <> intercalate ", " (fmap showParameter binder) <> ". "
    showParameter (variable, typ) = show variable <> ": " <> show typ

-- Ignores types of parameters (at least for now).
--
-- >>> a = Variable "a"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
--
-- >>> areAlphaEquivalent (Heading [(x, t)] x) (Heading [(y, t)] y)
-- True
-- >>> areAlphaEquivalent (Heading [(x, t)] a) (Heading [(y, t)] a)
-- True
-- >>> areAlphaEquivalent (Heading [(x, t)] x) (Heading [(y, t)] x)
-- False
-- >>> areAlphaEquivalent (Heading [(y, t)] x) (Heading [(x, t)] x)
-- False
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] x) (Heading [(x, t)] x)
-- False
areAlphaEquivalent :: Heading Variable -> Heading Variable -> Bool
areAlphaEquivalent (Heading [] leftHead) (Heading [] rightHead) =
  leftHead == rightHead
areAlphaEquivalent
  (Heading ((leftVariable, _) : leftBinder) leftHead)
  (Heading ((rightVariable, _) : rightBinder) rightHead)
    | leftVariable == leftHead =
        rightVariable == rightHead && length leftBinder == length rightBinder
    | rightVariable == rightHead = False
    | otherwise =
        areAlphaEquivalent
          (Heading leftBinder leftHead)
          (Heading rightBinder rightHead)
areAlphaEquivalent _ _ = False

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
--
-- >>> imitatableHead (Heading [] x) 0
-- Just (Right (Variable "x"))
-- >>> imitatableHead (Heading [(y, t)] x) 0
-- Just (Right (Variable "x"))
-- >>> imitatableHead (Heading [(y, t)] y) 0
-- Just (Left 0)
-- >>> imitatableHead (Heading [(x, t), (y, t)] y) 0
-- Just (Left 1)
-- >>> imitatableHead (Heading [(x, t), (y, t)] y) 1
-- Just (Left 0)
-- >>> imitatableHead (Heading [(y, t)] y) 1
-- Nothing
imitatableHead :: Heading Variable -> Int -> Maybe (Either Int Variable)
imitatableHead (Heading [] head) _ = Just (Right head)
imitatableHead (Heading ((variable, _) : binder) head) n
  | variable == head && n <= 0 = Just (Left (-n))
  | variable == head = Nothing
  | otherwise = imitatableHead (Heading binder head) (n - 1)

data NormalTerm' head = NormalTerm
  { heading :: Heading head
  , arguments :: [NormalTerm]
  , returnType :: Type
  }
  deriving (Eq)

instance (Show head) => Show (NormalTerm' head) where
  show (NormalTerm heading arguments returnType) =
    show heading <> arguments' <> " :: " <> show returnType
   where
    arguments'
      | null arguments = ""
      | otherwise = " " <> unwords (fmap (\argument -> "(" <> show argument <> ")") arguments)

type NormalTerm = NormalTerm' Atom
type RigidTerm = NormalTerm' Variable
type FlexibleTerm = NormalTerm' Metavariable

pattern Term :: [(Variable, Type)] -> head -> [NormalTerm] -> Type -> NormalTerm' head
pattern Term binder head arguments returnType = NormalTerm (Heading binder head) arguments returnType

apply :: head -> [NormalTerm] -> Type -> NormalTerm' head
apply = Term []

var :: head -> Type -> NormalTerm' head
var head = apply head []

termKind :: NormalTerm -> Either FlexibleTerm RigidTerm
termKind NormalTerm{heading = Heading binder (AVar head), ..} =
  Right (NormalTerm{heading = Heading binder head, ..})
termKind NormalTerm{heading = Heading binder (AMetavar head), ..} =
  Left (NormalTerm{heading = Heading binder head, ..})

pattern Rigid :: RigidTerm -> NormalTerm
pattern Rigid rigid <- (termKind -> Right rigid)
  where
    Rigid NormalTerm{heading = Heading binder head, ..} =
      NormalTerm{heading = Heading binder (AVar head), ..}

pattern Flexible :: FlexibleTerm -> NormalTerm
pattern Flexible flexible <- (termKind -> Left flexible)
  where
    Flexible NormalTerm{heading = Heading binder head, ..} =
      NormalTerm{heading = Heading binder (AMetavar head), ..}

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> z = Variable "z"
-- >>> t = Base "t"
-- >>> typeOfAtom var = if var == AVar x then Just (Function t (Function t t)) else Just t
--
-- >>> asNormalTerm typeOfAtom (Var x)
-- Just x :: t -> t -> t
--
-- >>> asNormalTerm typeOfAtom (Application (Var x) (Var y))
-- Just x (y :: t) :: t -> t
--
-- >>> asNormalTerm typeOfAtom (Application (Application (Var x) (Var y)) (Var z))
-- Just x (y :: t) (z :: t) :: t
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Var x))
-- Just λz: t. x :: t -> t -> t
--
-- >>> asNormalTerm typeOfAtom (Lambda y t (Lambda z t (Var x)))
-- Just λy: t, z: t. x :: t -> t -> t
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Application (Var x) (Var y)))
-- Just λz: t. x (y :: t) :: t -> t
--
-- >>> asNormalTerm typeOfAtom (Application (Lambda x t (Var x)) (Var y))
-- Nothing
--
-- >>> asNormalTerm typeOfAtom (Lambda x t (Application (Lambda z t (Var z)) (Var y)))
-- Nothing
asNormalTerm :: (Atom -> Maybe Type) -> Expression -> Maybe NormalTerm
asNormalTerm typeOfAtom (Lambda parameter typ body) = do
  let typeOfAtom' variable
        | variable == AVar parameter = Just typ
        | otherwise = typeOfAtom variable
  NormalTerm{heading = (Heading binder head), ..} <- asNormalTerm typeOfAtom' body
  Just (NormalTerm{heading = Heading ((parameter, typ) : binder) head, ..})
asNormalTerm typeOfAtom other = do
  returnType <- typeOf typeOfAtom other
  (heading, arguments) <- extractBody other
  return (NormalTerm heading arguments returnType)
 where
  extractBody Lambda{} = Nothing
  extractBody (Application function argument) = do
    (heading, arguments) <- extractBody function
    normalArgument <- asNormalTerm typeOfAtom argument
    Just (heading, arguments <> [normalArgument])
  extractBody (Atom head) = Just (Heading [] head, [])

scopedArguments :: NormalTerm' head -> [NormalTerm]
scopedArguments (NormalTerm (Heading scope _) arguments _) =
  scopedArgument <$> arguments
 where
  scopedArgument (NormalTerm (Heading binder head) subarguments returnType) =
    NormalTerm (Heading (scope <> binder) head) subarguments returnType

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
-- >>> u = Base "u"
--
-- >>> typeOfTerm (NormalTerm (Heading [(x, t)] y) [] u)
-- t -> u
-- >>> typeOfTerm (NormalTerm (Heading [(x, t), (y, u)] y) [] u)
-- t -> u -> u
-- >>> typeOfTerm (NormalTerm (Heading [(x, t)] y) [NormalTerm (Heading [] (AVar x)) [] u] u)
-- t -> u
typeOfTerm :: NormalTerm' head -> Type
typeOfTerm NormalTerm{heading = Heading [] _, returnType} = returnType
typeOfTerm NormalTerm{heading = Heading ((_, parameterType) : binder) head, ..} =
  Function parameterType (typeOfTerm (NormalTerm{heading = Heading binder head, ..}))

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
-- >>> u = Base "u"
--
-- >>> typeOfHead (NormalTerm (Heading [(x, t)] y) [] u)
-- u
-- >>> typeOfHead (NormalTerm (Heading [(x, t), (y, u)] y) [] u)
-- u
-- >>> typeOfHead (NormalTerm (Heading [(x, t)] y) [NormalTerm (Heading [] (AVar x)) [] u] u)
-- u -> u
typeOfHead :: NormalTerm' head -> Type
typeOfHead NormalTerm{arguments = [], returnType} = returnType
typeOfHead NormalTerm{arguments = (argument : arguments), ..} =
  Function (typeOfTerm argument) (typeOfHead NormalTerm{arguments, ..})

-- >>> t = Base "t"
-- >>> _M = AMetavar (Metavariable "M")
-- >>> (a, a') = (AVar a', Variable "a")
-- >>> (b, b') = (AVar b', Variable "b")
-- >>> (x, x') = (AVar x', Variable "x")
--
-- >>> substituteHead (Term [(x', t)] x [] t) (var _M t)
-- λx: t. x :: t
-- >>> substituteHead (var x t) (Term [(a', t)] _M [] t)
-- λa: t. x :: t
-- >>> substituteHead (Term [(a', t)] a [] t) (Term [(b', t)] _M [var x t] t)
-- λb: t. x :: t
substituteHead :: NormalTerm -> NormalTerm' head -> NormalTerm
substituteHead
  (NormalTerm (Heading innerBinder head) arguments returnType)
  (NormalTerm (Heading outerBinder _) [] _) =
    Term (outerBinder <> innerBinder) head arguments returnType
substituteHead
  (NormalTerm (Heading [] head) innerArguments _)
  (NormalTerm (Heading outerBinder _) outerArguments returnType) =
    Term outerBinder head (innerArguments <> outerArguments) returnType
substituteHead
  (NormalTerm (Heading ((parameter, _) : innerBinder) innerHead) innerArguments innerReturnType)
  (NormalTerm (Heading outerBinder outerHead) (argument : outerArguments) outerReturnType) =
    substituteHead
      (substitute (AVar parameter) argument withoutParameter)
      (Term outerBinder outerHead outerArguments outerReturnType)
   where
    withoutParameter = Term innerBinder innerHead innerArguments innerReturnType

-- >>> t = Base "t"
-- >>> _M = AMetavar (Metavariable "M")
-- >>> a = AVar (Variable "a")
-- >>> (x, x') = (AVar x', Variable "x")
--
-- >>> substitute _M (var a (Function t (Function t t))) (apply _M [var x t, var x t] t)
-- a (x :: t) (x :: t) :: t
-- >>> substitute _M (var a t) (apply x [var _M t, var _M t] t)
-- x (a :: t) (a :: t) :: t
-- >>> substitute _M (Term [(x', t)] x [] t) (apply _M [var a t] t)
-- a :: t
substitute :: Atom -> NormalTerm -> NormalTerm -> NormalTerm
substitute
  expected
  substitution
  outer@(NormalTerm (Heading binder head) arguments returnType)
    | head == expected =
        substitute expected substitution (substituteHead substitution outer)
    | otherwise =
        Term binder head (substitute expected substitution <$> arguments) returnType

newtype DisagreementSet = DisagreementSet [(NormalTerm, NormalTerm)]
  deriving (Eq)

addToSet :: NormalTerm -> NormalTerm -> DisagreementSet -> DisagreementSet
addToSet left right (DisagreementSet set) = DisagreementSet ((left, right) : set)

substituteInSet :: Atom -> NormalTerm -> DisagreementSet -> DisagreementSet
substituteInSet head substitution (DisagreementSet set) =
  DisagreementSet (bimap go go <$> set)
 where
  go = substitute head substitution

instance Show DisagreementSet where
  show (DisagreementSet []) = "{}"
  show (DisagreementSet set) = "{ " <> set' <> " }"
   where
    set' = intercalate "; " (showPair <$> set)
    showPair (left, right) = show left <> " = " <> show right

newtype Substitutions = Substitutions [(Metavariable, NormalTerm)]
  deriving (Eq)

instance Show Substitutions where
  show (Substitutions []) = "{}"
  show (Substitutions substitutions) = "{ " <> substitutions' <> " }"
   where
    substitutions' = intercalate "; " (showSubstitution <$> substitutions)
    showSubstitution (left, right) = show left <> " => " <> show right

addSubstitution :: Metavariable -> NormalTerm -> Substitutions -> Substitutions
addSubstitution head substitution (Substitutions substitutions) =
  Substitutions ((head, substitution) : substitutions)

-- >>> _F = AMetavar (Metavariable "F")
-- >>> _X = AMetavar (Metavariable "X")
-- >>> _W = AMetavar (Metavariable "W")
-- >>> (a', a) = (Variable "a", AVar a')
-- >>> (b', b) = (Variable "b", AVar b')
-- >>> (c', c) = (Variable "c", AVar c')
-- >>> (d', d) = (Variable "d", AVar d')
-- >>> (e', e) = (Variable "e", AVar e')
-- >>> (f', f) = (Variable "f", AVar f')
-- >>> (u', u) = (Variable "u", AVar u')
-- >>> (v', v) = (Variable "v", AVar v')
-- >>> t = Base "t"
--
-- >>> left = apply _F [apply _F [var _X t] t] t
-- >>> right = apply a [apply a [var b t] t] t
-- >>> solve (DisagreementSet [(left, right)]) someVariables someMetavariables
-- [({ X => b :: t; F => a :: t -> t },{}),({ M1 => λv2: t. b :: t; M0 => λv1: t. a (M1 (v1 :: t) :: t) :: t; F => λv0: t. a (M0 (v0 :: t) :: t) :: t },{}),({ X => b :: t; M0 => λv1: t. v1 :: t; F => λv0: t. a (M0 (v0 :: t) :: t) :: t },{}),({ M1 => b :: t; M0 => a (M1 :: t) :: t; X => a (M0 :: t) :: t; F => λv0: t. v0 :: t },{})]
--
-- >>> tc = Function (Function t t) (Function t (Function t t))
-- >>> left = var _X (Function tc t)
-- >>> ex = Term [(d', Function t t), (e', t), (f', t)] a [apply d [var e t] t, var f t] t
-- >>> right = Term [(u', tc)] u [Term [(v', t)] _W [] t, apply _X [ex] t, apply _X [var _F tc] t] t
-- >>> solve (DisagreementSet [(left, right)]) someVariables someMetavariables
-- [({ M1 => λv1: (t -> t) -> t -> t -> t. a (M3 (v1 :: (t -> t) -> t -> t -> t) :: t) (M4 (v1 :: (t -> t) -> t -> t -> t) :: t) :: t; X => λv0: (t -> t) -> t -> t -> t. v0 (M0 (v0 :: (t -> t) -> t -> t -> t) :: t -> t) (M1 (v0 :: (t -> t) -> t -> t -> t) :: t) (M2 (v0 :: (t -> t) -> t -> t -> t) :: t) :: t },{ λv0: (t -> t) -> t -> t -> t. M3 (v0 :: (t -> t) -> t -> t -> t) :: t = λu: (t -> t) -> t -> t -> t. M0 (λd: t -> t, e: t, f: t. a (d (e :: t) :: t) (f :: t) :: t) (a (M3 (λd: t -> t, e: t, f: t. a (d (e :: t) :: t) (f :: t) :: t) :: t) (M4 (λd: t -> t, e: t, f: t. a (d (e :: t) :: t) (f :: t) :: t) :: t) :: t) :: t; λv0: (t -> t) -> t -> t -> t. M4 (v0 :: (t -> t) -> t -> t -> t) :: t = λu: (t -> t) -> t -> t -> t. M2 (λd: t -> t, e: t, f: t. a (d (e :: t) :: t) (f :: t) :: t) :: t; λv0: (t -> t) -> t -> t -> t. M0 (v0 :: (t -> t) -> t -> t -> t) :: t -> t = λu: (t -> t) -> t -> t -> t, v: t. W :: t; λv0: (t -> t) -> t -> t -> t. M2 (v0 :: (t -> t) -> t -> t -> t) :: t = λu: (t -> t) -> t -> t -> t. F (M0 (F :: (t -> t) -> t -> t -> t) :: t -> t) (a (M3 (F :: (t -> t) -> t -> t -> t) :: t) (M4 (F :: (t -> t) -> t -> t -> t) :: t) :: t) (M2 (F :: (t -> t) -> t -> t -> t) :: t) :: t })]
--
-- >>> left = apply a [var _X t, apply _F [var b (Function t t)] t] t
-- >>> right = apply a [apply _F [Term [(u', t)] _X [] t] t, apply b [var _X t] t] t
-- >>> take 1 $ solve (DisagreementSet [(left, right)]) someVariables someMetavariables
-- [({ F => λv0: t -> t. v0 (M0 (v0 :: t -> t) :: t) :: t },{ M0 (b :: t -> t) :: t = X :: t; X :: t = X :: t })]
solve
  :: DisagreementSet
  -> Stream Variable
  -> Stream Metavariable
  -> [(Substitutions, DisagreementSet)]
solve initialSet v m = go' [(Context (Substitutions []) v m, initialSet)]
 where
  go' [] = []
  go' sets = (extractSubstitutions <$> solved) <> go' (goSolve unsolved)
   where
    (solved, unsolved) = pickPairs (simplifySets sets)
    extractSubstitutions (Context substitutions _ _, set) = (substitutions, set)
  goSolve problems = do
    (Context substitutions variables metavariables, (flexible, rigid, rest)) <- problems
    (substitution, variables', metavariables') <- match flexible rigid variables metavariables

    let unknown = head (heading flexible)
    let set' = addToSet (Flexible flexible) (Rigid rigid) rest
    let set'' = substituteInSet (AMetavar unknown) substitution set'
    let substitutions' = addSubstitution unknown substitution substitutions
    return (Context substitutions' variables' metavariables', set'')

-- go substitutions set variables metavariables = do
--   set' <- maybeToList (simplify set)
--   case pickPair set' of
--     Nothing -> return (substitutions, set')
--     Just (flexible, rigid, DisagreementSet rest) -> do
--       (substitution, variables', metavariables') <-
--         match flexible rigid variables metavariables
--       let metavar = head (heading flexible)
--       let substitutions' = addSubstitution metavar substitution substitutions
--       let left = substitute (AMetavar metavar) substitution (Flexible flexible)
--       let right = substitute (AMetavar metavar) substitution (Rigid rigid)
--       go substitutions' (DisagreementSet ((left, right) : rest)) variables' metavariables'

data Context = Context Substitutions (Stream Variable) (Stream Metavariable)

simplifySets :: [(c, DisagreementSet)] -> [(c, DisagreementSet)]
simplifySets [] = []
simplifySets ((s, set) : rest) = case simplify set of
  Nothing -> simplifySets rest
  Just set' -> (s, set') : simplifySets rest

type Solved c = (c, DisagreementSet)
type Solvable c = (c, (FlexibleTerm, RigidTerm, DisagreementSet))
pickPairs :: [(c, DisagreementSet)] -> ([Solved c], [Solvable c])
pickPairs = go id id
 where
  go solved unsolved [] = (solved [], unsolved [])
  go solved unsolved ((s, set) : rest) = case pickPair set of
    Nothing -> go (solved . ((s, set) :)) unsolved rest
    Just problem -> go solved (unsolved . ((s, problem) :)) rest

pickPair :: DisagreementSet -> Maybe (FlexibleTerm, RigidTerm, DisagreementSet)
pickPair (DisagreementSet set) = go id set
 where
  go _ [] = Nothing
  go previous ((Flexible flexible, Rigid rigid) : rest) =
    Just (flexible, rigid, DisagreementSet (previous rest))
  go previous ((Rigid rigid, Flexible flexible) : rest) =
    Just (flexible, rigid, DisagreementSet (previous rest))
  go previous (pair : rest) = go (previous . (pair :)) rest

-- >>> a = Variable "a"
-- >>> b = Variable "b"
-- >>> c = Variable "c"
-- >>> f = Metavariable "F"
-- >>> u = Variable "u"
-- >>> v = Variable "v"
-- >>> w = Variable "w"
-- >>> x = Metavariable "X"
-- >>> y = Metavariable "Y"
-- >>> t = Base "t"
-- >>> var v t = NormalTerm (Heading [] (AVar v)) [] t
-- >>> meta v t = NormalTerm (Heading [] (AMetavar v)) [] t
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x t, var u t] t, var c t] t
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y t, var v t] t, NormalTerm (Heading [] (AMetavar f)) [var c t] t] t
-- >>> simplify (DisagreementSet [(left, right)])
-- Just { λu: t. X :: t = λv: t. Y :: t; c :: t = F (c :: t) :: t }
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x t, var u t] t] t
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y t, var v t] t] t
-- >>> simplify (DisagreementSet [(left, right)])
-- Just { λu: t. X :: t = λv: t. Y :: t }
--
-- >>> left = NormalTerm (Heading [(u, t), (v, t)] (AVar a)) [var u t, NormalTerm (Heading [(w, t)] (AVar v)) [] t] t
-- >>> right = NormalTerm (Heading [(v, t), (w, t)] (AVar a)) [var v t, NormalTerm (Heading [(u, t)] (AVar v)) [] t] t
-- >>> simplify (DisagreementSet [(left, right)])
-- Nothing
simplify :: DisagreementSet -> Maybe DisagreementSet
simplify (DisagreementSet set) = DisagreementSet <$> simplifyRigidPairs set
 where
  simplifyRigidPairs = foldr simplifyRigidPair (Just [])
  simplifyRigidPair (Rigid left, Rigid right) rest
    | areAlphaEquivalent (heading left) (heading right) = do
        newPairs <- simplifyRigidPairs (zip (scopedArguments left) (scopedArguments right))
        rest' <- rest
        return (newPairs <> rest')
    | otherwise = Nothing
  simplifyRigidPair pair rest = (pair :) <$> rest

data Stream a = Stream a (Stream a) deriving (Eq, Show, Functor)

zipWithList :: Stream a -> [b] -> (Stream a, [(a, b)])
zipWithList as [] = (as, [])
zipWithList (Stream a as) (b : bs) = (as', (a, b) : bs')
 where
  (as', bs') = zipWithList as bs

integers :: Int -> Stream Int
integers n = Stream n (integers (n + 1))

someMetavariables :: Stream Metavariable
someMetavariables = fmap (\x -> Metavariable ("M" <> show x)) (integers 0)

someVariables :: Stream Variable
someVariables = fmap (\x -> Variable ("v" <> show x)) (integers 0)

-- >>> t = Base "t"
-- >>> _M = Metavariable "M"
-- >>> a = Variable "a"
-- >>> b = Variable "b"
-- >>> c = Variable "c"
-- >>> show' = fmap (\(x, _, _) -> x)
--
-- >>> show' $ match (Term [(a, t)] _M [] t) (var a t) someVariables someMetavariables
-- []
-- >>> show' $ match (var _M t) (var a t) someVariables someMetavariables
-- [a :: t]
-- >>> show' $ match (var _M t) (apply a [var (AVar b) t] t) someVariables someMetavariables
-- [a (M0 :: t) :: t]
-- >>> show' $ match (apply _M [var (AVar b) t] t) (var a t) someVariables someMetavariables
-- [λv0: t. a :: t,λv0: t. v0 :: t]
-- >>> show' $ match (apply _M [var (AVar b) t] t) (Term [(b, t)] a [var (AVar c) t] t) someVariables someMetavariables
-- [λv0: t, v1: t. a (M0 (v0 :: t) (v1 :: t) :: t) :: t,λv0: t. v0 :: t,λv0: t, v1: t. v0 :: t]
-- >>> show' $ match (apply _M [apply (AVar b) [var (AVar a) (Function t t)] t] t) (apply a [apply (AVar b) [var (AMetavar _M) (Function t t)] t] t) someVariables someMetavariables
-- [a :: t -> t,λv0: t. a (M0 (v0 :: t) :: t) :: t,λv0: t. v0 :: t]
-- >>> show' $ match (var _M (Function t (Function t t))) (Term [(a, t), (b, t)] a [] t) someVariables someMetavariables
-- [λv0: t, v1: t. v0 :: t]
match
  :: FlexibleTerm
  -> RigidTerm
  -> Stream Variable
  -> Stream Metavariable
  -> [(NormalTerm, Stream Variable, Stream Metavariable)]
match flexible rigid variables metavariables
  | n < 0 = []
  | otherwise = imitate <> project
 where
  n_flexible = length (binder (heading flexible))
  n_rigid = length (binder (heading rigid))
  n = n_rigid - n_flexible

  p_flexible = length (arguments flexible)
  p_rigid = length (arguments rigid)

  imitate =
    case imitatableHead (heading rigid) n_flexible of
      Nothing -> []
      Just head
        | n > 0 -> [imitateWithExtendedBinder head]
        | otherwise -> imitateDirectly head

  imitateWithExtendedBinder head =
    makeSubstitution binderTypes head' argumentTypes (returnType rigid)
   where
    binderTypes = ws <> vs
    ws = typeOfTerm <$> arguments flexible
    vs = snd <$> drop n_flexible (binder (heading rigid))
    argumentTypes = typeOfTerm <$> arguments rigid
    head'
      | Right constant <- head = const constant
      | Left offset <- head = (!! (length ws + offset))

  imitateDirectly head = do
    head' <- case head of
      Right head' -> return head'
      -- A bound argument? In my direct imitation?
      Left _ -> []
    k <- [max 0 (p_flexible - p_rigid) .. p_flexible]

    let (takenFlexible, remainingFlexible) =
          splitAt k (typeOfTerm <$> arguments flexible)
    let (takenRigid, remainingRigid) =
          splitAt (p_rigid - p_flexible + k) (typeOfTerm <$> arguments rigid)
    let returnType' = foldr Function (returnType rigid) remainingRigid

    if remainingFlexible == remainingRigid
      then
        return (makeSubstitution takenFlexible (const head') takenRigid returnType')
      else
        []

  project = do
    let ws = typeOfTerm <$> arguments flexible
    let vs = snd <$> drop n_flexible (binder (heading rigid))
    let wholeBinderTypes = ws <> vs

    n_substitution <- [1 .. p_flexible + n]
    projectOn <- [0 .. min n_substitution p_flexible - 1]

    let binderTypes = take n_substitution wholeBinderTypes
    let projectedType = binderTypes !! projectOn
    let returnType' = dropParameters n_substitution (typeOfHead flexible)

    case getExtraParameters returnType' projectedType of
      Nothing -> []
      Just extraParameters ->
        [makeSubstitution binderTypes (!! projectOn) extraParameters returnType']

  makeSubstitution
    :: [Type]
    -> ([Variable] -> Variable)
    -> [Type]
    -> Type
    -> (NormalTerm, Stream Variable, Stream Metavariable)
  makeSubstitution binderTypes head argumentTypes returnType =
    (term, variables', metavariables')
   where
    term = Term binder (AVar (head (fst <$> binder))) arguments returnType
    (variables', binder) = zipWithList variables binderTypes
    propagatedBinder =
      binder <&> \(parameter, parameterType) ->
        var (AVar parameter) parameterType

    (metavariables', hs) = zipWithList metavariables argumentTypes
    arguments =
      hs <&> \(h, argumentType) ->
        apply (AMetavar h) propagatedBinder argumentType
