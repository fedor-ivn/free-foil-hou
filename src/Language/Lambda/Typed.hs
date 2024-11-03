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
  pattern Rigid,
  pattern Flexible,
  asNormalTerm,
  DisagreementSet (..),
  simplify,
  match,
) where

import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Prelude hiding (head)

data Type
  = Base String
  | Function Type Type
  deriving (Eq, Show)

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

newtype Variable = Variable String deriving (Eq, Show)

newtype Metavariable = Metavariable String deriving (Eq, Show)

data Atom
  = AVar Variable
  | AMetavar Metavariable
  deriving (Eq, Show)

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
-- Just (Base "t")
-- >>> typeOf (const Nothing) (Lambda x t (Var x))
-- Just (Function (Base "t") (Base "t"))
-- >>> typeOf (const (Just u)) (Lambda x t (Var y))
-- Just (Function (Base "t") (Base "u"))
-- >>> typeOf (const (Just t)) (Application (Lambda x t (Var x)) (Var x))
-- Just (Base "t")
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
  deriving (Eq, Show)

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

imitatableHead :: Heading Variable -> Int -> Maybe Variable
imitatableHead (Heading [] head) _ = Just head
imitatableHead (Heading ((variable, _) : binder) head) n
  | variable == head = Nothing
  | n <= 0 = Just head
  | otherwise = imitatableHead (Heading binder head) (n - 1)

data NormalTerm' head = NormalTerm
  { heading :: Heading head
  , arguments :: [NormalTerm]
  , returnType :: Type
  }
  deriving (Eq, Show)

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
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Application (Var x) (Var y))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"}], returnType = Function (Base "t") (Base "t")})
--
-- >>> asNormalTerm typeOfAtom (Application (Application (Var x) (Var y)) (Var z))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "z")}, arguments = [], returnType = Base "t"}], returnType = Base "t"})
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Var x))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Lambda y t (Lambda z t (Var x)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "y",Base "t"),(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Application (Var x) (Var y)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"}], returnType = Function (Base "t") (Base "t")})
--
-- >>> asNormalTerm typeOfAtom (Application (Lambda x t (Var x)) (Var y))
-- Nothing
--
-- >>> asNormalTerm typeOfAtom (Lambda x t (Application (Lambda z t (Var z)) (Var y)))
-- Nothing
asNormalTerm :: (Atom -> Maybe Type) -> Expression -> Maybe NormalTerm
asNormalTerm typeOfAtom (Lambda variable typ body) = do
  let typeOfAtom' var
        | var == AVar variable = Just typ
        | otherwise = typeOfAtom var
  NormalTerm{heading = (Heading binder head), ..} <- asNormalTerm typeOfAtom' body
  Just (NormalTerm{heading = Heading ((variable, typ) : binder) head, ..})
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
-- Function (Base "t") (Base "u")
-- >>> typeOfTerm (NormalTerm (Heading [(x, t), (y, u)] y) [] u)
-- Function (Base "t") (Function (Base "u") (Base "u"))
-- >>> typeOfTerm (NormalTerm (Heading [(x, t)] y) [NormalTerm (Heading [] (AVar x)) [] u] u)
-- Function (Base "t") (Base "u")
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
-- Base "u"
-- >>> typeOfHead (NormalTerm (Heading [(x, t), (y, u)] y) [] u)
-- Base "u"
-- >>> typeOfHead (NormalTerm (Heading [(x, t)] y) [NormalTerm (Heading [] (AVar x)) [] u] u)
-- Function (Base "u") (Base "u")
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
-- NormalTerm {heading = Heading {binder = [(Variable "x",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Base "t"}
-- >>> substituteHead (var x t) (Term [(a', t)] _M [] t)
-- NormalTerm {heading = Heading {binder = [(Variable "a",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Base "t"}
-- >>> substituteHead (Term [(a', t)] a [] t) (Term [(b', t)] _M [var x t] t)
-- NormalTerm {heading = Heading {binder = [(Variable "b",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Base "t"}
substituteHead :: NormalTerm -> NormalTerm' head -> NormalTerm
substituteHead
  (Term innerBinder head arguments returnType)
  (Term outerBinder _ [] _) =
    Term (outerBinder <> innerBinder) head arguments returnType
substituteHead
  (Term [] head innerArguments _)
  (Term outerBinder _ outerArguments returnType) =
    Term outerBinder head (innerArguments <> outerArguments) returnType
substituteHead
  (Term ((parameter, _) : innerBinder) innerHead innerArguments innerReturnType)
  (Term outerBinder outerHead (argument : outerArguments) outerReturnType) =
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
-- NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [], returnType = Base "t"},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}
-- >>> substitute _M (var a t) (apply x [var _M t, var _M t] t)
-- NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Base "t"},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}
-- >>> substitute _M (Term [(x', t)] x [] t) (apply _M [var a t] t)
-- NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Base "t"}
substitute :: Atom -> NormalTerm -> NormalTerm -> NormalTerm
substitute expected substitution outer@(Term binder head arguments returnType)
  | head == expected = substitute expected substitution (substituteHead substitution outer)
  | otherwise = Term binder head (substitute expected substitution <$> arguments) returnType

newtype DisagreementSet = DisagreementSet [(NormalTerm, NormalTerm)]
  deriving (Eq, Show)

newtype Substitutions = Substitutions [(Metavariable, NormalTerm)]
  deriving (Eq, Show)

addSubstitution :: Metavariable -> NormalTerm -> Substitutions -> Substitutions
addSubstitution head substitution (Substitutions substitutions) =
  Substitutions ((head, substitution) : substitutions')
 where
  substitutions' = fmap (substitute (AMetavar head) substitution) <$> substitutions

-- >>> _F = AMetavar (Metavariable "F")
-- >>> _X = AMetavar (Metavariable "X")
-- >>> a = AVar (Variable "a")
-- >>> b = AVar (Variable "b")
-- >>> t = Base "t"
--
-- >>> left = apply _F [apply _F [var _X t] t] t
-- >>> right = apply a [apply a [var b t] t] t
-- >>> solve (DisagreementSet [(left, right)]) someVariables someMetavariables
-- [(Substitutions [(Metavariable "X",NormalTerm {heading = Heading {binder = [], head = AVar (Variable "b")}, arguments = [], returnType = Base "t"}),(Metavariable "F",NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Function (Base "t") (Base "t")})],DisagreementSet []),(Substitutions [(Metavariable "M1",NormalTerm {heading = Heading {binder = [(Variable "v2",Base "t")], head = AVar (Variable "b")}, arguments = [], returnType = Base "t"}),(Metavariable "M0",NormalTerm {heading = Heading {binder = [(Variable "v1",Base "t")], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "b")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}),(Metavariable "F",NormalTerm {heading = Heading {binder = [(Variable "v0",Base "t")], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "b")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}], returnType = Base "t"})],DisagreementSet [])]
solve
  :: DisagreementSet
  -> Stream Variable
  -> Stream Metavariable
  -> [(Substitutions, DisagreementSet)]
solve = go (Substitutions [])
 where
  go substitutions set variables metavariables = do
    set' <- maybeToList (simplify set)
    case pickPair set' of
      Nothing -> return (substitutions, set')
      Just (flexible, rigid, DisagreementSet rest) -> do
        (substitution, variables', metavariables') <-
          match flexible rigid variables metavariables
        let metavar = head (heading flexible)
        let substitutions' = addSubstitution metavar substitution substitutions
        let left = substitute (AMetavar metavar) substitution (Flexible flexible)
        let right = substitute (AMetavar metavar) substitution (Rigid rigid)
        go substitutions' (DisagreementSet ((left, right) : rest)) variables' metavariables'

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
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Just (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [] t, NormalTerm (Heading [(v, t)] (AMetavar y)) [] t), (var c t, NormalTerm (Heading [] (AMetavar f)) [var c t] t)])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x t, var u t] t] t
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y t, var v t] t] t
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Just (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [] t, NormalTerm (Heading [(v, t)] (AMetavar y)) [] t)])
-- >>> simplified == expected
-- True
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
-- [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Base "t"}]
-- >>> show' $ match (var _M t) (apply a [var (AVar b) t] t) someVariables someMetavariables
-- [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AMetavar (Metavariable "M0")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}]
-- >>> show' $ match (apply _M [var (AVar b) t] t) (var a t) someVariables someMetavariables
-- [NormalTerm {heading = Heading {binder = [(Variable "v0",Base "t")], head = AVar (Variable "a")}, arguments = [], returnType = Base "t"}]
-- >>> show' $ match (apply _M [var (AVar b) t] t) (Term [(b, t)] a [var (AVar c) t] t) someVariables someMetavariables
-- [NormalTerm {heading = Heading {binder = [(Variable "v0",Base "t"),(Variable "v1",Base "t")], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AMetavar (Metavariable "M0")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "v0")}, arguments = [], returnType = Base "t"},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "v1")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}], returnType = Base "t"},NormalTerm {heading = Heading {binder = [(Variable "v0",Base "t")], head = AVar (Variable "v0")}, arguments = [], returnType = Base "t"}]
-- >>> show' $ match (apply _M [apply (AVar b) [var (AVar a) (Function t t)] t] t) (apply a [apply (AVar b) [var (AMetavar _M) (Function t t)] t] t) someVariables someMetavariables
-- [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "a")}, arguments = [], returnType = Function (Base "t") (Base "t")},NormalTerm {heading = Heading {binder = [(Variable "v0",Base "t")], head = AVar (Variable "a")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AMetavar (Metavariable "M0")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "v0")}, arguments = [], returnType = Base "t"}], returnType = Base "t"}], returnType = Base "t"}]
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
    makeSubstitution binderTypes (const head) argumentTypes (returnType rigid)
   where
    binderTypes = ws <> vs
    ws = typeOfTerm <$> arguments flexible
    vs = snd <$> drop n_flexible (binder (heading rigid))
    argumentTypes = typeOfTerm <$> arguments rigid

  imitateDirectly head = do
    k <- [max 0 (p_flexible - p_rigid) .. p_flexible]

    let (takenFlexible, remainingFlexible) =
          splitAt k (typeOfTerm <$> arguments flexible)
    let (takenRigid, remainingRigid) =
          splitAt (p_rigid - p_flexible + k) (typeOfTerm <$> arguments rigid)
    let returnType' = foldr Function (returnType rigid) remainingRigid

    if remainingFlexible == remainingRigid
      then
        return (makeSubstitution takenFlexible (const head) takenRigid returnType')
      else
        []

  project = do
    let ws = typeOfTerm <$> arguments flexible
    let vs = snd <$> drop n_flexible (binder (heading rigid))
    let wholeBinderTypes = ws <> vs

    n_substitution <- [1 .. p_flexible + n]
    projectOn <- [0 .. max n_substitution p_flexible - 1]

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
