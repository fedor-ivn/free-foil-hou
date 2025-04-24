{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Config (
  -- * Config types
  Config (..),
  Problem (..),
  Solution (..),
  CanonicalConstraint (..),
  fromUnificationConstraint,
  toUnificationConstraint,
  CanonicalProblem,
  CanonicalSolution,
  CanonicalConfig,
  IsCanonicalConstraint (..),
  IsCanonicalSubstitutions (..),
  IsCanonicalMetavarBinders (..),
  Result,
  toCanonicalSolution,
  toCanonicalConfig,
)
where

import Control.Monad.Foil (Distinct, NameBinderList)
import qualified Control.Monad.Foil as Foil
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Lambda.Impl (
  MetaSubsts',
  MetaTerm,
  MetavarBinders,
  annotate,
  fromMetaTerm,
  fromTerm,
  getTermFromScopedTerm,
  toBinders,
  toTerm,
  withMetaSubstVars,
 )
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Print as Raw

newtype Config binders constraint solution = Config
  { configProblems :: [Problem binders constraint solution]
  }
  deriving (Show, Generic)

type CanonicalConfig =
  Config
    MetavarBinders
    CanonicalConstraint
    (Solution MetavarBinders MetaSubsts')

data Problem binders constraint solution = Problem
  { problemMetavarBinders :: binders
  , problemConstraints :: [constraint]
  , problemSolutions :: [solution]
  }
  deriving (Show, Generic)

type CanonicalProblem =
  Problem
    MetavarBinders
    CanonicalConstraint
    (Solution MetavarBinders MetaSubsts')

type CanonicalSolution = Solution MetavarBinders MetaSubsts'

data Solution binders substitutions = Solution
  { solutionName :: Maybe Text
  , solutionMetavarBinders :: binders
  , solutionSubstitutions :: substitutions
  }
  deriving (Show, Generic)

data CanonicalConstraint where
  CanonicalConstraint
    :: (Distinct n)
    => NameBinderList Foil.VoidS n
    -> Foil.NameMap n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> CanonicalConstraint

type Result = Either String

class IsCanonicalConstraint c where
  toCanonicalConstraint :: MetavarBinders -> c -> Result CanonicalConstraint
  fromCanonicalConstraint :: MetavarBinders -> CanonicalConstraint -> Result c

class IsCanonicalSubstitutions s where
  toCanonicalSubstitution :: MetavarBinders -> s -> Result MetaSubsts'
  fromCanonicalSubstitution :: MetavarBinders -> MetaSubsts' -> Result s

class IsCanonicalMetavarBinders b where
  toCanonicalMetavarBinders :: b -> Result MetavarBinders
  fromCanonicalMetavarBinders :: MetavarBinders -> Result b

instance IsCanonicalConstraint CanonicalConstraint where
  toCanonicalConstraint :: MetavarBinders -> CanonicalConstraint -> Result CanonicalConstraint
  toCanonicalConstraint _ = Right

  fromCanonicalConstraint :: MetavarBinders -> CanonicalConstraint -> Result CanonicalConstraint
  fromCanonicalConstraint _ = Right

instance IsCanonicalSubstitutions MetaSubsts' where
  toCanonicalSubstitution :: MetavarBinders -> MetaSubsts' -> Result MetaSubsts'
  toCanonicalSubstitution _ = Right

  fromCanonicalSubstitution :: MetavarBinders -> MetaSubsts' -> Result MetaSubsts'
  fromCanonicalSubstitution _ = Right

instance IsCanonicalMetavarBinders MetavarBinders where
  toCanonicalMetavarBinders :: MetavarBinders -> Result MetavarBinders
  toCanonicalMetavarBinders = Right

  fromCanonicalMetavarBinders :: MetavarBinders -> Result MetavarBinders
  fromCanonicalMetavarBinders = Right

toCanonicalSolution
  :: forall b s
   . (IsCanonicalMetavarBinders b, IsCanonicalSubstitutions s)
  => MetavarBinders
  -> Solution b s
  -> Result CanonicalSolution
toCanonicalSolution globalBinders (Solution name localBinders substs) = do
  canonicalLocalBinders <- toCanonicalMetavarBinders localBinders
  let binders = globalBinders <> canonicalLocalBinders
  canonicalSubsts <- toCanonicalSubstitution binders substs
  return $ Solution name canonicalLocalBinders canonicalSubsts

toCanonicalProblem
  :: forall b c s
   . ( IsCanonicalMetavarBinders b
     , IsCanonicalConstraint c
     , IsCanonicalSubstitutions s
     )
  => Problem b c (Solution b s)
  -> Result CanonicalProblem
toCanonicalProblem (Problem binders constraints solutions) = do
  canonicalBinders <- toCanonicalMetavarBinders binders
  canonicalConstraints <- mapM (toCanonicalConstraint canonicalBinders) constraints
  canonicalSolutions <- mapM (toCanonicalSolution canonicalBinders) solutions
  return $ Problem canonicalBinders canonicalConstraints canonicalSolutions

toCanonicalConfig
  :: ( IsCanonicalMetavarBinders b
     , IsCanonicalConstraint c
     , IsCanonicalSubstitutions s
     )
  => Config b c (Solution b s)
  -> Either String CanonicalConfig
toCanonicalConfig (Config problems) =
  Config <$> mapM toCanonicalProblem problems

instance Show CanonicalConstraint where
  show :: CanonicalConstraint -> String
  show = Raw.printTree . fromUnificationConstraint

fromUnificationConstraint :: CanonicalConstraint -> Raw.UnificationConstraint
fromUnificationConstraint (CanonicalConstraint binders binderTypes lhs rhs) =
  let fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm
   in Raw.AUnificationConstraint
        (toBinders binders binderTypes)
        (fromMetaTerm' lhs)
        (fromMetaTerm' rhs)

toUnificationConstraint
  :: MetavarBinders
  -> Raw.UnificationConstraint
  -> Maybe CanonicalConstraint
toUnificationConstraint metavarBinders (Raw.AUnificationConstraint vars lhs rhs) =
  withMetaSubstVars
    Foil.emptyScope
    Map.empty
    Foil.NameBinderListEmpty
    Foil.emptyNameMap
    binders
    $ \scope env binderList binderTypes ->
      let annotate' =
            annotate
              metavarBinders
              binderTypes
              . toTerm scope env
              . getTermFromScopedTerm
       in do
            (lhs', _) <- annotate' lhs
            (rhs', _) <- annotate' rhs
            pure (CanonicalConstraint binderList binderTypes lhs' rhs')
 where
  binders = map (\(Raw.AVarBinder ident typ) -> (ident, typ)) vars
