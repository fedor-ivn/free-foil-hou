{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lambda.Config (
  -- * Config types
  Config (..),
  Problem (..),
  Solution (..),
  UnificationConstraint (..),
  fromUnificationConstraint,
  toUnificationConstraint,
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

data Config = Config
  { configLanguage :: Text
  , configFragment :: Text
  , configProblems :: [Problem]
  }
  deriving (Show, Generic)

data Problem = Problem
  { problemMetavars :: MetavarBinders
  , problemConstraints :: [UnificationConstraint]
  , problemSolutions :: [Solution]
  }
  deriving (Show, Generic)

data Solution = Solution
  { solutionName :: Text
  , solutionMetavars :: MetavarBinders -- Store the parsed metavars
  , solutionSubstitutions :: MetaSubsts'
  }
  deriving (Show, Generic)

data UnificationConstraint where
  UnificationConstraint
    :: (Distinct n)
    => NameBinderList Foil.VoidS n
    -> Foil.NameMap n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> MetaTerm Raw.MetavarIdent n Raw.Type
    -> UnificationConstraint

instance Show UnificationConstraint where
  show :: UnificationConstraint -> String
  show = Raw.printTree . fromUnificationConstraint

fromUnificationConstraint :: UnificationConstraint -> Raw.UnificationConstraint
fromUnificationConstraint (UnificationConstraint binders binderTypes lhs rhs) =
  let fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm
   in Raw.AUnificationConstraint
        (toBinders binders binderTypes)
        (fromMetaTerm' lhs)
        (fromMetaTerm' rhs)

toUnificationConstraint
  :: MetavarBinders
  -> Raw.UnificationConstraint
  -> Maybe UnificationConstraint
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
            pure (UnificationConstraint binderList binderTypes lhs' rhs')
 where
  binders = map (\(Raw.AVarBinder ident typ) -> (ident, typ)) vars
