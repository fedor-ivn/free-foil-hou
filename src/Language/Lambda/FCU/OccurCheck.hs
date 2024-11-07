{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.OccurCheck where

import           Language.Lambda.FCU.Substitutions (Substitutions)
import           Language.Lambda.FCU.Terms         (Id, Term (..))

occ :: Term -> Substitutions -> Term -> Bool
occ _ _ _ = error "occ not implemented"
