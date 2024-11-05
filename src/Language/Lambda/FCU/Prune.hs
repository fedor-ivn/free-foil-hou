{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Prune where

import Language.Lambda.FCU.Terms (Id, Term (..))



prune :: [Term] -> ((Id, Term), Term) -> (Id, Term)
prune tn (rho, u) = case u of
  
