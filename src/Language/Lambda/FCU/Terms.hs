{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Terms where

import Data.List (elemIndex)
import Data.Maybe (maybeToList)
import Data.String (IsString (..))
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.FCUSyntax.ErrM qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Par qualified as Raw

showRaw :: Raw.Term -> String
showRaw term = case term of
  Raw.WTerm (Raw.MetavarId x) -> x
  Raw.OTerm (Raw.Id x) -> x
  Raw.CTerm (Raw.ConstructorId x) -> x
  Raw.AppTerm t1 t2 -> addParens $ showRaw t1 ++ " " ++ showRaw t2
  Raw.AbsTerm (Raw.PatternVar (Raw.Id x)) (Raw.ScopedTerm t) -> "λ" ++ x ++ " . " ++ showRaw t
  where
    addParens s = "(" ++ s ++ ")"

instance IsString Raw.Term where
  fromString s = case Raw.pTerm . Raw.myLexer $ s of
    Raw.Ok term -> term
    Raw.Bad err -> error $ "Parse error: " ++ err

-- >>> Raw.AppTerm (Raw.AppTerm (Raw.CTerm (Raw.ConstructorId "Cons")) (Raw.OTerm (Raw.Id "x"))) (Raw.OTerm (Raw.Id "y"))
-- AppTerm (AppTerm (CTerm (ConstructorId "Cons")) (OTerm (Id "x"))) (OTerm (Id "y"))

-- >>> "X" :: Raw.Term
-- WTerm (MetavarId "X")

subset :: [Raw.Term] -> [Raw.Term] -> Bool
subset sm tn = all (`elem` tn) sm

-- >>> subset ["x", "y"] ["x", "y", "z"]
-- True

-- >>> subset ["x", "y", "z"] ["x", "y"]
-- False

-- | Apply a permutation to two lists of arguments
permutate :: [Raw.Id] -> [Raw.Term] -> [Raw.Term] -> [Raw.Id]
permutate zs as bs = [zs !! i | b <- bs, i <- maybeToList $ elemIndex b as]

-- >>> permutate ["z1", "z2"] ["a", "b", "c"] ["b", "a"]
-- [Id "z2",Id "z1"]

-- >>> permutate ["z1", "z2", "z3"] ["a", "b", "c"] ["b", "a", "c"]
-- [Id "z2",Id "z1",Id "z3"]

-- >>> permutate ["z1", "z2", "z3", "z4"] ["a", "b", "c"] ["b", "a", "d", "c"]
-- [Id "z2",Id "z1",Id "z3"]

newMetaVarId :: Raw.MetavarId -> Raw.MetavarId
newMetaVarId (Raw.MetavarId s) = Raw.MetavarId (s ++ "'")

-- >>> newMetaVarId (Raw.MetavarId "X")
-- MetavarId "X'"
