{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.PPTerm where

import Data.String
import qualified Language.Lambda.FCU.FCUSyntax.ErrM as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Abs as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Par as Raw


showRaw :: Raw.Term -> String
showRaw term = case term of
    Raw.WTerm (Raw.MetavarId x) -> x
    Raw.OTerm (Raw.Id x) -> x
    Raw.CTerm (Raw.ConstructorId x) -> x
    Raw.AppTerm t1 t2 -> addParens $ showRaw t1 ++ " " ++ showRaw t2
    Raw.AbsTerm (Raw.Id x) t -> "λ" ++ x ++ " . " ++ showRaw t
  where
    addParens s = "(" ++ s ++ ")"

instance Show Raw.Term where
    show :: Raw.Term -> String
    show = showRaw

instance IsString Raw.Term where
    fromString :: String -> Raw.Term
    fromString s = case Raw.pTerm . Raw.myLexer $ s of
        Raw.Ok term -> term
        Raw.Bad err -> error $ "Parse error: " ++ err

-- >>> Raw.AppTerm (Raw.AppTerm (Raw.CTerm (Raw.ConstructorId "Cons")) (Raw.OTerm (Raw.Id "x"))) (Raw.OTerm (Raw.Id "y"))
-- ((Cons x) y)


-- >>> "X" :: Raw.Term
-- X
