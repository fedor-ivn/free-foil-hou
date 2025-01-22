module Language.Lambda.FCU.Parse where

import qualified Language.Lambda.FCU.FCUSyntax.Abs as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Par as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Lex as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Layout as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Print as Raw
import qualified Language.Lambda.FCU.FCUSyntax.ErrM as Raw
import Language.Lambda.FCU.Terms
import qualified Control.Applicative as Raw


-- Parses a term from a string
parseTerm :: String -> Either String Term
parseTerm input = case Raw.pTerm . Raw.myLexer $ input of
    Raw.Bad err -> Left $ "Parse error: " ++ err
    Raw.Ok term -> Right $ convertTerm term

-- Converts the raw parsed term to the Haskell representation
convertTerm :: Raw.Term -> Term
convertTerm term = case term of
    Raw.WTerm (Raw.MetavarId x) -> W x
    Raw.OTerm (Raw.Id x) -> O x
    Raw.CTerm (Raw.ConstructorId x) -> Constructor x
    Raw.AppTerm t1 t2 -> convertTerm t1 :@ convertTerm t2
    Raw.AbsTerm (Raw.Id x) t -> x :.: convertTerm t

-- >>> parseTerm "xy :@ y"
-- Right xy y
