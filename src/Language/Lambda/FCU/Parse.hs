module Language.Lambda.FCU.Parse where

import qualified Language.Lambda.FCU.FCUSyntax.Abs as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Par as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Lex as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Layout as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Print as Raw
import qualified Language.Lambda.FCU.FCUSyntax.ErrM as Raw
import Language.Lambda.FCU.Terms


-- Parses a term from a string
parseTerm :: String -> Either String Term
parseTerm input = case Raw.pTerm . Raw.myLexer $ input of
    Raw.Bad err -> Left $ "Parse error: " ++ err
    Raw.Ok term -> Right $ convertTerm term

-- Converts the raw parsed term to the Haskell representation
convertTerm :: Raw.Term -> Term
convertTerm term = case term of
    Raw.WTerm (Raw.Id x) -> W x
    Raw.OTerm (Raw.Id x) -> O x
    Raw.Constructor (Raw.Id x) -> Constructor x
    Raw.AppTerm t1 t2 -> convertTerm t1 :@ convertTerm t2
    Raw.CompTerm (Raw.Id x) t -> x :.: convertTerm t

-- Main function to test parsing
-- main :: IO ()
-- main = do
--     let input = "W x :@ (Constructor Foo :@ O y)"
--     case parseTerm input of
--         Left err -> putStrLn err
--         Right term -> print term
 
-- >>> parseTerm "W x :@ (Constructor Foo :@ O y)"
-- Right x Foo y
