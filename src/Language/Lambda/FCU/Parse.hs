module Language.Lambda.FCU.Parse where

import qualified Language.Lambda.FCU.FCUSyntax.Abs as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Par as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Lex as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Layout as Raw
import qualified Language.Lambda.FCU.FCUSyntax.Print as Raw
import qualified Language.Lambda.FCU.FCUSyntax.ErrM as Raw
import Language.Lambda.FCU.Terms

parseTerm :: String -> Either String Term
parseTerm input = case Raw.pTerm . Raw.myLexer $ input of
    Raw.Bad err -> Left $ "Parse error: " ++ err
    Raw.Ok term -> Right $ convertTerm term

convertTerm :: Raw.Term -> Term 
convertTerm term = case term of
    Raw.Lam (Raw.Id x) t -> x :.: convertTerm t
    Raw.App t1 t2 -> convertTerm t1 :@ convertTerm t2
    Raw.Var (Raw.VarIdent x) -> O x
    Raw.MetaVar (Raw.MetaVarIdent x) -> W x

main :: IO ()
main = do
    let input = "λx. x y"
    case parseTerm input of
        Left err -> putStrLn err
        Right term -> print term

--- >>> parseTerm "x"
-- /workspaces/haskell/lambda-free-foil/src/Language/Lambda/FCU/FCUSyntax/Par.hs:351:19: error: [GHC-88464]
--     • Data constructor not in scope:
--         Language.Lambda.FCUSyntax.Abs.Var :: VarIdent -> Term
--       NB: no module named ‘Language.Lambda.FCUSyntax.Abs’ is imported.
--     • In the first argument of ‘HappyAbsSyn15’, namely
--         ‘(Language.Lambda.FCUSyntax.Abs.Var happy_var_1)’
--       In the expression:
--         HappyAbsSyn15 (Language.Lambda.FCUSyntax.Abs.Var happy_var_1)
--       In an equation for ‘happyReduction_17’:
--           happyReduction_17 (HappyAbsSyn9 happy_var_1)
--             = HappyAbsSyn15 (Language.Lambda.FCUSyntax.Abs.Var happy_var_1)
-- (deferred type error)
