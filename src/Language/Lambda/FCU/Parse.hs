import qualified AbsSyntax as A
import qualified ParSyntax as P
import qualified LexSyntax as L

parseTerm :: String -> Either String Term
parseTerm input = case P.pTerm (L.tokens input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right term -> Right $ convertTerm term

-- Convert BNFC AST to your Term type
convertTerm :: A.Term -> Term
convertTerm (A.Lam x t) = x :.: convertTerm t
convertTerm (A.App t1 t2) = convertTerm t1 :@ convertTerm t2
convertTerm (A.Var x) = O x
convertTerm (A.MetaVar x) = W x
convertTerm (A.Cons x) = Constructor x

-- >>> parseTerm "x"
