{-# LANGUAGE ImportQualifiedPost #-}

module Language.Lambda.FCU.Parse where

import Control.Applicative qualified as Raw
import Language.Lambda.FCU.FCUImplSW.Terms (Term (..))
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.FCUSyntax.ErrM qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Layout qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Lex qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Par qualified as Raw
import Language.Lambda.FCU.FCUSyntax.Print qualified as Raw

-- | Parses a term from a string
parseTerm :: String -> Either String Term
parseTerm input = case Raw.pTerm . Raw.myLexer $ input of
  Raw.Bad err -> Left $ "Parse error: " ++ err
  Raw.Ok term -> Right $ convertTerm term

-- | Converts the raw parsed term to the Haskell representation
convertTerm :: Raw.Term -> Term
convertTerm term = case term of
  Raw.WTerm (Raw.MetavarId x) -> W x
  Raw.OTerm (Raw.Id x) -> O x
  Raw.CTerm (Raw.ConstructorId x) -> Constructor x
  Raw.AppTerm t1 t2 -> convertTerm t1 :@ convertTerm t2
  Raw.AbsTerm (Raw.PatternVar (Raw.Id x)) (Raw.ScopedTerm t) -> x :.: convertTerm t

-- >>> parseTerm "( f a ) ( λ x . λ y . y )"
-- Right (f a) (λx . (λy . (y)))
