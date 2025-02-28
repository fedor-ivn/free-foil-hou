-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Lambda.FCU.FCUSyntax.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Language.Lambda.FCU.FCUSyntax.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Language.Lambda.FCU.FCUSyntax.Abs.Id where
  prt _ (Language.Lambda.FCU.FCUSyntax.Abs.Id i) = doc $ showString i
instance Print Language.Lambda.FCU.FCUSyntax.Abs.MetavarId where
  prt _ (Language.Lambda.FCU.FCUSyntax.Abs.MetavarId i) = doc $ showString i
instance Print Language.Lambda.FCU.FCUSyntax.Abs.ConstructorId where
  prt _ (Language.Lambda.FCU.FCUSyntax.Abs.ConstructorId i) = doc $ showString i
instance Print Language.Lambda.FCU.FCUSyntax.Abs.Term where
  prt i = \case
    Language.Lambda.FCU.FCUSyntax.Abs.WTerm metavarid -> prPrec i 0 (concatD [prt 0 metavarid])
    Language.Lambda.FCU.FCUSyntax.Abs.OTerm id_ -> prPrec i 0 (concatD [prt 0 id_])
    Language.Lambda.FCU.FCUSyntax.Abs.CTerm constructorid -> prPrec i 0 (concatD [prt 0 constructorid])
    Language.Lambda.FCU.FCUSyntax.Abs.AppTerm term1 term2 -> prPrec i 0 (concatD [prt 0 term1, doc (showString ":@"), prt 0 term2])
    Language.Lambda.FCU.FCUSyntax.Abs.AbsTerm pattern_ scopedterm -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString ":.:"), prt 0 scopedterm])

instance Print Language.Lambda.FCU.FCUSyntax.Abs.Pattern where
  prt i = \case
    Language.Lambda.FCU.FCUSyntax.Abs.PatternVar id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print Language.Lambda.FCU.FCUSyntax.Abs.ScopedTerm where
  prt i = \case
    Language.Lambda.FCU.FCUSyntax.Abs.ScopedTerm term -> prPrec i 0 (concatD [prt 0 term])
