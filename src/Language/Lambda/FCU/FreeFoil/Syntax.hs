{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Lambda.FCU.FreeFoil.Syntax where

import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw

-- * Generated code

-- ** Signature
mkSignature ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms
mkPatternSynonyms ''TermSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.Id ''Raw.ScopedTerm ''Raw.Pattern

-- ** Scope-safe patterns

mkFoilPattern ''Raw.Id ''Raw.Pattern
deriveCoSinkable ''Raw.Id ''Raw.Pattern
mkToFoilPattern ''Raw.Id ''Raw.Pattern
mkFromFoilPattern ''Raw.Id ''Raw.Pattern

deriveUnifiablePattern ''Raw.Id ''Raw.Pattern

-- | Deriving 'GHC.Generic' and 'GenericK' instances.
deriving instance GHC.Generic (TermSig scope term)
deriveGenericK ''TermSig

-- -- | Match 'Raw.Ident' via 'Eq'.
-- instance ZipMatchK Raw.Ident where zipMatchWithK = zipMatchViaEq

-- | Ignore 'Raw.BNFC'Position' when matching terms.
instance ZipMatchK Raw.BNFC'Position where zipMatchWithK = zipMatchViaChooseLeft

-- | Generic 'ZipMatchK' instance.
instance ZipMatchK TermSig

instance ZipMatch TermSig where
  zipMatch = genericZipMatch2

-- * User-defined code

-- | Generic annotated scope-safe \(\lambda\Pi\)-terms with patterns.
type Term = AST FoilPattern TermSig

-- | Scode-safe \(\lambda\Pi\)-terms annotated with source code position.
type Term = Term Raw.BNFC'Position

-- | Scope-safe patterns annotated with source code position.
type FoilPattern = FoilPattern Raw.BNFC'Position

-- ** Conversion helpers

-- | Convert 'Raw.Term' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: Foil.Distinct n => Foil.Scope n -> Map Raw.Id (Foil.Name n) -> Raw.Term -> Term n
toTerm = convertToAST convertToTermSig toFoilPattern getTermFromScopedTerm

-- | Convert 'Raw.Term' into a closed scope-safe term.
-- This is a special case of 'toTerm'.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

-- | Convert a scope-safe representation back into 'Raw.Term'.
-- This is a special case of 'convertFromAST'.
--
-- 'Raw.Id' names are generated based on the raw identifiers in the underlying foil representation.
--
-- This function does not recover location information for variables, patterns, or scoped terms.
fromTerm :: Term n -> Raw.Term
fromTerm = convertFromAST
  convertFromTermSig
  (Raw.Var (error "location missing"))
  (fromFoilPattern mkId)
  (Raw.AScopedTerm (error "location missing"))
  mkId
  where
    mkId n = Raw.Id ("x" ++ show n)

-- | Parse scope-safe terms via raw representation.
--
-- >>> fromString "λx.λy.λx.x" :: Term Foil.VoidS
-- λ x0 . λ x1 . λ x2 . x2
instance IsString (AST FoilPattern TermSig Foil.VoidS) where
  fromString input = case Raw.pTerm (Raw.tokens input) of
    Left err   -> error ("could not parse λΠ-term: " <> input <> "\n  " <> err)
    Right term -> toTermClosed term

-- | Pretty-print scope-safe terms via raw representation.
instance Show (AST FoilPattern TermSig Foil.VoidS) where
  show = Raw.printTree . fromTerm