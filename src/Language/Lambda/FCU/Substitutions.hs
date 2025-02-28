{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lambda.FCU.Substitutions where

import Language.Lambda.FCU.FCUImplSW.Substitutions (devarOne)
import Language.Lambda.FCU.FCUSyntax.Abs qualified as Raw
import Language.Lambda.FCU.Terms

newtype Substitutions
  = Substitutions [(String, Raw.Term)]
  deriving (Eq)

ppSubstitutions :: Substitutions -> String
ppSubstitutions (Substitutions subs) =
  "[" ++ unwords ["(" ++ show x ++ " |-> " ++ show y ++ ")" | (x, y) <- subs] ++ "]"

instance Show Substitutions where
  show :: Substitutions -> String
  show = ppSubstitutions

-- >>> Substitutions [("x", "Y")]
-- [("x" |-> Y)]

betaReduceOnce :: Raw.Term -> Raw.Term
betaReduceOnce term =
  case term of
    Raw.AppTerm (Raw.AbsTerm (Raw.Id x) s) t -> betaReduceOnce (devar (Substitutions [(x, t)]) s)
    Raw.AppTerm s t -> Raw.AppTerm (betaReduceOnce s) (betaReduceOnce t)
    Raw.AbsTerm x s -> Raw.AbsTerm x (betaReduceOnce s)
    _ -> term

-- >>> betaReduceOnce (" ( x :.: y ) :@ z")
-- y

-- >>> betaReduceOnce ("( ( x :.: y ) :@ z ) :@ w")
-- (y w)

-- >>> betaReduceOnce (" ( ( x :.: x ) :@ z) :@ w")
-- (z w)

-- λz1 . (λz2 . (Snd ((λz1 . (λz2 . (Y' z2))) (Fst l2)) (z1)))

betaReduce :: Raw.Term -> Raw.Term
betaReduce term =
  let term' = betaReduceOnce term
   in if term == term' then term else betaReduce term'

-- >>> betaReduce ("z1 :.: (z2 :.: ( Snd :@ ( ( ( k1 :.: (k2 :.: ( Y :@ k2 ) ) ) :@ ( Fst :@ l2 ) ) :@ z1 ) ) )")
-- λz1 . λz2 . (Snd (Y z1))

applySubstitutions :: Substitutions -> Raw.Term -> Raw.Term
applySubstitutions (Substitutions th) s = case s of
  Raw.AppTerm s1 s2 -> Raw.AppTerm (applySubstitutions (Substitutions th) s1) (applySubstitutions (Substitutions th) s2)
  Raw.AbsTerm x s1 -> Raw.AbsTerm x (applySubstitutions (Substitutions th) s1)
  Raw.OTerm (Raw.Id x) -> case lookup x th of
    Just t -> t
    Nothing -> Raw.OTerm (Raw.Id x)
  Raw.WTerm (Raw.MetavarId x) -> case lookup x th of
    Just t -> t
    Nothing -> Raw.WTerm (Raw.MetavarId x)
  Raw.CTerm (Raw.ConstructorId x) -> Raw.CTerm (Raw.ConstructorId x)

-- >>> applySubstitutions (Substitutions [("x", "y")]) ("x")
-- y

devar :: Substitutions -> Raw.Term -> Raw.Term
devar th s = betaReduce (applySubstitutions th s)

-- >>> devar (Substitutions [("x", "y")]) ("x")
-- y

-- >>> devar (Substitutions[("x", "y")]) ("x :@ z :@ x")
-- (y (z y))

rename :: Raw.Id -> Raw.Id -> Raw.Term -> Raw.Term
rename (Raw.Id x) y s = devar (Substitutions [(x, Raw.OTerm y)]) s

-- >>> rename "x" "y" "x :@ z"
-- (y z)

-- >>> rename "x" "y" "x :.: z"
-- λx . z

mkvars :: [Raw.Term] -> [Raw.Id]
mkvars sn = [Raw.Id ("z" ++ show i) | i <- [1 .. length sn]]

-- >>> mkvars ["x", "y"]
-- [Id "z1",Id "z2"]

-- >>> mkvars []
-- []

combineSubstitutions :: Substitutions -> Substitutions -> Substitutions
combineSubstitutions (Substitutions s1) (Substitutions s2) = Substitutions (s1 ++ s2)
