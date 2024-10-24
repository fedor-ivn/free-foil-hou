{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification where

import Data.Maybe (isJust)
import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm, toTerm)
import Language.Lambda.FCU.Terms (Term (..), Id)
import Language.Lambda.FCU.Substitutions (Substitutions (..), applySubstitutions)

------- Unification ----- bvs (th (s,t)) = Q for all, (subs, S)
unify :: [Id] -> (Substitutions, (Term, Term)) -> Maybe Substitutions
unify bvs (th, (s, t)) = case (s, t) of
    (O x, O y) -> if x == y then Just (Substitutions []) else Nothing
    (x :.: s', y :.: t')
        | x == y    -> unify (bvs ++ [x]) (th, (s', t'))
        | otherwise -> Nothing
    (f :@ x, g :@ y)
        | f == g   -> unify bvs (th, (x, y))
    otherwise -> Nothing


-- >>> unify [] (Substitutions [], ("x", "x"))
-- Just (Substitutions [])

-- >>> unify [] (Substitutions [], ("x", "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "z" :.: "z"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "x" :.: "y"))
-- Just (Substitutions [])

-- >>> unify [] (Substitutions [], ("Fst" :@ "x", "Fst" :@ "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("Cons" :@ "x" :@ "y", "Cons" :@ "x" :@ "y"))
-- Just (Substitutions [])
