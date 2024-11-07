{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Prune where

import Language.Lambda.FCU.Terms (Id, Term (..))
import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm)
import Language.Lambda.FCU.Substitutions (Substitutions (..), devar)
import Language.Lambda.FCU.Strip (strip)
import Language.Lambda.FCU.Covers (coverExists)
import Data.List (elemIndex)

foldlNTerms :: (([(Id, Term)], Term) -> [(Id, Term)]) -> ([(Id, Term)], [Term]) -> [(Id, Term)]
foldlNTerms _ (rho, []) = rho
foldlNTerms f (rho, t:ts) = foldlNTerms f (f (rho, t), ts)

prune :: [Term] -> ([(Id, Term)], Term) -> [(Id, Term)]
prune tn (rho, u) = case strip (devar rho u) of
    (x :.: t', _) -> prune (O x : tn) (rho, t')
    (Constructor _, rr) -> foldlNTerms (prune tn) (rho, rr)
    (O x, rr) -> if O x `elem` tn
        then foldlNTerms (prune tn) (rho, rr)
        else error "Not unifiable"
    (W _W, sm) -> case termsDiff tn sm of
        [] -> rho
        qs -> rho ++ map (replaceQ _W rho tn sm) qs
    _ -> rho


replaceQ :: Id -> [(Id, Term)] -> [Term] -> [Term] -> Term -> (Id, Term)
replaceQ _W rho tn sm q = if coverExists (map toRTerm tn) (toRTerm (devar rho q))
    then (_W, substitution)
    else error ("Not unifiable " ++ show q)
    where
        lambdaVars = ["z" ++ show i | i <- [1 ..]]
        bodyVars = [O ("z" ++ show i) | i <- [1 ..], i/= indexQ]
        indexQ = case elemIndex q sm of
            Just idx -> idx
            Nothing -> error ("q not found in sm " ++ show sm ++ show q)
        lambdaArgs = take (length sm) lambdaVars
        newTerm = W (_W ++ "'")
        body = foldl1 (:@) (newTerm : [z | (i, z) <- zip [0..] bodyVars, i /= indexQ])
        substitution = foldr (:.:) body lambdaArgs

termsDiff :: [Term] -> [Term] -> [Term]
termsDiff tn = filter (`notElem` tn)

-- >>> prune ["x", "y"] ([], "Cons" :@ "x" :@ "y")
-- []

-- >>> prune ["Snd" :@ "x", "q", "y"] ([], "Cons" :@ ("X" :@ "x" :@ "q") :@ "y")
-- Not unifiable x
