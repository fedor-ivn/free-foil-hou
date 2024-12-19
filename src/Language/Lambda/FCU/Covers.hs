{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.Lambda.FCU.Covers
  ( findCover
  , coverExists
  , CoverTest(..)
  , runCoverTest
  ) where

import           Data.Maybe                 (isJust)
import           Language.Lambda.FCU.RTerms (RTerm (..), toRTerm, toTerm)
import           Language.Lambda.FCU.Terms  (Term (..))

findCover :: [RTerm] -> RTerm -> Maybe RTerm
findCover params r =
  case lookup r paramVars of
    Just x -> Just x
    Nothing ->
      case r of
        RO {} -> Nothing
        RConstructor {} -> Just r
        RApp f b -> do
          fCover <- findCover params f
          bCover <- findCover params b
          return (RApp fCover bCover)
  where
    boundVars = [RO ("x" ++ show i) | i <- [1 ..]]
    paramVars = zip params boundVars

coverExists :: [RTerm] -> RTerm -> Bool
coverExists params r = isJust (findCover params r)

-- Cons x y, [y, x]
-- >>> findCover [RO "y", RO "x"] (RApp (RApp (RConstructor "Cons") (RO "x")) (RO "y"))
-- Just (RApp (RApp (RConstructor "Cons") (RO "x2")) (RO "x1"))
data CoverTest =
  CoverTest
    { coverTestParams :: [Term]
    , coverTestRHS    :: Term
    }

runCoverTest :: CoverTest -> Maybe Term
runCoverTest CoverTest {..} =
  toTerm <$> findCover (map toRTerm coverTestParams) (toRTerm coverTestRHS)

coverTest1 :: CoverTest
coverTest1 =
  CoverTest {coverTestParams = ["y", "x"], coverTestRHS = "Cons" :@ "x" :@ "y"}

-- >>> runCoverTest coverTest1
-- Just Cons (x2) (x1)
coverTest2 :: CoverTest
coverTest2 =
  CoverTest
      -- snd l, z, cons (fst x) y
    { coverTestParams = ["Snd" :@ "l", "z", "Cons" :@ ("Fst" :@ "x") :@ "y"]
      -- cons (cons (cons (fst x) y) z) (snd l)
    , coverTestRHS =
        "Cons" :@ ("Cons" :@ ("Cons" :@ ("Fst" :@ "x") :@ "y") :@ "z") :@
        ("Snd" :@ "l")
    }
-- >>> runCoverTest coverTest2
-- Just Cons (Cons (x3) (x2)) (x1)
