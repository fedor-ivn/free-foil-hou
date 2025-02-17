module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Language/Lambda/Impl.hs"]
