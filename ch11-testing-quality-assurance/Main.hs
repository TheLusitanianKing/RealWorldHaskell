module Main where

import QC
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck (prop_idempotent :: [Integer] -> Bool)
    -- verboseCheck (prop_idempotent :: [Integer] -> Bool)
    -- using the more general Property type here
    quickCheck (prop_minimum :: [Integer] -> Property)
    quickCheck prop_punctuate'
