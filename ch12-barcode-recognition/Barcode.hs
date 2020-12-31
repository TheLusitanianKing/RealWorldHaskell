module Barcode where

-- EAN-13 : 13-digit sequence, broken into 4 groups
-- first two : number system
-- next five : manufacturer ID
-- next five : product ID
-- last one  : check digit (for validation purpose)
-- UPC-A is the same except it uses a single digit to represent its number system

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])