--{-# LANGUAGE ScopedTypeVariables #-}

module Luhn where

import Data.Char (digitToInt)
import Control.Arrow ((>>>))

{--
Reverse the order of the digits in the number.
Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
Taking the second, fourth ... and every other even digit in the reversed digits:
Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
Sum the partial sums of the even digits to form s2
If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.

For example, if the trial number is 49927398716:

Reverse the digits:
  61789372994
Sum the odd digits:
  6 + 7 + 9 + 7 + 9 + 4 = 42 = s1
The even digits:
    1,  8,  3,  2,  9
  Two times each even digit:
    2, 16,  6,  4, 18
  Sum the digits of each multiplication:
    2,  7,  6,  4,  9
  Sum the last:
    2 + 7 + 6 + 4 + 9 = 28 = s2

s1 + s2 = 70 which ends in zero which means that 49927398716 passes the Luhn test

Task
Write a function/method/procedure/subroutine that will validate a number with the Luhn test, and
use it to validate the following numbers:

   49927398716
   49927398717
   1234567812345678
   1234567812345670
--}



{--
luhn :: String -> Bool
luhn = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) . zipWith (*) (cycle [1,2]) . map digitToInt . reverse
---}

luhn :: String -> Bool
luhn = 
  reverse >>> map digitToInt >>>
  zipWith (*) (cycle [1,2]) >>>
  map (uncurry (+) . (`divMod` 10)) >>>
  sum >>>
  (`mod` 10) >>>
  (0 ==)


{--
1. Reverse the order of the digits in the number.
2. Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
3. Taking the second, fourth ... and every other even digit in the reversed digits:
4. Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
5. Sum the partial sums of the even digits to form s2
6. If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.
--}
luhnTest :: String -> Bool
luhnTest n =
  let (odds, evens) = (oddsEvens . map digitToInt . reverse) n           -- 1. and 3. 
      s1 = sum odds                                                      -- 2.
      s2 = sum (map (crossSum . (* 2)) evens)                            -- 4. and 5.
   in (s1 + s2) `rem` 10 == 0                                            -- 6.

oddsEvens :: [Int] -> ([Int], [Int])
oddsEvens xs = foldr collectOddsEvens ([], []) (zip xs [1 ..])
  where
    collectOddsEvens :: (Int, Int) -> ([Int], [Int]) -> ([Int], [Int])
    collectOddsEvens (x, i) (odds, evens)
      | odd i     = (x : odds, evens)
      | otherwise = (odds, x : evens)

crossSum :: Int -> Int
crossSum = uncurry (+) . (`divMod` 10) --sum . map digitToInt . show

main :: IO ()
main = do
  let testcases = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
  print $ map luhnTest testcases
  print $ map luhn testcases