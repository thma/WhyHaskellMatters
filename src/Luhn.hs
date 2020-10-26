module Luhn where

import Data.Char (digitToInt)
import Control.Arrow ((>>>))
import Data.Natural (Natural)

{-- From Rosetta Code:
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

divisibleBy10 :: Int -> Bool
divisibleBy10 = (0 ==) . (`mod` 10)

sumUpDigits :: [Int] -> Int
sumUpDigits = sum . map (uncurry (+) . (`divMod` 10))   -- map (uncurry (+) . (`divMod` 10)) [6,2,7,16,9,6,7,4,9,18,4] -> [6,2,7,7,9,6,7,4,9,9,4]

doubleEach2nd :: [Int] -> [Int]
doubleEach2nd = zipWith (*) (cycle [1,2])             -- zipWith (*) [6,1,7,8,9,3,7,2,9,9,4] [1,2,1,2,1,2,1,2,1,2,1] -> [6,2,7,16,9,6,7,4,9,18,4]

splitIntoDigits :: Natural -> [Int]
splitIntoDigits = reverse . map digitToInt . show                      -- toDigits 49927398716 -> [4,9,9,2,7,3,9,8,7,1,6]

luhn1 :: Natural -> Bool
luhn1 = divisibleBy10 . sumUpDigits . doubleEach2nd . splitIntoDigits

luhn2 :: Natural -> Bool
luhn2 n = divisibleBy10 (sumUpDigits (doubleEach2nd (splitIntoDigits n)))

luhn3 :: Natural -> Bool
luhn3 = splitIntoDigits  >>>
        doubleEach2nd >>>
        sumUpDigits >>>
        divisibleBy10
       
luhn4 :: Natural -> Bool
luhn4 = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) . zipWith (*) (cycle [1,2]) . map digitToInt . reverse . show       

{--
1. Reverse the order of the digits in the number.
2. Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
3. Taking the second, fourth ... and every other even digit in the reversed digits:
4. Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
5. Sum the partial sums of the even digits to form s2
6. If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.
--}
luhnTest :: Natural -> Bool
luhnTest n =
  let (odds, evens) = (oddsEvens . map digitToInt . reverse . show ) n   -- 1. and 3.
      s1 = sum odds                                                      -- 2.
      s2 = sum (map (crossSum . (* 2)) evens)                            -- 4. and 5.
  in (s1 + s2) `rem` 10 == 0                                             -- 6.
  where 
    crossSum :: Int -> Int
    crossSum = uncurry (+) . (`divMod` 10)
    
oddsEvens :: [Int] -> ([Int], [Int])
oddsEvens xs = foldr collectOddsEvens ([], []) (zip xs [1 ..])
  where
    collectOddsEvens :: (Int, Int) -> ([Int], [Int]) -> ([Int], [Int])
    collectOddsEvens (x, i) (odds, evens)
      | odd i     = (x : odds, evens)
      | otherwise = (odds, x : evens)


main :: IO ()
main = do
  let testcases = [49927398716, 49927398717, 1234567812345678, 1234567812345670]
  print $ map luhnTest testcases
  print $ map luhn1 testcases
  print $ map luhn2 testcases
  print $ map luhn3 testcases
  print $ map luhn4 testcases