module Lists where

import Prelude hiding (map, foldr)
import qualified Prelude as P (foldr)
import Functions (square)

-- a list of numbers to play around with
someNumbers :: [Integer]
someNumbers = [49,64,97,54,19,90,934,22,215,6,68,325,720,8082,1,33,31]

-- compute squares for all list elements
squareAll :: [Integer] -> [Integer]
squareAll [] = []
squareAll (n:rest) = square n : squareAll rest

tripleAll :: [Integer] -> [Integer]
tripleAll [] = []
tripleAll (n:rest) = (\i -> i*i*i) n : squareAll rest

-- We don't want to repeat ourselves so we want something that captures the essence of mapping a function over a list:
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- now we can use map to simply declare our intention (the 'what') and don't have to explain the 'how'
squareAll' :: [Integer] -> [Integer]
squareAll' = map square


sumUp :: [Integer] -> Integer
sumUp [] = 0
sumUp (n:rest) = n + sumUp rest

prod :: [Integer] -> Integer
prod [] = 1
prod (n:rest) = n * prod rest

-- get the drift? again we are looking for the essence of these algorithms.

-- | 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

sumUp' :: [Integer] -> Integer
sumUp' = foldr (+) 0

prod' :: [Integer] -> Integer
prod' = foldr (*) 1

-- now we have map and a reduce, what about the legendary map/reduce?
-- it's called foldMap in Haskell

-- see an example here: https://github.com/thma/LtuPatternFactory#map-reduce

-- | Map each element of the list to a monoid,
-- and combine the results.
foldMap :: (Monoid m) => (a -> m) -> [a] -> m
foldMap f = foldr (mappend . f) mempty

