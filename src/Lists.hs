module Lists where

import Prelude hiding (map, foldr, length, filter, sum)
--import qualified Prelude as P (foldr)
import Functions (square, double)

--data [a] = [] | a : [a]

-- a list of numbers to play around with
someNumbers :: [Integer]
someNumbers = [49,64,97,54,19,90,934,22,215,6,68,325,720,8082,1,33,31]

-- use arithmetic sequences to generate a list of hundred Integers
upToHundred :: [Integer]
upToHundred = [1..100]

oddsUpToHundred :: [Integer]
oddsUpToHundred = [1,3..100]

fac' :: Integer -> Integer
fac' n   = prod [1..n]

length :: [a] -> Integer
length []     =  0
length (_x:xs) =  1 + length xs

filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
  
-- filtering lists

someEvenNumbers :: [Integer]
someEvenNumbers = filter even someNumbers

someOddNumbers :: [Integer]
someOddNumbers = filter (\n -> n `rem` 2 /= 0) someNumbers  

-- compute squares for all list elements
squareAll :: [Integer] -> [Integer]
squareAll [] = []
squareAll (n:rest) = square n : squareAll rest

-- compute the double value for all list elements
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (n:rest) = double n : doubleAll rest

-- We don't want to repeat ourselves so we want something that captures the essence of mapping a function over a list:
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- now we can use map to simply declare our intention (the 'what') and don't have to explain the 'how'
squareAll' :: [Integer] -> [Integer]
squareAll' = map square


sum :: [Integer] -> Integer
sum [] = 0
sum (n:rest) = n + sum rest

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

-- making use of such abstract higher order functions
-- algorithms can be defined in a dense declarative way

len :: [a] -> Integer
len = foldr count 0
  where count _ n = n + 1

map' :: (a1 -> a2) -> [a1] -> [a2]
map' f = foldr ((:) . f) []

-- now we have map and a reduce, what about the legendary map/reduce?
-- it's called foldMap in Haskell

-- see an example here: https://github.com/thma/LtuPatternFactory#map-reduce

-- | Map each element of the list to a monoid,
-- and combine the results.
foldMap :: (Monoid m) => (a -> m) -> [a] -> m
foldMap f = foldr (mappend . f) mempty





-- building stuff based on lists

newtype Stack a = Stck [a] deriving Show

sempty :: Stack a
sempty = Stck []

pop :: Stack a -> (a, Stack a)
pop (Stck [])     = error "Stack is empty!"
pop (Stck (x:xs)) = (x, Stck xs)

push :: Stack a -> a -> Stack a
push (Stck l) x = Stck (x : l)

rev :: Stack a -> Stack a
rev (Stck xs) = Stck (reverse xs)


type Queue a = (Stack a, Stack a)

qempty :: Queue a
qempty = (sempty, sempty)

enqueue :: Queue a -> a -> Queue a
enqueue (inStack, outStack) x = (push inStack x, outStack)

dequeue :: Queue a -> (a, Queue a)
dequeue (Stck [], Stck []) = error "Queue is empty!"
dequeue (inStack, Stck []) = 
  let revertedInStack = rev inStack
      (x, restInstack) = pop revertedInStack
  in (x, (Stck [], restInstack))
dequeue (inStack, outStack) = 
  let (x, restOutStack) = pop outStack
  in (x, (inStack, restOutStack))

type Queue' a = [a]

qempty' :: Queue' a
qempty' = []

enqueue' :: Queue' a -> a -> Queue' a
enqueue' q x = x:q

dequeue' :: Queue' a -> (a, Queue' a)
dequeue' [] = error "Queue is empty!"
dequeue' xs = (last xs, init xs)


