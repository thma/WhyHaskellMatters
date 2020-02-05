module Functions where

import Data.Natural

-- take an Int as argument and compute its square
square :: Int -> Int
square x = x * x

-- take a tuple of two Ints and compute their product
mul :: (Int, Int) -> Int
mul (x, y) = x * y

-- add two numbers 
add :: Int -> Int -> Int
add x y = x + y

-- partial application: applying add to 5 returns a function of type Int -> Int
add5 :: Int -> Int
add5 = add 5

-- The function curry takes a function of type ((a, b) -> c) and return the equivalent curried function of type a -> b -> c
-- curry :: ((a, b) -> c) -> a -> b -> c
curMul :: Int -> Int -> Int
curMul = curry mul

-- uncurry does the inverse, take the curried form and return the equivalent uncurried function
-- uncurry :: (a -> b -> c) -> (a, b) -> c
uncurAdd :: (Int, Int) -> Int
uncurAdd = uncurry add

-- recursive definition of factorial 
factorial :: Natural -> Natural
factorial n = 
  if n == 0
    then 1
    else n * factorial (n - 1)

-- definition of factorial using pattern matching
fac :: Natural -> Natural
fac 0 = 1
fac n = n * fac (n - 1)