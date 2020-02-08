module Functions where

import Data.Natural
import Control.Arrow ((>>>))

-- take an Int as argument and compute its square
square :: Integer -> Integer
square x = x * x

-- take a tuple of two Integers and compute their product
mul :: (Integer, Integer) -> Integer
mul (x, y) = x * y

-- add two numbers 
add :: Integer -> Integer -> Integer
add x y = x + y

-- partial application: applying add to 5 returns a function of type Integer -> Integer
add5 :: Integer -> Integer
add5 = add 5

-- combining functions with the `.` operator: (.) :: (b -> c) -> (a -> b) -> a -> c
add5AndSquare :: Integer -> Integer
add5AndSquare = square . add5

-- or by using the (>>>) operator: f >>> g = g . f
-- add5AndSquare = add5 >>> square

-- The function curry takes a function of type ((a, b) -> c) and return the equivalent curried function of type a -> b -> c
-- curry :: ((a, b) -> c) -> a -> b -> c
curMul :: Integer -> Integer -> Integer
curMul = curry mul

-- uncurry does the inverse, take the curried form and return the equivalent uncurried function
-- uncurry :: (a -> b -> c) -> (a, b) -> c
uncurAdd :: (Integer, Integer) -> Integer
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



