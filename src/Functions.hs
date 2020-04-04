module Functions where

import Control.Arrow ((>>>))
import Data.Natural
import Prelude hiding ((.))

-- define constant `aNumber` with a value of 42.
aNumber :: Integer
aNumber = 42

-- define constant `aString` with a value of "hello world"
aString :: String
aString = "Hello World"

-- define a function `square` which takes an Integer as argument and compute its square
square :: Integer -> Integer
square n = n ^ 2

double :: Integer -> Integer
double n = 2 * n

-- combining functions with the `.` operator: (.) :: (b -> c) -> (a -> b) -> a -> c
squareAfterDouble :: Integer -> Integer
--squareAfterDouble = square . double
squareAfterDouble n = (square . double) n

ifOddDouble :: Integer -> Integer
ifOddDouble n =
  if odd n
    then double n
    else n

ifOddSquare :: Integer -> Integer
ifOddSquare n =
  if odd n
    then square n
    else n

ifOdd :: (Integer -> Integer) -> Integer -> Integer
ifOdd growthFunction n =
  if odd n
    then growthFunction n
    else n

ifOddDouble' :: Integer -> Integer
ifOddDouble' n = ifOdd double n

ifOddSquare' :: Integer -> Integer
ifOddSquare' n = ifOdd square n

ifPredGrow :: (Integer -> Bool) -> (Integer -> Integer) -> Integer -> Integer
ifPredGrow predicate growthFunction n =
  if predicate n
    then growthFunction n
    else n

ifEvenDouble :: Integer -> Integer
ifEvenDouble n = ifPredGrow even double n

ifEvenSquare :: Integer -> Integer
ifEvenSquare n = ifPredGrow even square n

ifOddDouble'' :: Integer -> Integer
ifOddDouble'' n = ifPredGrow odd double n

ifOddSquare'' :: Integer -> Integer
ifOddSquare'' n = ifPredGrow odd square n


-- function taking a tuple of two Integers and computing their product
mul :: (Integer, Integer) -> Integer
mul (x, y) = x * y

-- function adding two numbers
add :: Integer -> Integer -> Integer
add x y = x + y

-- partial application: applying add to 5 returns a function of type Integer -> Integer
add5 :: Integer -> Integer
add5 = add 5

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

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

fac' n   = product [1..n]

