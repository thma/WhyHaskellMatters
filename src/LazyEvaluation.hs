module LazyEvaluation where

import Data.Natural

-- it's possible to define non-terminating expressions like
viciousCircle :: a
viciousCircle = viciousCircle

-- this expression returns True because of lazy evaluation:
test = (4 == 4) || viciousCircle

ignoreY :: Integer -> Integer -> Integer
ignoreY x y = x

-- arithmetic sequences
-- all natural numbers
naturalNumbers = [0..]

-- all even numbers
evens = [2,4..]

-- all odd numbers
odds  = [1,3..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- the set of all pythagorean triples PT = {(a,b,c) | a,b,c ∊ ℕ ∧ a² + b² = c² }
pt :: [(Natural,Natural,Natural)]
pt = [(a,b,c) | c <- [2..],
                b <- [2..c-1],
                a <- [2..b-1],
                a^2 + b^2 == c^2]

-- infinite list of all prime numbers
primes :: [Integer]
primes = sieve (2:[3,5..])
  where 
    sieve (p:xs) = p:sieve [x | x <- xs, x `rem` p > 0]
    
wenn :: Bool -> b -> b -> b
wenn p x y = if p then x else y    

cond :: [(Bool, a)] -> [a]
cond []                 = []
cond ((True,  v):rest)  = v : cond rest
cond ((False, _):rest)  = cond rest