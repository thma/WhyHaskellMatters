module LazyEvaluation where

import Data.Natural

-- it's possible to define non-terminating expressions like
viciousCircle :: a
viciousCircle = viciousCircle

-- this expression returns True because of lazy evaluation:
test = (4 == 4) || viciousCircle


-- list comprehension
-- all natural numbers
naturalNumbers = [0..]

-- all even numbers
evens = [2,4..]

-- all odd numbers
odds  = [1,3..]


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
    
    
    



