module LazyEvaluation where

import Data.Natural ( Natural )

-- it's possible to define non-terminating expressions like
viciousCircle :: a
viciousCircle = viciousCircle

-- this expression returns True because of lazy evaluation:
test :: Bool
test = (4 == 4) || viciousCircle

ignoreY :: Integer -> Integer -> Integer
ignoreY x y = x

-- arithmetic sequences
-- all natural numbers
naturalNumbers :: [Integer]
naturalNumbers = [0..]

-- all even numbers
evens :: [Integer]
evens = [2,4..]

-- all odd numbers
odds :: [Integer]
odds  = [1,3..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

evens' :: [Integer]
evens' = [2*n | n <- [1..]]

odds' :: [Integer]
odds' = [2*i - 1 | i <- [1..]]

squares :: [(Natural,Natural)]
squares = [(a,b) | a <- [1..],
                   b <- [1..a],
                   b^2 == a]

-- the set of all pythagorean triples PT = {(a,b,c) | a,b,c ∊ ℕ ∧ a² + b² = c² }
pt :: [(Natural,Natural,Natural)]
pt = [(a,b,c) | c <- [1..],
                b <- [1..c],
                a <- [1..b],
                a^2 + b^2 == c^2]

-- infinite list of all prime numbers
primes :: [Integer]
primes = sieve (2:[3,5..])
  where 
    sieve (p:xs) = p:sieve [x | x <- xs, x `rem` p > 0]
    
myIf :: Bool -> b -> b -> b
myIf p x y = if p then x else y    

cond :: [(Bool, a)] -> a
cond []                  = error "make sure that at least one condition is true"
cond ((True,  v):_rest)  = v
cond ((False, _):rest)   = cond rest

sign :: (Ord a, Num a) => a -> a
sign x = cond [(x > 0     , 1 )
              ,(x < 0     , -1)
              ,(otherwise , 0 )]
              
-- | An operator that allows you to write C-style ternary conditionals of
-- the form:
-- > p ? t ?? f
infixr  0 ?
(?) :: b -> (b -> a) -> a
p ? f = f p

-- |'if' with the 'Bool' argument at the end (infixr 1).
infixr  1 ??
(??) :: a -> a -> Bool -> a
(??) t f p = if p then t else f

oddOrEven :: Integral a => a -> String
oddOrEven x = (odd x) ? "odd" ?? "even" 