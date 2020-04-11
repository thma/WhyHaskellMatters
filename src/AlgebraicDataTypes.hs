{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible instances are covered in pattern matching
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-} -- allows automatic deriving of Functor and Foldable
module AlgebraicDataTypes where

import Control.Monad ((>=>))

-- a simple sum type
data Status = Green | Yellow | Red  deriving (Eq, Ord, Show, Read)
data Severity = Low | Middle | High deriving (Eq, Ord, Show, Read)

severity :: Status -> Severity
severity Green  = Low
severity Yellow = Middle
severity Red    = High

data StatusSeverityTuple = SST Status Severity deriving (Show, Read)

data PairStatusSeverity = PSS Status Severity deriving (Show, Read)

-- a simple product type
data Pair = P Status Severity --deriving (Show)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Read, Functor, Foldable)

--data Maybe a = Just a | Nothing deriving (Eq, Show)

findValue :: String -> [(String, a)] -> Maybe a
findValue = lookup

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

safeRoot :: (Ord a, Floating a) => a -> Maybe a
safeRoot x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)

findDivRoot :: Double -> String -> [(String, Double)] -> Maybe Double
findDivRoot x key map =
  case findValue key map of
      Nothing -> Nothing
      Just y  -> case safeDiv x y of
          Nothing -> Nothing
          Just d  -> case safeRoot d of
              Nothing -> Nothing
              Just r  -> Just r

findDivRoot' x key map =
  findValue key map >>= \y ->
  safeDiv x y       >>= \d ->
  safeRoot d

findDivRoot'' x key map = 
  findValue key map >>=
  (safeDiv x >=>
  safeRoot)
  
findDivRoot''' x key map = do
  y <- findValue key map
  d <- safeDiv x y
  safeRoot d
  
