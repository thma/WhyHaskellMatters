module TypeClasses where

import AlgebraicDataTypes (Status (..), Severity (..), PairStatusSeverity (..), Tree (..))
import Data.Char (toUpper)

instance Num Char where
  a + b       = toEnum (fromEnum a + fromEnum b)
  a - b       = toEnum (fromEnum a - fromEnum b)
  a * b       = toEnum (fromEnum a * fromEnum b)
  abs         = id
  signum      = toEnum . signum . fromEnum
  fromInteger = toEnum . fromInteger
  negate      = id

instance Eq PairStatusSeverity where
   (PSS sta1 sev1) == (PSS sta2 sev2) = (sta1 == sta2) && (sev1 == sev2)

{--
instance Eq Status where
  Green  == Green  = True
  Yellow == Yellow = True
  Red    == Red    = True
  _      == _      = False
  
instance Eq Severity where
  Low    == Low    = True
  Middle == Middle = True
  High   == High   = True
  _      == _      = False  
  
--}

{--
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)
--}  

{--
instance Foldable Tree where
  foldr f i (Leaf a) = f a i
  foldr f i (Node a b) = foldr f (foldr f i b) a
   
--}  
  
statusTree :: Tree Status
statusTree = Node (Leaf Green) (Node (Leaf Red) (Leaf Yellow))

toSeverity :: Status -> Severity
toSeverity Green  = Low
toSeverity Yellow = Middle
toSeverity Red    = High

maxStatus = foldr max Green statusTree
maxStatus' = maximum statusTree

treeSize = length statusTree

strToUpper :: String -> String
strToUpper = map toUpper 
 
up :: IO () 
up = 
  getLine >>= \str ->
  print (strToUpper str)

up' :: IO () 
up' = do
  str <- getLine
  print (strToUpper str)

greet = do
  putStr "Please enter your first name"
  first <- getLine
  putStr "Please enter your last name"
  last <- getLine
  putStrLn ("Hello " ++ first ++ " " ++ last)
  
greet' =
  putStr "Please enter your first name" >>= \_ ->
  getLine                               >>= \first ->
  putStr "Please enter your last name"  >>= \_ ->
  getLine                               >>= \last ->
  putStrLn ("Hello " ++ first ++ " " ++ last)