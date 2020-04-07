module TypeClasses where

import AlgebraicDataTypes (Status (..), Severity (..), PairStatusSeverity (..))

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

instance Eq Status where
  