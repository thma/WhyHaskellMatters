module TypeClasses where

data Color = Blue | Yellow | Red | Green | White deriving (Eq, Show)

-- Y + R = Orange
-- R + B = Violett
-- B + G = Green
-- G + Y = Yellow-Green
-- Y + Orange = Yello-Orange
-- Orange + Red = Orange-Red
-- R + Violett = Red-Violett
-- Violett + B = Blue-Violett
-- B + G = Blue-Green
-- _ + _ = Black

