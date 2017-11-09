
module Geometry.Cuboid
  ( volume
  , area
  ) where


volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = let r = rectangleArea in r a b * 2 + r a c * 2 + r c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b



