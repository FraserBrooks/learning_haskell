-------------------------------
--  Solving Problems Recursively

import Data.List

--
-- Heathrow to London
--
-- Finding the shortest path:

data Node = Node Road Road | EndNode Road
data Road = Road Int Node

-- A node is either a normal node and has information about the road that
-- leads to the other main road and the road that leads to the next node
-- or an end node, which only has information about the road to the other
-- main road. A road keeps information about how long it is and which
-- node it points to.
--
-- This is an alright way to represent the road system in Haskell and we
-- could solve the problem this way but there is a simpler way.
--
-- We can represent our road system as sections:

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

-- (we could also use a triple of (Int, Int, Int) to represent a road section.
-- Using tuples instead of making your own algebraic data types is good for some
-- small localized stuff, but it's usually better to make a new type for things
-- like this. It gives the type system more information. i.e we won't accidentally
-- add a Vector to a section of a road system.

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- All we need to do now is implement our solution. Our function returns a path so
-- we'll represent a path as a list as well. Let's introduce a `Label` type that's
-- just an enumeration of either A, B or C. We'll also make a type synonym: `Path`:

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- thus our function should have type: optimalPath :: RoadSystem -> Path

-- We're going to walk over the list with the sections from left to right
-- and keep the optimal path on A and the optimal path on B as we go along.
-- To do this we can use a left fold:


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA ----
      priceB = sum $ map snd pathB ----
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                      then (A,a):pathA
                      else (C,c):(B,b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                      then (B,b):pathB
                      else (C,c):(A,a):pathA
  in  (newPathToA, newPathToB)

-- Optimization tip: when we do `priceA = sum $ map snd pathA` we're calculating
-- the price from the path on every step. We wouldn't have to do that if we
-- implemented `roadStep` as:
--  `(Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)`
-- where the integers represent the best price on A and B.

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (pathA, pathB) = foldl roadStep ([],[]) roadSystem
      priceA = sum $ map snd pathA ----
      priceB = sum $ map snd pathB ----
   in if priceA <= priceB then reverse pathA else reverse pathB


-- we have the function that finds an optimal path, now we just have to
-- read a textual representation of a road system from the standard input,
-- convert it into a `RoadSystem` and feed that into our function.

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path ----
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice




