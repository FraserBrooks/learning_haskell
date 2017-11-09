--
-- Chapter 6 High Order Functions

-- Curried Functions / Partially applied functions

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoByNine :: (Num a) => a -> a -> a
multTwoByNine x y = multThree 9 x y

multTwoByNine' = multThree 9
-- Two functions above are the same except the
-- prime version returns a function that accepts
-- two more parameters. Whereas the other returns
-- the result.


-- We can partially apply infix operators too:

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- be careful with (-x), use (subtract 4)
-- to partially apply subtraction as (-x)
-- is for negatives
subtractFour :: (Num a) => a -> a
subtractFour = (subtract 4)


-- High order functions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

addFour :: (Num a) => a -> a
addFour x = applyTwice (+2) x


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g x y = f y x
-- can actually be even simpler:
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

-- Map and Filter
-- map (+3) [1,5,6] is the same as:
-- [x+3| x <- [1,5,6]]
--
-- but the map function is usually easier to read

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x  = x:filter' p xs
  |otherwise = filter' p xs


-- quicksort with filter:
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quicksort (filter' (<=x) xs)
        bigger = quicksort (filter' (>x) xs)


-- thanks to haskell's laziness, even if you map something over
-- a list several times and filter it several times, it will only
-- pass over the list once

-- largest number under 100,000 that's divisible by 3829:
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- sum of all odd squares that are smaller than 10000:
sum' = sum (takeWhile (<10000)  (filter odd (map (^2) [1..])))
-- Or:
sum_list_comp = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz sequences:
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n: collatz (n `div` 2)
  | odd n = n: collatz (n*3 +1)

-- for all starting numbers between 1 and 100, how many chains
-- have length greater than 15?
numGreater = length (filter' greater (map collatz [1..100]))
    where greater ls = length ls > 15

-- Lambdas
-- lambdas are basically anonymous functions that are used because
-- we need some functions only once. To make a lambda we use \
-- which works just like the 'fun' keyword in ocaml

numGreater' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- folding:

sum2 :: (Num a) => [a] -> a
sum2 xs = foldl (\acc x -> acc + x) 0 xs
--or even more succinctly:
sum3 :: (Num a) => [a] -> a
sum3 = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if y == x then True else acc) False ys

--right fold:
mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs
-- could do map with fold left but then we'd have to append (++)
-- rather than prepend (:) which is much more inefficient

-- you can also mess about with reversing lists before or after
-- computation to get the desired result but the main concern is to
-- avoid using append

-- One big difference between the two folds is that right fold will
-- work on infinite lists whereas left fold wont

-- foldl1 & foldr1: assume starting value is accumulator
sum4 :: (Num a) => [a] -> a
sum4 = foldl1 (+)
-- can cause runtime errors though because they depend on the
-- list having at least one element

-- the power of folding! :

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)
-- obviously better implemented with pattern matching

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- alternative reverse:
reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

-- scanl & scanr - return all accumlator states in a list

-- how many elements does it take for the sum of the roots
-- of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile  (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- (using takeWhile rather than filter cause filter obviously
--   wouldn't work after 'length' with an infinite list )

-- Function Application with $
--  $ has the lowest precedence as apposed to
--  normal function application with a space

($$) :: (a -> b) -> a -> b
f $$ x = f x

--  function application with a space is left-associative
--  funcion application with $ is right associative
--    -most of the time $ is a convenience function so we
--     don't have to write so many parentheses
-- eg.
theNumberFour = sqrt $ 3 + 4 + 9 -- = sqrt (3+4+9)

-- $ also allows us to map function application over a list of functions:

applyThree = map ($ 3) [(4+), (10*), (flip subtract $ 55), (^2), sqrt]

--  Function Composition:
--    in maths: (f.g)(x) = f(g(x))
--    in haskell:
--    (.) :: (b -> c) -> (a -> b) -> a -> c
--    f . g = \x -> f (g x)

-- one of the uses for this is to make functions on the fly:

negateAll xs = map (\x -> negate (abs x)) xs
-- or:
negateAll' xs = map (negate . abs) xs

-- function composition is right associative so we can compose
-- many functions at a time: f (g (z x)) = (f . g . z)





