
import Data.List
import Data.Function (on)

numUniques:: (Eq a) => [a] -> Int
numUniques = length . nub

-- nub is a function that takes a list and weeds
-- out duplicate elements

-- to import modules into GHCI | :m + Data.List Data.Map Data.Set etc..
--  or import specific functions | import Data.List (nub, sort)
--  or a module except a few functions
--  (to avoid name clashes) | import Data.List hiding (nub)

-- can also use 'qualified imports' | import qualified Data.Map
-- which will require us to refer to Data.Map's filter function
-- via | Data.Map.filter
-- to avoid clashing with the prelude filter function

-- Data.List functions:

example1 = intersperse '.' "MONKEY"
-- "M.O.N.K.E.Y"

-- intercalate takes a list and a list of lists and inserts
-- the first list in between the other lists then flattens
example2 = intercalate " " ["Good", "show", "mate!"]
-- "Good show mate!"

-- transpose takes a list of lists and if you look at is as
-- a 2D matrix, the columns become the rows and vice versa
example3 = transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]


-- foldl' & foldr'
--    stricter versions of their respective lazy incarnations
--    lazy folds can sometimes hit a stack overflow so use these
--    instead if that happens. Apart from computing intermediate
--    values of the accumulator to avoid an overflow, these stricter
--    versions of fold work in exactly the same way.

-- concat flattens a list of lists
example4 = concat ["Foo", "Fighters"]
-- "FooFighters"

-- concatMap is the same as mapping a function to a list then
-- concatenating the list with concat:
example5 = concatMap (++ "!") ["a","b","c"]
-- "a!b!c!"


-- 'and' takes a boolean list and returns True if all are true
example6 = and $ map (>4) [5,6,7,8]
-- true
-- 'or' intuitively works the same as above except using ||

-- 'any' and 'all' take a list and a predicate and check
-- whether any or all of the elements satisfy the predicate
example7 = any (==4) [2,3,4,5,6,55]
-- true
example8 = all (>4) [6,9,10]
-- true


-- iterate takes a function and a starting value and returns
-- the results of repeated application of the function in the
-- form of an infinite list:
example9 = take 10 $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]

-- splitAt takes a number and a list and splits the list after
-- that number of elements
example10 = splitAt 3 "heythere"
-- ("hey", "there")


-- takeWhile is a really useful function. It takes elements
-- from a list while a certain predicate holds:
example11 = takeWhile (>3) [6,5,6,5,6,1,4,5,3]
-- [6,5,6,5,6]
example12 = takeWhile (/=' ') "This is a sentence"
-- "This"
example13 = sum $ takeWhile (<10000) $ map (^3) [1..]
-- 53361

-- dropWhile is similar, only it drops all elements while a
-- predicate is true:
example14 = dropWhile (/=' ') "This is also a sentence"
-- " is also a sentence"

-- span is kind of like takeWhile, only it returns a pair of lists:
example16 = let (fw, rest) = span (/=' ') "This is a sentence"
              in "First word:" ++ fw ++ ", the rest:" ++ rest
-- "First word: This, the rest: is a sentence"

-- break is like span only it breaks when the predicate is first true
-- rather than until it is false like span:
example17 = break (==4) [1,2,3,4,5,6,7]
-- ([1,2,3],[4,5,6,7])

-- sort sorts a list... obviously.
example18 = sort [3,5,6,2,65]
-- [2,3,5,6,65]

-- 'group' takes a list and groups adjacent elements into sublists
--  if they are equal:
example19 = group [1,1,1,2,2]
-- [[1,1,1],[2,2]]

-- if we sort a list before grouping it, we can find out how many times
-- each element appears in the list
example20 = map (\l@(x:xs) -> (x, length l)) . group . sort $ [1,1,1,2,2]
-- [(1,3),(2,2)]

-- inits and tails are like init and tail only they recursively apply
-- to the list until there is nothing left:
example21  = inits "w00t"
-- ["", "w", "w0", "w00", "w00t"]

-- using fold to implement searching a list for a sublist

search :: (Eq a) => [a] -> [a] -> Bool
search sbls ls =
  let sublen = length sbls
  in foldl (\acc x -> if take sublen x == sbls then True else acc)
  False (tails ls)

-- 'isInfixOf' searches for a sublist within a list in much the
-- same way as defined above
example22 = "hey" `isInfixOf` "xxxheyxxx"
-- true
example23 = "hey" `isPrefixOf` "hey....."
--true
example24 = "there" `isSuffixOf` "hey there"
--true

-- elem and notElem check if an element is or isn't in a list
example25 = 'a' `elem` "vkncsa" && 'g' `notElem` "klfdmn"
-- true

-- partition takes a list and splits it into two lists via a predicate
example26 = partition (`elem` ['A'..'Z']) "BOBistheBEST"
-- ("BOBBEST", "isthe")


-- 'find' takes a list and a predicate and returns the first element
-- that satisfies the predicate wrapped in a 'Maybe'
example27 = find (>4) [1,2,3,4,5,6,7,8]
-- Just 5

--  finding indeces:
example28 = 4 `elemIndex` [1,2,3,4,5]
-- Just 3
example29 = 10 `elemIndex` [1,2]
-- Nothing

example30 = ' ' `elemIndices` "Where are the spaces?"
-- [5,9,13]
example31 = findIndex (==4) [5,4,3]
-- Just 1
example32 = findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
-- [0,6,10,14]

-- 'zip' and 'zipWith' zip together two lists either into a tuple
--  or with a binary function. 'zip3', 'zip4', 'zipWith3', 'zipWith4'
--  all do exactly what they say:
example33 = zipWith3 (\x y z -> max x $ max y z) [1,2,3] [3,2,1] [1,3,1]


-- lines takes input strings from a file and seperates out the lines:
example34 = lines "first line\nsecond line\nthird line"
-- ["first line", "second line", "third line"]
-- 'unlines' does exactly the inverse of the above

-- 'words' and 'unwords' are for splitting a line of text into words
-- or joinging a list of words into text.

-- 'delete' takes an element and a list and deletes the first
--  occurence of that element

-- \\ is the list difference function. It acts like a set difference,
--  wherein for every element in the right-hand list it removes a matching
--  element in the left one:

example35 = [1..10] \\ [3,4,5]
-- [1,2,6,7,8,9,10]

-- union also acts like a function on sets. It returns the union of two
-- lists. It basically just goes over every element in the first list and
-- appends it to the first one if it isn't already in yet.  Duplicates are
-- removed from the second list first though!
example36 = [1,2,3] `union` [2,3,4,4]
-- [1,2,3,4]

-- intersect returns the elements that are found in both given lists:
example37 = [1..7] `intersect` [5..10]
-- [5,6,7]

-- insert takes an element and a list of elements that can be sorted and
-- inserts it into the last position where it's still less than or equal to
-- the next element
example38 = insert 4 [3,5,1,2,8,2]
-- [3,4,5,1,2,8,2]


-- length, take, drop, splitAt, !!, and replicate all work with the Int type.
-- This is mostly for historical reasons as they could use more generic Integral
-- or Num typeclasses but that would likely break a lot of existing code, that's why
-- Data.List has it's own generic versions:
--    genericLength, genericTake, genericDrop, genericSplitAt, genericIndex

example39 = let xs = [1..6] in sum xs / genericLength xs
-- 3.5  (the normal 'length' function would cause an error)

-- nub, delete, union, intersect, and group also have more general counterparts
-- that let you specify an equality function rather than using == :
--    nubBy, deleteBy, unionBy, intersectBy, groupBy
--  ie. `group` is the same as `groupBy (==)`

values = [-4.3, -2.4, -1.2, 0.4, 10.5, -3, -54]
example40 = groupBy (\x y -> (x > 0) == (y > 0)) values
-- [[-4.3, -2.4, -1.2], [0.4, 10.5], [-3.0, -54.0]]
-- the above groups consecutive positive or negative elements

-- a perhaps better way of writing this function would be to import
-- the `on` function from Data.Function:
example41 = groupBy ((==) `on` (> 0)) values

-- on is defined as follows:

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)


-- sort, insert, maximum, and minimum also have their more general equivalents.
-- the general equivalents take a function that returns LT, EQ, GT which are
-- the possible values of the Ordering type.
-- So `sort` is the same as `sortBy compare`

xs = [[5,4,5,4,4],[1,2,3],[34,5,3,3],[],[2],[2,2]]
example42 = sortBy (compare `on` length) xs
-- [[], [2], [2,2], [1,2,3], [34,5,3,3], [5,4,5,4,4]]























