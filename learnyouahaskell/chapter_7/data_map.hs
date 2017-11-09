
import Data.List
import Data.Function (on)
import Data.Char
import qualified Data.Map as M

-- Association lists/ dictionaries: lists that store key value pairs

-- most obvious way to represent them in haskell is to use
-- a list of pairs:

phoneBook :: [(String, String)]
phoneBook =
  [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
  ]



findKey' :: (Eq k) => k -> [(k,v)] -> v
findKey' key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-- this will crash at runtime if the key we search for isn't in
-- the dictionary. A better solution is below:


findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key [] = Nothing
findKey'' key ((k,v):xs) = if key == k then Just v else findKey'' key xs


-- recursion patterns on lists like above are usually better written
-- using a fold as its more concise and easier to read:

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- the function implemented above is the lookup function from the Data.List
-- module. If we want to find a value to a key, we have to traverse all the
-- elements of the list until we find it. The Data.Map module offers association
-- lists that are much faster because they are implemented with trees.

-- the fromList function takes a list and returns a map with the same associations:
example1 = M.fromList phoneBook
-- if there are duplicate keys, the duplicates are just discarded
-- one difference between Data.Map implementation and association lists
-- is that the lists only require elements be equatable (belonging to Eq)
-- but Data.Map maps require the keys be orderable (belonging to Ord). This
-- is so the Data.Map can arrange the keys in a tree data structure.


-- empty represents an empty map:
example2 = M.empty
-- fromList []

-- insert takes a key, a value, and a map, and returns a new map
example3 = M.insert 3 100 M.empty
-- fromList [(3,100)]

-- we can implement our own fromList by using the empty map, insert, and a fold:

fromList' :: (Ord k) => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty


-- null checks if a map is empty:
example4 = M.null M.empty
-- True


-- size reports the size of a map:
example5 = M.size M.empty
-- 0


-- singleton takes a key and a value and creates a map that has exactly one mapping:
example6 = M.singleton 3 9
-- fromList [(3,9)]

-- lookup works like Data.List lookup only it works on maps:
example7 = M.lookup 3 $ M.fromList [(4,5), (3,4) , (55,3)]
-- Just 4

-- member is a predicate that takes a key and a map:
example8 = M.member 3 $ M.fromList [(4,5), (3,4) , (55,3)]
-- True


-- map and filter work much like their list equivalents:
example9 = M.map (*100) $ M.fromList [(4,5), (3,4), (55,3)]
-- fromList [(4,500), (3,400), (55,300)]
example10 = M.filter isUpper $ M.fromList [(1, 'a'), (2, 'A'), (3, 'b')]
-- fromList [(2, 'A')]

-- toList is the inverse of fromList

-- keys, and elems returns the key and values respectively in a list
-- `keys` is the same as `map fst . M.toList`
-- `elems` is the same as `map snd . M.toList`

-- fromListWith acts like fromList but takes a function to decide what
-- to do with duplicate keys:

phoneBook' =
  [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
  ]

phoneBookToMap :: (Ord k) => [(k,String)] -> M.Map k String
phoneBookToMap = M.fromListWith (\n1 n2 -> n1 ++ ", " ++ n2)
example11 = phoneBookToMap phoneBook'

-- insertWith works in much the same way. It takes a function that
-- will determine what is done if the key already exists































