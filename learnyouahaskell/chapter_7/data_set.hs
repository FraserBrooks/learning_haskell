
import Data.List
import Data.Char
import qualified Data.Set as Set

-- The Data.Set module offers us mathematical sets wherein every element
-- is unique. Sets are kind of like a cross between lists and maps. The
-- Data.Set module implements sets more efficiently with trees rather than
-- the most obvious list based implementation. Just like with Data.Map this
-- means the keys must belong to the Ord typeclass


text1 = "bdfgnslefinseoligaeifg"
text2 = "FKnlew33£t3tgslkefnsale%^&"

-- fromList:
set1 = Set.fromList text1
set2 = Set.fromList text2


-- intersection: which elements they both share:
example1 = Set.intersection set1 set2
-- fromList "aefglns"

-- difference: which are only in the first:
example2 = Set.difference set1 set2

-- union: all unique letters in both sentences:
example3 = Set.union set1 set2


-- other functions work in much the same way as the Data.Map functions:
--    null
--    size
--    member
--    empty
--    singleton
--    insert
--    delete


example4 = Set.null Set.empty
--True

example5 = Set.size $ Set.fromList [1,2,2,2,2]
-- 2

-- we can also check for subsets/ proper subsets (not just identical):

example6 = Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- True
example7 = Set.fromList [2,3,4] `Set.isProperSubsetOf` Set.fromList [2,3,4]
-- False



-- Sets are often used to weed out duplicate elements by converting from and
-- then back to a list. The Data.List function nub already does that but weeding
-- out duplicates is much faster for large lists if you use sets; though this
-- does require the members be of the Ord typeclass whereas nub only requires
-- equality)

-- `setNub` is generally faster than nub on big lists but it won't preserve
-- the ordering of the list while Data.List nub does:

setNub :: (Ord k) => [k] -> [k]
setNub = Set.toList . Set.fromList











