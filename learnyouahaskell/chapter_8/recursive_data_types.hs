
import qualified Data.Map as Map

-- Recursive Data Types in Haskell

data List' a = Empty' | Cons a (List' a) deriving (Show, Read, Eq, Ord)

example1 = Empty'
-- Empty (duh)

example2 = 5 `Cons` Empty'
-- Cons 5 Empty

example3 = 4 `Cons` (5 `Cons` Empty')
-- Cons 4 (Cons 5 Empty)

-- as is clear from the code above, our definition of a List is isomorphic
-- with the standard implementation using [] and :

-- we can define functions to be automatically infix by making them comprised
-- of only special characters. We can also do the same with constructors, since
-- they're just functions that return a data type:

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- first of all notice the new optional syntactic construct, fixity declarations:
--      infixl 7 *
--      infixl 6 +
-- fixity definitions define how tightly the operator binds and whether
-- it's left-associative or right-associative. For example * binds tighter
-- than + because of its higher fixity (thus preserving bidmas/bodmas).

-- lets looks at another example by reimplementing the ++ operator for our
-- list type defined above:

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


list1 = 3 :-: 4 :-: 5 :-: Empty
list2 = 6 :-: 7 :-: Empty

example4 = list1 .++ list2
-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))

-- notice how we pattern match on (x :-: xs), we can do this because pattern
-- matching is actually about matching constructors. In fact, pattern
-- matching ONLY works on constructors. ie [] , : , etc. are all constructors.


-- Binary Search Tree:

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a l r)
    | x == a = True
    | x <  a = treeElem x l
    | x >  a = treeElem x r


nums = [6,43,15,3,44,6,7,5,23,2,5,32,33]

numsTree = foldr treeInsert EmptyTree nums

example5 = 44 `treeElem` numsTree
-- True


-- Typeclasses (again)

-- We've seen some of the standard Haskell typeclasses and we've seen which
-- types are in them. We've also learned how to automatically make our own
-- typeclasses by asking Haskell to derive the instances for us. We can of
-- course make our own typeclasses and make instances of them without using
-- derivation:

-- for example, this is how the Eq class is defined:
class Eq' a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)

-- if we have `class Eq a where...` and then define a type declaration within
-- that class like `(==) :: a -> a -> Bool` then when we examine the type of
-- that function later on, it will have the type of `(Eq a) => a -> a -> Bool`

-- so what can we do with our classes? Not much other than making type instances
-- of it:

data TrafficLight = Red | Yellow | Green
-- We could derive Eq here but we'll do it manually:

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- we use the instance keyword. class is for defining typeclasses and instance
-- is for making our types instances of typeclasses.

-- because (==) was defined in terms of (/=) via mutual recursion we only had
-- to overwrite one of them in the instance declaration. That's called the
-- minimal complete definition for the typeclass - the minimal of functions that
-- we have to implement so that our type behaves as advertised by the class.
--
-- If Eq was defined simply as:
--
--  class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--
-- we'd have to implement both of these functions when making a type an instance
-- of it because haskell wouldn't know the functions are related/inverses.
--
-- instance of Show:

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

example6 = Red == Red
-- True

example7 = [Red, Yellow]
-- [Red light, Yellow light]

-- You can also make typeclasses that are subclasses of other typeclasses:
--  ie.
--      class (Eq a) => Num a where
--        ...
--        ...
-- which is essentially saying that a type has to belong to Eq before it
-- can also belong to Num (which makes a lot of sense)

-- What about type constructors that take a parameter?:

-- instance (Eq m) => Eq (Maybe m) where
--    Just x == Just y = x == y
--    Nothing == Nothing = True
--    _ == _ = False

-- We have to add a class constraint with this instance declaration to say
-- that we want all types of the form `Maybe m` to be part of the Eq class
-- but the type of `m` must also be part of Eq.
--
-- Most of the time, class constraints in `class` declarations are used for
-- making a typeclass a subclass of another typeclass and class constraints
-- in `instance` declarations are used to express requirements about the
-- contents of some type.


-- yes-no typeclass example:

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

-- `id` is a standard library function that takes a parameter and
-- returns the same thing.

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True


example8 = yesno EmptyTree
-- False
example9 = yesno (45356435 :: Int )
-- True


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yes no = if yesno yesnoVal then yes else no

example10 = yesnoIf [] "Yeah" "No!"
-- "No!"

example11 = yesnoIf [1,2,3] "Yeah" "no"
-- "Yeah"


-- The Functor typeclass
-- for things that can be mapped over (eg. Lists)

-- the Functor typeclass is implemented like so:

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- remember the type for map:
--                    map :: (a -> b) -> [a] -> [b]
-- map is just a fmap the works only on lists

instance Functor' [] where
  fmap' = map

-- so for lists, fmap is just map.

instance Functor' Maybe where
  fmap' f (Just x) = Just (f x)
  fmap' f Nothing = Nothing

-- note that `Functor` takes a type constructor that takes one type itself, not
-- a concrete type.

example12 = fmap (++ " inside the Just") (Just "...")
-- Maybe "... inside the Just"
example13 = fmap (*2) (Just 200)
-- Just 400
example14 = fmap (*2) Nothing
-- Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

example15 = fmap (*2) EmptyTree
-- EmptyTree
example16 = fmap (*4) (foldr treeInsert EmptyTree [5,6])
-- Node 24 ( Node 20 EmptyTree EmptyTree) EmptyTree


-- we can partially apply the Either type constructor allowing us
-- to declare Either as an instance of Functor:

instance Functor' (Either a) where
  fmap' f (Right x) = Right (f x)
  fmap' f (Left x) = Left x
  -- we do nothing to the Left as otherwise both Left and Right
  -- would have to be the same type which would obviously defeat
  -- the entire point of `Either`.

-- Maps from Data.Map can also be made a functor because they hold
-- values (or not!). In the case of Map k v, fmap will map a function
-- v -> v' over a map of type Map k v and return a map of type Map k v'


instance Ord k => Functor' (Map.Map k) where
  fmap' f m = Map.fromList  $ map (\(k,v) -> (k,f v)) $ Map.toList m

example17 = fmap (++"!") (Map.fromList [(3, "Fraser"), (4, "John")])
-- fromList [(3, "Fraser!"), (4, "John!")]

-------
  -- Kinds and some type-foo

--  Type constructors take other types as parameters to eventually produce
--  concrete types, just like functions take values as parameters to produce
--  values. Just like functions they can be partially applied.
--
--  Types are labels that values carry so that we can reason about the values.
--  Types also have their own little labels, called kinds.
--
--  A kind is more or less the type of a type.
--  We can examine the kind of a type using `:k` in ghci
--
--  ghci> :k Int
--  Int :: *
--
--  A * means that the type is a concrete type.
--  What about `Maybe`? :
--
--  ghci> :k Maybe
--  Maybe :: * -> *
--  See how this looks like a function definition?
--  It means that the type constructor takes a concrete type (eg. Int)
--  and then returns a concrete type like `Maybe Int`.
--
--  Types are the labels of values and kinds are the labels of types.
--
--  ghci> :k Either
--  Either :: * -> * -> *
--  this tells us that Either takes two concrete types as one would expect.
--
--  Type constructors are curried just like functions, so we can partially
--  apply them.
--
--  ghci :k Either String
--  Either String :: * -> *
--
--  When we wanted to make Either part of the Functor typeclass we had to
--  partially apply it because Functor wants types that take only one parameter
--  while Either takes two. In other words, Functor wants types of kind * -> *
--
--  looking at Functor again:
--
--      class Functor f where
--        fmap :: (a -> b) -> f a -> f b
--
--  we see that the `f` type variable is a type that takes one concrete type
--  to produce a concrete type. We know it has to produce a concrete type because
--  it's used as the type of a value in a function twice (f a -> f b)

class Tofu t where
  tofu :: j a -> t a j

-- in the class above,
--        j a must be a concrete type therefore
--        j a has to have a kind of *
--        we assume * for a and therefore that j has kind * -> *
--        therefore, t has kind  | * -> (* -> *) -> * | so it has
--        to take a concrete type (a), a type constructor that takes
--        one concrete type (j) and produces a new concrete type. Wowza
--
-- if we make a type with a kind of | * -> (* -> *) -> * |

data Frank a b = Frank {frankField :: b a} deriving (Show)
-- kind = *{a} -> (* -> *){b} -> *

-- lets make some Frank values:

frank1 = Frank {frankField = Just "NOOT"}
-- Frank {frankField = Just "NOOT"} :: Frank [Char] Maybe

frank2 = Frank {frankField = Node 'a' EmptyTree EmptyTree}
-- Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree

-- We can see that as Frank has a type of the form `a b`, the values for
-- a and b must combine to create a concrete type.
--
-- Making Frank an instance of Tofu:

instance Tofu Frank where
  tofu x = Frank x

example18 = tofu (Just 'a') :: Frank Char Maybe
-- Frank {frankField = Just 'a'}
example19 = tofu ["HELLO"] :: Frank [Char] []
-- Frank {frankField = ["HELLO"]}


-- doesn't seem all that useful. Let's do some more:

data Barry t k p = Barry {yabba :: p, dabba :: t k}
-- say we want to make Barry an instance of Functor
--
-- Barry is of kind (* -> *) -> * -> * -> *
--
-- so to make Barry a part of Functor we have to partially
-- apply the first two type parameters so that we're left with * -> *

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

-- with the above, we just mapped the f over the first field!
-- Barry is part of Functor!


