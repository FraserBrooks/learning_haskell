
import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

example1 = Circle (Point 10 20) 2

example2 = map (Circle (Point 10 20)) [4,5,6,7]

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2-x1) * (abs $ x2-x1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
  = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

example3 = nudge (baseRect 30 40) 3 7

-- Record syntax:

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavour :: String
                     } deriving (Show)

guy = Person "John" "Smith" 33 6.2 "0442940320" "vanilla"

example4 = firstName guy
-- "John"

example5 = age guy
-- 33


-- Using the record syntax automatically defines lookup functions
-- and also changes the way `Show` displays:

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

example6 = Car {year=1967, model="Mustang", company="Ford"}
-- if we use record syntax we can supply the parameters in any order

-- Type Parameters
-- data Maybe a = Nothing | Just a


data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vectMult` (Vector l m n) = Vector (i*l) (j*m) (k*n)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*m


-- Derived instances:

-- a typeclass is a sort of interface that defines some behaviour
-- a type can be made an instance of a typeclass if it supports that
-- behaviour. For example, data types that support the Eq typeclass
-- can be equated because they have implementations for (==) & (/=).

-- Haskell can automatically make our type an instance of any of the
-- following classes: Eq, Ord, Enum, Bounded, Show, and Read. This
-- is done with the keyword `deriving`:

data Person' = Person' { firstName' :: String
                       , lastName' :: String
                       , age' :: Int
                       } deriving (Eq)


-- haskell will first see if the value constructors match (only one here)
-- then will check all the data contained inside matches by testing each
-- pair of fields with (==). This means that all the fields need to be
-- part of the Eq typeclass as well for haskell to derive Eq.

personA = Person' {firstName' = "John", lastName' = "Smith", age' = 33}
personB = Person' {firstName' = "Sally", lastName' = "Jenkins", age' = 22}
personC = Person' {firstName' = "Susan", lastName' = "Palmer", age' = 45}

people = [personA, personB, personC]

example7 = personA `elem` people
-- true

-- The Show and Read typeclasses are for things that can be converted
-- to and from Strings. They can be derived under the same circumstances
-- as Eq: Ie. for haskell to derive the correct behaviour all the fields
-- must be of the Show or Read typeclass:

data Monster = Monster { name :: String
                       , health :: Int
                       , strength :: Int
                       } deriving (Eq, Show, Read)

wumpus = Monster {name = "Wumpus", health = 100, strength = 3}

owlbearString = "Monster {name =\"OwlBear\", health = 120, strength = 8}"
owlbear = read owlbearString :: Monster

t = read "Just 't'" :: Maybe Char



-- We can easily use algebraic data types to make enumerations and the Enum
-- and Bounded typeclasses help us with that:

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


wednesday = read "Wednesday" :: Day
thursday = Thursday

example8 = wednesday < thursday
-- true

-- being a part of `Bounded` we can also get the 'lowest' and the
-- 'highest' day:
example9 = minBound :: Day
-- Monday
example10 = maxBound :: Day
-- Sunday
example11 = maxBound :: Int
-- 9223372036854775807

-- being part of `Enum` we can also get the predecessors and successors:
example12 = succ Monday
-- Tuesday
example13 = pred Saturday
-- Friday
example14 = [Monday .. Sunday]


-- Type Synonyms

-- [Char] and String are interchangeable. This is a type synonym. We can
-- define our own with the `type` keyword:

-- type String = [Char]

type Nos = [Int]

type PhoneBook' = [(String, String)]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- now we can write clearer type declarations:

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Be careful not to go overboard with type synonyms. Only
-- use them to describe what some existing type represents in our
-- functions (and thus our type declarations become better documentation)

-- type synonyms can also be parameterized:
type AssocList k v = [(k,v)]

-- now a function that gets the value by a key in an association list
-- can have a type of:
--    (Eq k) => k -> AssocList k v -> Maybe v


-- just like we can partially apply functions to get new functions,
-- we can partially apply type parameters and get new type constructors:

type IntMap v = Map.Map Int v
-- v is the type of value in the map


-- another cool data type:

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

example15 = Right 20
-- Right 20
example16 = Left "w00t"
-- Left "w00t"

-- Right 'a' :: Either a Char
-- The idea with `Either` is we pattern match on both the Left and Right
-- and we do different stuff for each. For example we use the Maybe type
-- when an operation could fail but `Nothing` doesn't tell us anything about
-- why. This is alright when there is only one possible reason for failure,
-- but if we want more information we can use an `Either` where the `a` is some
-- type that tells us about the failure and the `b` is the result.
-- So errors can use the `Left` type constructor while the results use `Right`.
--
-- An example:

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"







