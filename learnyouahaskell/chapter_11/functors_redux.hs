---------------------------------
  -- Chapter 11 Functors

-- Haskell's combination of purity, high order functions, parameterised algebraic
-- types, and typeclasses allows us to implement polymorphism on a much higher level
-- than possible in other languages. We don't have to think about types belonging
-- to a big hierarchy of types. Instead, we only concern ourselves with what the
-- types can act like and then connect them with the appropriate typeclasses. An
-- `Int` can act like a lot of things. It can act like an equatable thing, like
-- an ordered thing, etc.
--
-- Typeclasses are open, which means that we can define our own data type, think
-- about what it can act like and connect it with the typeclasses that define its
-- behaviours. Because of that and because of Haskell's great type system that
-- allows us to know a lot about a function just by looking at its type declaration,
-- we can define typeclasses that define define behaviour that's very general and
-- abstract.
--
-- We've met typeclasses that define operations for seeing if two things are equal
-- or comparing two things by some ordering. Those are very abstract and elegant
-- behaviours, but we just don't think of them as anything very special because
-- we've been dealing with them for most of our lives.
--
-- We recently met functors, which are basically things that can be mapped over.
--
-- That's an example of a useful and yet still pretty abstract property that a
-- typeclass can describe. In this chapter, we'll take a closer look at functors,
-- along with slightly stronger and more useful versions of functors called
-- applicative functors. We'll also look at monoids.

-- FUNCTORS:
--
-- = something that can be mapped over, like lists, Maybe's, trees, and such
-- They are described by the typeclass `Functor`, which has only one typeclass
-- method, namely `fmap`, which has a type of: `fmap::(a->b)->f a->f b
-- Which says, give me a function that takes an `a` and returns a `b` and a
-- box with an `a` (or several) inside it and I'll give you a box with a `b`
-- (or several of them) inside it. It kind of applies the function to the element
-- inside the box. (The box analogy isn't actually all that accurate, it's just
-- a starting point)
--
-- If we want to make a type constructor an instance of `Functor`, it has to
-- have a kind of `* -> *` which means that it has to take exactly one
-- concrete type as a type parameter. For example, `Maybe` can be made an
-- instance because it takes one type parameter to produce a concrete type,
-- like `Maybe Int` or `Maybe String`. If a type constructor takes two
-- parameters, like `Either`, we have to partially apply the type constructor
-- until it only takes one type parameter. So we can't write
--                  `instance Functor Either where`
-- but we can write:
--                  `instance Functor (Either a) where`
-- and then if we imagine that `fmap` is only for `Either a`, it would have a
-- type declaration of:
--                     `fmap :: (b -> c) -> Either a b -> Either a c`
-- As you can see, the `Either a` part is fixed, because Either a takes only
-- one type parameter, whereas just `Either` takes two so
--                      `fmap :: (b -> c) -> Either b -> Either c`
-- wouldn't really make sense.

-- Lots of type constructors are instances of `Functor`, like [], Maybe, Either a
-- and a Tree type that we made our own.
--
-- `IO` and `(->) r` are also instances of `Functor`
--
-- If some value has a type of, say, IO String, that means that it's an I/O
-- action that, when performed, will go out into the real world and get some
-- string for us, which it will yield as a result. We can use `<-` in `do`
-- syntax to bind that result to a name. We mentioned that IO actions are like
-- boxes with little feet that go out and fetch some value from the outside world
-- for us. We can inspect what they fetched, but after inspecting, we have to wrap
-- the value back in `IO`. By thinking about this box with little feet analogy,
-- we can see how `IO` acts like a functor.
--
-- Let's see how `IO` is an instance of `Functor`. When we `fmap` a function over
-- an IO action, we want to get back an IO action that does the same thing, but
-- has our function applied over its result value.

instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)

-- The result of mapping something over an IO action will be an IO action, so right
-- off the bat we use `do` syntax to glue two actions and make a new one. In the
-- implementation for `fmap`, we take a new IO action that first performs the
-- original IO action and calls its result `result`. Then, we return `f result`.
--
-- (Remember `return` is a function that makes an IO action that doesn't do
-- anything but only presents something as its result. The action that a `do`
-- block produces will always have the result value of its last action. That's
-- why we use return to make an IO action that doesn't really do anything but
-- present `f result` as the result of the new IO action. )
--
-- We can play around with this new concept:

main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "You really did say " ++ line' ++ " backwards!"

-- rewrite using `fmap` :

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "You really did say " ++ line' ++ " backwards!"

-- Just like when we `fmap reverse` over `Just "blah"` to get `Just "halb"`, we
-- can `fmap reverse` over `getLine` because getLine is an IO action of type
-- `IO String`.
--
-- The IO action `fmap (++"!") getLine` will always append "!" to whatever our
-- getLine IOAction retrieves.
--
-- If we look at what `fmap`s type would be if it were limited to `IO`, it would
-- be: `fmap :: (a -> b) -> IO a -> IO b`
--
-- If you ever find yourself binding the result of an IO action to a name, only
-- to apply a function to that and call something else, consider using fmap.




-- Another instance of `Functor` that we've been dealing with is `(->) r`
-- The function type `r -> a` can be rewritten as `(->) r a`.
-- When we look at it in this way we can see (->) in a different light,
-- because we see that it's just a type constructor that takes two type
-- parameters, just like `Either`. But remember, a type constructor has to
-- take exactly one type parameter so that it can be made an instance of
-- `Functor`. That's why we can't make (->) an instance of `Functor`, but if
-- we partially apply it to `(->) r`, it doesn't pose any problems.
-- If the syntax allowed for type constructors to be partially applied with
-- sections (like we can partially apply + by doing `(2+)`, which is the same as
-- doing `(+) 2`), we could write `(->) r` as `(r ->)`.
--
-- How are functions functors?
-- Well, let's take a look at the implementation, which lies in
-- `Control.Monad.Instances`. (We usually mark functions that take anything
-- and return anything as `a -> b`. `r -> a` is the same thing, we just use
-- different letters for the type variables.)

import Control.Monad.Instances

instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

-- First of all, let's think about `fmap`s type. It's:
--
--          `fmap :: (a -> b) -> f a -> f b`
--
-- Now what we'll do is mentally replace all the `f`s which are the role
-- that our functor instance plays, with `(->) r`s. We'll do that to see
-- how fmap should behave for this particular instance. We get:
--
--          `fmap :: (a -> b) -> (r -> a) -> (r -> b)`
--
-- So mapping one function over a function has to produce a function just like
-- mapping over an IO action has to produce an IO action. That makes sense.
-- So it takes a function from `a` to `b` and a function from `r` to `a` and
-- returns a function from `r` to `b`. If we think about it, this is simply
-- function composition! Another way to write the above would be:

instance Functor ((->) r) where
  fmap = (.)

-- This makes obvious the revalation that using `fmap` over functions is just
-- composition.
--
-- We can call `fmap` as an infix function so that the resemblance to (.) is
-- more clear:

example1 = (*3) `fmap` (+100) $ 1
-- 303
example2 = (*3) . (+100) $ 1
-- 303

-- In summary we can start to throw away our box analogy and think of Functors
-- as a computation result that can be mapped over with a function to produce
-- the same computation result but the result of that computation is modified
-- with the function.



-- Just like we can partially apply all functions. We can also partially apply
-- fmap. fmap takes one function and returns a function that takes a functor
-- and returns a functor (remember curried function system at play in Haskell).
--
-- So we can provide an `a -> b` function and get back a function `f a -> f b`
-- where `f` is of course bounded by `(Functor f) =>`.
-- This is called LIFTING a function.
--
-- i.e. the expression `fmap (*2)` is a function that takes a functor `f`
-- over numbers and returns a functor over numbers.


-- Next up we're going to look at the FUNCTOR LAWS. In order for something to
-- be a functor, it should satisfy some laws. All functors are expected to
-- exhibit certain kinds of functor-like properties and behaviours.
-- Calling `fmap` on a functor should just map a function over the functor,
-- nothing more. This behaviour is described in the functor laws. There are
-- two of them that all instances of `Functor` should abide by. They aren't
-- enforced by Haskell automatically though.
--
-- FUNCTOR LAW 1:
-- if we map the id function over a functor, we should get back the original
-- functor. i.e. fmap id = id. Remember `id` is the identity function, which
-- just returns its parameter unmodified. It can also be written as `\x -> x`
--
-- FUNCTOR LAW 2:
-- composing two functions and then mapping the resulting function over a
-- functor should be the same as first mapping one function over the functor
-- and then mapping the other one.
-- Formally written: `fmap (f .g) = fmap f . fmap g`
--
-- If we know that a type obeys the functor laws, we know that calling `fmap`
-- on a value of that type will only map the function over it, nothing more.
-- This leads to code that is more abstract and extensible, becuase we can use
-- laws to reason about behaviours that any functor should have and make
-- functions that operate reliably on any functor.
--
-- All the `Functor` instances in the standard library obey these laws.
--
-- We can also look at functors as things that output values in a context.
-- For instance, `Just 3` outputs the value `3` in the context that it might
-- or not output any values at all. `[1,2,3]` outputs three values in the
-- context that there may be multiple values or no values at all. The function
-- `(+3)` will output a value, depending on which parameter it is given.




-- APPLICATIVE FUNCTORS:
-- applicative functors are a more advanced kind of regular functors.
--
-- So far, when we were mapping functions over functors, we usually
-- mapped functions that take only one parameter. But what happens
-- when we map a function like `*`?
--
-- fmap (*) (Just 3) = Just ((*) 3)
--
-- so we get a function wrapped in a `Just`
--
-- We see how by mapping multi-parameter functions over functors, we get
-- functors that contain functions inside them. So now we can map functions
-- that take these functions as parameters over them, because whatever is
-- inside a functor will be given to the function that we're mapping over.
--
let a = fmap (*) [1,2,3,4]
:t a
a :: [Integer -> Integer]
fmap (\f -> f 9) a
[9, 18, 27, 36]
--
-- But what if we have a functor value of `Just (3 *)` and a functor value
-- of `Just 5` and we want to take out the function from `Just (3 *)` and
-- map it over `Just 5`? With normal functors, we're out of luck, because
-- all they support is mapping normal functions over existing functors.
-- Even when we mapped `\f -> f 9` over a functor that contained functions
-- inside it, we were just mapping a normal function over it. But we can't
-- map a function that's inside a function a functor over another functor
-- with what `fmap` offers us. We could pattern-match against the `Just`
-- constructor to get the function out of it and then map it over `Just 5`,
-- but we're looking for a more general and abstract way of doing that.
--
-- This is where the `Applicative` typeclass comes in. It lies in the
-- `Control.Applicative` module and it defines two methods, `pure`
-- and `<*>`. It doesn't provide a default implementation for either
-- of them so we have to define them ourselves.

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- This class has a clas constraint that says a constructor must be in the
-- `Functor` typeclass before it can be in the `Applicative` typeclass.
--
-- `f` is our applicative functor. So `pure` takes a value of any type
-- and returns an applicative functor with that value inside it.
-- The <*> function is sort of a beefed up `fmap`. Whereas `fmap` takes
-- a function and a functor and applies the function inside the functor,
-- <*> takes a functor that has a function in it and another functor and
-- maps the function from the first functor over the the second.
--
-- Applicative instance implementation for `Maybe`:

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something

-- again `f` is the applicative functor that takes once concrete type as a
-- parameter, so we write `instance Applicative Maybe where`
-- rather than `instance Applicative (Maybe a) where`.
--
-- We wrote `pure = Just` because value constructors like `Just` are normal
-- functions. We could have also written `pure x = Just x`.
--
-- Because of the Functor class constraint in `Applicative` we can assume
-- that both of <*>'s parameters are functors.

example1 = Just (+3) <*> Just 9
-- Just 12
example2 = pure (+3) <*> Just 10
-- Just 13
example3 = Just (++"HAHAHA") <*> Nothing
-- Nothing
example4 = Nothing <*> Just "woot"
-- Nothing

-- We see how doing `pure (+3)` and `Just (+3)` is the same in this case. Use
-- `pure` if you're dealing with `Maybe` values in an applicative context (i.e.
-- using them with <*>), otherwise stick to `Just`. The first three examples
-- demonstrate how the function is extracted and then mapped, but in this case,
-- they could have been achieved by just mapping unwrapped functions over
-- functors. The last line is interesting however, as we try to extract a
-- function from a `Nothing` and then map it over something, which of course
-- results in a `Nothing`.
--
-- With normal functors, you can just map a function over a functor and then
-- you can't get the result out in any general way, even if the result is a
-- partially applied function. Applicative functors, on the other hand, allow
-- you to operate on several functors with a single function. Check out this:

example5 = pure (+) <*> Just 3 <*> Just 5
-- Just 8
example6 = pure (+) <*> Just 3 <*> Nothing
-- Nothing

-- <*> is left-associative, which means that example 5 is the same as:
--
--                `(pure (+) <*> Just 3) <*> Just 5`
--
-- first the + function is put into a functor, then we get `Just (3+)`
-- thanks to partial application. Finally, `Just (3+) <*> Just 5` is
-- carried out, which results in a `Just 8`
--
-- Applicative functors and the applicative style of doing:
--
--      `pure f <*> x <*> y <*> ...`
--
-- allows us to take a function that expects parameters that aren't necessarily
-- wrapped in functors and use that function to operate on several values that
-- are in functor contexts. The function can take as many parameters as we want,
-- because it's always parially applied step by step between occurences of <*>.
--
-- This becomes even more handy and apparent if we consider the fact that
--
-- `pure f <*> x` = `fmap f x`
--
-- This is actually one of the APPLICATIVE LAWS.
-- This makes sense, `pure` puts a value in a default context. If we just put
-- a function in a default context and then extract and apply it to a value
-- inside another applicative functor, we did the same thing as just mapping
-- that function over that applicative functor.
--
-- Instead of writing `pure f <*> x <*> y <*> ...`
-- we can write: `fmap f x <*> y <*> ...`
--
-- This is why `Control.Applicative` exports a function called `<$>`, which
-- is just `fmap` as an infix operator:

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- (quick reminder that type variables and parameter names are independent of
--  each other. i.e there are two `f`s above that represent different things.)

-- By using <$>, the applicative style really shines, because now if we want to
-- apply a function `f` between three applicative functors, we can write:
--
--      `f <$> x <*> y <*> z`
--
-- If the parameters weren't applicative functors but normal values we'd simply
-- write `f x y z`
--
-- Let's take a closer look at how this works:

example7 = (++) <$> Just "Mark" <*> Just " Hoppus"
-- Just "Mark Hoppus"
example8 = (++) "Mark" " Hoppus"

-- so to use a normal function on applicative functors, just sprinkle some <$>
-- and <*> about and the function will operate on applicatives and return an
-- applicative.
--
-- What happens in example7 is that first (++) gets mapped over `Just "Mark"`,
-- resulting in a value `Just ("Mark"++)` and then <*> takes the function out
-- of the `Just` and maps it over `Just " Hoppus"` resulting in `Just "Mark Hoppus".
-- Had any of the two values been `Nothing`, the result would have also been
-- `Nothing`.
--
-- Other applicative functors:

--lists:
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [ f x | f <-fs,
                    x <-xs]

-- Earlier, we said that `pure` takes a value and puts it in a default context.
-- Or in other words, a minimal context that still yields that value. The minimal
-- context for lists would be [], but the empty list represents the lack of a
-- value, so it can't hold in itself the value that we used `pure` on. That's
-- why `pure` takes a value and puts it in a singleton list. Similarly, the
-- minimal context for the `Maybe` applicative functor would be a `Nothing`, but
-- it represents the lack of a value, so `pure` is implemented as `Just` in the
-- instance implementation for `Maybe`.

example9 = pure "Hey" :: [String]
-- ["Hey"]
example10 = pure "Hey" :: Mayber String
-- Just "Hey"

-- What about <*> then? If we look at what <*>s type would be if it were limited
-- to only lists, we get:
--
--              `(<*>) :: [a -> b] -> [a] -> [b]`
--
-- We use a list comprehension because <*> has to somewhow extract the function
-- out of its left parameter and then map it over the right parameter. The thing
-- here though is that the left list can have zero functions, one function, or
-- many functions. The right list can also hold several values. That's why we use
-- a list comprehension to draw from both list, applying every possible function
-- from the left list to every possible value from the right list:

example11 = [(*0), (+100), (^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]

-- if we have a list of functions that take two parameters, we can apply
-- those functions between two lists:

example12 = [(+), (*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]

-- because <*> is left-associative, `[(+),(*)] <*> [1,2]` happens first,
-- resulting in `[(1+), (2+), (1*), (2*)]` and then we get
-- `[(1+),(2+),(1*),(2*)] <*> [3.4]` which produces the final result:
-- [(1+3),(1+4),(2+3),(2+4),(1*3),(1*4),(2*3),(2*4)]

example13 = (++) <$> ["a", "b", "c"] <*> ["d","e","f"]
-- ["ad","ae","af","bd","be","bf","cd","ce","cf"]

-- We can now view lists as non-deterministic computations. A value like 100
-- or "what" can be viewed as a deterministic computation that has only one
-- result, whereas a list like [1,2,3] can be viewed as a computation that
-- can't decide which result it wants to have, so it presents us with all
-- the possible results. So when you do something like
-- `(+) <$> [1,2,3] <*> [4,5,6]`, you can think of it as adding together
-- two non-deterministic computations with `+`, only to produce another
-- non-deterministic computation that's even less sure about its result.
--
-- Using the applicative style on lists is often a good replacement for list
-- comprehensions:

example14 = [x*y | x <- [2,5,10],
                   y <- [8,10,11]]

-- or, using applicative style:
example15 = (*) <$> [2,5,10] <*> [8,10,11]

-- both examples above give: [16,20,22,40,50,55,80,100,110]

-- the second is usually a lot clearer. We can see that we're just calling *
-- between two non-deterministic computations.
--
-- It's easy to see how `pure f <*> xs` equals `fmap f xs` with lists.
-- `pure f` is just [f] and `[f] <*> xs` will apply every function in the
-- left list to every value in the right one, but there's just one function
-- in the left list, so it's like mapping.

----------
-- IO is another instance of `Applicative`:
instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

-- the minimal context that still holds is intuitively just `return`.
--
-- If <*> were just for IO it would have type:
--
--      `(<*>) :: IO (a -> b) -> IO a -> IO b`
--
-- Now, rather than just seeing <*> as extracting the left value, we also
-- have a notion of SEQUENCING, because we're taking two IO actions and
-- we're sequencing (glueing) them into one. We have to extract a result
-- from the first IO action but to extract from an IO action, it has to
-- be performed.
--
-- Consider:

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- we glue together two getLine IO actions and a return, because we wanted
-- our new glued-together IO action to hold the result.
-- Another way of writing this would be to use the applicative style:

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

-- once again we are just applying a function between the results of
-- two other IO actions.

-- we can also do stuff like:

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "Two lines = " ++ a

-- if you ever find yourself binding some IO actions to names and then
-- calling some function on them and presenting that as the result by
-- `return`, consider using the applicative style because it's usually
-- more concise and terse.


-------------
-- `(->) r` is another instance of `Applicative`
--
-- they're rarely used with the applicative style but it's still interesting:
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)

-- when we use `pure` the result it yields always has to be the same value
-- we called it with. A minimal default context that still yields that value
-- as a result. So we create a function that ignores its parameter and always
-- returns that value. If we look at the type for `pure`, but specialized for
-- the `(->) r` instance, it's:
--                               `pure :: a -> (r -> a)`

example16 = pure 4 "sadkflm"
-- 4
example17 = pure 45 [3,4,5,3]
-- 45

-- this instance is a bit cryptic:


example18 = (+) <$> (+3) <*> (*100) $ 5
-- 508

-- Calling <*> with two applicative functors results in an applicative functor,
-- so if we use it on two functions, we get back a function.
-- When we do `(+) <$> (+3) <*> (*100)`, we're making a function that will use
-- + on the results of (+3) and (*100) and return that.

example19 = (foo) <$> (+3) <*> (*2) <*> (/2) $ 5
  where foo x y z = [x, y, z]

-- Same again here, we create a function that will call the function
-- `\ x y z -> [x,y,z]` with the eventual results from (+3), (*2), and (/2)
--
-- You can think of functions as boxes that contain their eventual results, so
-- doing `k <$> f <*> g` creates a function that will call `k` with the eventual
-- results from `f` and `g`.
--
-- When we do something like `(+) <$> Just 3 <*> Just 5`, we're using + on values
-- that might or might not be there, which also results in a value that might or
-- might not be there. When we do (+) <$> (+10) <*> (+5), we're using + on the
-- future return values of (+10) and (+5) and the result is also something that
-- will produce a value only when called with a parameter.



----------------
-- ZipList is another instance of Applicative that lives in `Control.Applicative`

-- it turns out that there are actually more ways for lists to be applicative
-- functors. One way is the one we already covered, which says that calling <*>
-- with a list of functions and a list of values results in a list which has all
-- the possible combinations of applying functions from the left to the right.
--
-- However, it could also work in such a way that the first function in the
-- left list gets applied to the first element in the right one, the second
-- in the the left gets applied to the second in the right etc.
--
-- Because one type can't have two instances of the same typeclass,
-- the `ZipList a` type was introduced, which has one constructor `ZipList`
-- that simply takes a list, here's the instance:
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- because of how `zipWith` works, the resulting list will be as long as the
-- shorter of the two lists.
--
-- pure takes a value and puts it into a list repeating indefinitely. The infinite
-- list makes sense as a minimal value with zip lists, because it has to produce
-- the value on every position.

-- <*> does exactly as we said it should. Applies function n from the left
-- to value n from the right.
--
-- We can see that pure producing an infinite list satisfies the law that
-- `pure f <*> xs = fmap f xs`. If `pure 3` just returned `Ziplist [3]`,
-- `pure (*2) <*> Ziplist [1,5,10]` would result in `ZipList [2]`, because
-- the resulting list of two zipped list is always as long as the shorter of
-- the two.
--
-- So how do zip lists work in an applicative style? (`Ziplist a` doesn't have
-- a `Show` instance, so we have to use `getZipList` function to extract
-- out the raw list)

exmaple20 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101, 102, 103]
exmaple21 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
-- [101, 102, 103]
example22 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]

-- (The function (,,) is the same as `\x y z -> (x,y,z)`

-------ZIPWITH
-- Aside from `zipWith`, the standard library has functions such as `zipWith3`,
-- `zipWith4`, all the way up to 7. `zipWith` takes a function that takes
-- two parameters and zips with three lists with it, and so on. By using
-- zip lists with an applicative style, we don't have to have a separate zip
-- function for each numberof lists that we want to zip together. We just use
-- the applicative style to zip together an arbitrary amount of lists with
-- a function.
--
-----------
-- `Control.Applicative` defines a function that's called `liftA2`, which has
-- a type of: `liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`
-- it's defined like so:

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b ->  f c
liftA2 f a b = f <$> a <*> b

-- Nothing special then. It just applies a function between two applicatives,
-- hiding the applicative style that we've become familiar with. The reason
-- we're looking at it is because it clearly showcases why applicative functors
-- are more powerful than just ordinary functors. With ordinary functors, we can
-- just map functions over one functor. But with applicative functors, we can
-- apply a function between several functors. It's also interesting to look at
-- this function's type as `(a -> b -> c) -> (f a -> f b -> f c). If we look at
-- it like this, we can say that `liftA2` takes a normal binary function and
-- promotes it to a function that operates on two functors.

-- Here's an interesting concept: we can take two applicative functors and
-- combine them into one applicative functor that has inside it the results
-- of those two applicative functors in a list. For instance, we have `Just 3`
-- and `Just 4`. Let's assume that the second one has a singleton list inside
-- it:
example23 = liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
example24 = (:) Just 3 <*> Just [4]
-- Just [3,4]

-- Remember : is just a function. We can carry on combining `Just [3,4]` with
-- other `Just n`s. It seems that we can combine any amount of applicatives into
-- one applicative that has a list of the results of those applicatives inside it.
-- Let's try implementing a function that takes a list of applicatives and returns
-- an applicative that has a list as its result value:

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- Here we are transforming a list of applicatives into an applicative with a
-- list. From that, we can lay some groundwork for an edge condition. If we want
-- to turn an empty list into an applicative with a list of results we will
-- obviously just put an empty list in a default context.
--
-- So if we do `sequenceA [Just 1, Just 2]`,
-- that's equal to `(:) <$> Just 1 <*> sequenceA [Just 2]`
-- which is equal to `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
-- So we end up with `Just [1,2]`
--
-- Another way to implement `sequenceA` is with a fold. Remember, pretty much
-- any function where we traverse a list and accumulate a result can be
-- implemented with a fold.

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- We approach the list from the right and start off with an accumulator value
-- of `pure []`. We do `liftA2 (:)` between the accumulator and the last element
-- of the list, which results in an applicative that has a sigleton in it.
-- The we do `liftA2 (:)` with the now last element and the current accumulator
-- and so on...

example25 = sequenceA [Just 3, Just 2, Just 1]
-- Just [3,2,1]
example26 = sequenceA [(+3), (+2), (+1)] 3
-- [6,5,4]
example27 = sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

-- When used on Maybe values, `sequenceA` creates a Maybe value with all the
-- results inside it as a list. If one of the values was `Nothing`, then
-- the result is also a `Nothing`. This is cool when you have a list of
-- `Maybe` values and you're only interested in the values if none of them
-- are a `Nothing`.
--
-- When used with functions, `sequenceA` takes a list of functions and returns
-- a function that returns a list. In example26 we made a function that took
-- a number as a parameter and applied it to each function in the list and then
-- returned a list of results.
--
-- Doing `(+) <$> (+3) <*> (*2)` will create a function that takes a parameter,
-- feeds it to both (+3) and (*2) and then calls `+` with those two results. In
-- the same vein, it makes sense that `sequenceA [(+3), (*2)]` makes a function
-- that takes a parameter and feeds it to all of the functions in the list.
-- Instead of calling `+` with the results of the functions, a combination of
-- `:` and `pure []` is used to gather those results in a list.
--
-- Using `sequenceA` is cool when we have a list of functions and we want to feed
-- the same input to all of them and then view the list of results. For instance,
-- we have a number and we're wondering whether it satisfies all of the predicates
-- in a list. One way to do this:

example28 = map (\f -> f 7) [(>4), (<10), odd]
-- [True, True, True]
example29 = and $ map (\f -> f 7) [(>4), (<10), odd]
-- True

-- (Remember, `and` takes a list of bools and returns true is they all are true.)
--
-- Another way to achieve the same thing would be with `sequenceA`:
example30 = sequenceA [(>4), (<10), odd] 7
-- [True, True, True]
example31 = and $ sequenceA [(>4), (<10), odd] 7
-- True

-- Here we create a function that will take a number and feed it to all of
-- the predicates in [(>4),(<10),odd] amd return a list of booleans.
-- It turns a list with the type `(Num a) => [a -> Bool]` into a function
-- with the type `(Num a) => a -> [Bool]`
--
-- Because lists are homogenous, all the functions in the list have to be
-- functions of the same type, of course.
--
-----
--
-- When used with [], `sequenceA` takes a list of lists and returns a list
-- of lists... It actually creates lists that have all possible combinations
-- of their elments. Example with list comprehension equivalent:

example32 = sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
example33 = [[x,y] | x <- [1,2,3], y <- [4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

------
-- When used with IO actions, `sequenceA` is the same thing as `sequence`.
-- It takes a list of IO actions and returns an IO action that will perform
-- each of those actions and have as its result a list of the results of those
-- IO actions. That's because to turn an `[IO a]` value into an `IO [a]` value,
-- to make an IO action that yields a list of results when performed, all those
-- actions have to be sequenced so that they're then performed one after the
-- other when evaluation is forced. You can't get the result of an IO action
-- without performing it!

exmaple34 = sequenceA [getLine, getLine, getLine]
heyh
ho
woo
--["heyh", "ho", "woo"]
--

-- Like normal functors, applicative functors come with a few laws. The most
-- important one we have already mentioned: `pure f <*> x = fmap f x`.
-- APPLICATIVE LAWS:
--
--    - pure id <*> v = v
--    - pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--    - pure f <*> pure x = pure (f x)
--    - u <*> pure y = pure ($ y) <*> u
--
-- We won't go over them in detail right now because that would take a while
-- and probably be kind of boring.
--
-- -----------
-- In conclusion, applicative functors are very interesing and very
-- useful because they allow us to combine different computations, such as
-- IO computations, non-deterministic computations, computations that might
-- have failed, etc. by using the applicative style. Just by using <$> and
-- <*> we can use normal functions to uniformly operate on any number of
-- applicative functors and take advantage of the semantics of each one.
-- (Wowza! 786 lines is definitely a new record)
