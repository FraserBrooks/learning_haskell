
import System.Random

-- Randomness

-- Enter the `System.Random` module. It has all the functions that satisfy
-- our needs for randomness.
--
-- The function `random` has type:
--
--          `random :: (RandomGen g, Random a) => g -> (a, g)`
--
-- The `RandomGen` typeclass is for types that can act as sources of
-- randomness. The `Random` typeclass is for things that can take on
-- random values. A boolean value can take on a random value (true or
-- false). A number can also take on a random value. A function on the
-- other hand obviously cannot take on a random value.
--
-- To use the `random` function we need a random generator.
-- The `System.Random` module exports a type called `StdGen` that is an
-- instance of the `RandomGen` typeclass. We can either make a StdGen
-- manually or we can tell the system to give us one based on a multitude
-- of sort of random stuff.
--
-- To manually make a random generator we can use the `mkStdGen` function
-- with type:
--              mkStdGen :: Int -> StdGen
--
-- It takes an integer and uses it to create a random generator.

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (fst, newGen) = random gen
      (snd, newGen') = random newGen
      (thd, newGen'') = random newGen'
  in (fst, snd, thd)


-- There's actually a function `randoms` that takes a generator and
-- returns an infinite sequence of values based on that generator.

example1 = take 5 $ randoms (mkStdGen 20896) :: [Int]
example2 = take 5 $ randoms (mkStdGen 20333) :: [Bool]



randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (rest, finalGen) = finiteRandoms (n-1) newGen
  in (value:rest, finalGen)



-- What if we want a random value in some sort of range?
-- We can use `randomR` with type:
--
--        `randomR :: (RandomGen g, Random a) :: (a,a) -> g -> (a,g)
--
-- the first argument is a tuple containing the lower and upper bounds:

-- die roll:
example3 = randomR (1,6) (mkStdGen 3425423) :: (Int, StdGen)

-- there's also `randomRs` which produces a stream of random values:

example4 = take 10 $ randomRs ('a', 'z') (mkStdGen 34) :: [Char]

-- The problem so far is that we've been defining all our random numbers
-- with hard coded integers which means we'll get the same numbers every
-- time. That's why `System.Random` offers the `getStdGen` IO action, which
-- has a type of `IO StdGen`. When your program starts, it asks the system
-- for a good random number generator and stores that in a so called global
-- generator. `getStdGen` fetched you that global random generator when you
-- bind it to something:

main = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a','z') gen) ++ "\n"

-- another useful action is the `newStdGen` action which splits our
-- current random generator into two generators. It updates the global
-- random generator with one of them and encapsulates the other as its
-- result.

  gen' <- newStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen') ++ "\n"

-- not only do we get a new random generator when we bind `newStdGen` to
-- something, the global one gets updated as well, so if we do `getStdGen`
-- again and bind it, we'll get a third generator:

  gen'' <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen'') ++ "\n"

-- the code above will print out three different random strings.
