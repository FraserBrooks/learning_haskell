
import Control.Monad

main = do
  foo <- putStrLn "Hello there!" -- here we are just assigning () to foo
  putStrLn "this line is also valid and makes more sense"
  let someString = "we can use let when we don't need to wrap in an IO"
      someChar = 'c'
      ksdlgk = "asd"
  return "is the opposite of <-"
  return "takes a value and encapsulates it in an IO type"
  return "we can have as many as we want as they don't stop execution"
  return "all these `IO String`s are being thrown away"
  return ()
  return 3
  return 'a'
  return "All of these return lines have no effect. We just use   "
  return "lines when we need an I/O action that doesn't do anything."

  putStr "same as putStrLn but doesn't add a new line"
  putChar '#' -- putStr is defined recursively with putChar
  print 2 -- print a type that's part of `Show`
  putStrLn ""

-- getChar is an I/O action that reads a character from input but
-- because of the buffer it won't read until the user hits the return key.
--
-- main = do
--  c <- getChar
--  if c /= ' '
--      then do
--          putChar c
--          main
--      else return ()

-- when function:  part of Control.Monad. Takes a boolean value and an I/O
--                 action, if that boolean is True it returns the same I/O
--                 action that was given, if False it returns `return ()`

  c <- getChar
  getLine -- clear the line
  when (c /= ' ') $ putChar c
  putStrLn ""

-- sequence takes a list of I/O actions and returns an I/O action that
-- will perform those actions one after the other. The result contained
-- in that I/O action will be a list of the results of all the I/O actions
-- that were performed.

-- main = do
--  a <- getLine
--  b <- getLine
--  c <- getLine
--  print [a,b,c]     is the same as:

  rs <- sequence [getLine, getLine, getLine]
  print rs
  sequence (map print [1,2,3,4,5])
-- because mapping a function that returns an I/O action over a list and
-- then sequencing it is so common, the utility functions mapM and mapM_
-- where introduced:
--
-- mapM takes a function and a list, maps the function over the list and
-- then sequences it:
  mapM print [1,2,3]
-- mapM_ does the same, only it throws away the result later. We usually
-- use mapM_ when we don't care what result our I/O actions have:
  mapM_ print [1,2,3]


-- forever takes an I/O action and returns an I/O action that just repeats
-- the I/O action it got forever.
--
-- main = forever $ do
--    putStr "Input: "
--    l <- getLine
--    putStrLn $ map toUpper l

-- forM (located in Control.Monad) is like mapM, only that it has its
-- parameters switched around. The first parameter is the list and the
-- second one is the function to map over that list, which is then sequenced.
-- That allows us to use lambdas and the do notation like so:

  do
    colours <- forM [1,2,3,4] (\a -> do
      putStrLn $ "Input a colour: " ++ show a
      colour <- getLine
      return colour)
    putStrLn "The colours you gave are: "
    mapM putStrLn colours

-- we don't actually need to do `return colour` in the inner do block
-- we could just have `getLine` on the second line instead of
-- `colour <- getLine`.
--
-- you can think of forM as meaning: make an I/O action for every
-- element in this list. What each I/O action will do can depend on
-- the element that was used to make the action. Finally, perform those
-- actions and bind their results to something. We don't have to bind it,
-- we could just throw the results away if we don't need them.

-- we could have done the above without forM, only it's more readable with
-- forM.


















