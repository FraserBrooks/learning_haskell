
-- Exceptions
--
-- All languages have procedures, functions, and pieces of code that might
-- fail. Different langauges have different ways of handling these failures.
-- In C, we usually use some abnormal return value (like `-1` or a null pointer)
-- to indicate that what a function returned shouldn't be treated like a normal
-- value. Java and C#, on the other hand, tend to use exceptions to handle failure.
--
-- Haskell has a very good type system. Algebraic data types allow for types like
-- `Maybe` and `Either` and we can use values of those types to represent results
-- that may be there or not. In C, returning, say, '-1' on failure is completely
-- a matter of convention. It only has special meaning to humans. If we're not
-- careful, we might treat these abnormal values as ordinary ones and then they
-- can cause havoc and dismay in our code. Haskell's type system gives us some
-- much-needed saftey.
--
-- Despite these expressive types, Haskell still has support for exceptions
-- because they make more sense in IO contexts. A lot of things can go wrong
-- when dealing with the outside world because it's so unreliable.
--
-- So IO code (impure code) can throw exceptions but so can pure code. For
-- example if we look at the `div` and `head` functions, their types don't
-- utilise the `Maybe` or the `Either` type but the can both fail.
--
-- Pure code can throw exceptions, but they can only be caught in the IO
-- part of our code because in pure code we don't know when (or even if) our
-- code will be evaluated. Because pure code is lazy, it doesn't have a well
-- defined order of execution, whereas IO code does.
--
-- Earlier we talked about how we should spend as little time as possible in
-- the IO part of our code. The logic of our program should reside mostly within
-- our pure functions, because their results are dependent only on their parameters.
-- When dealing with pure functions, you only have to think about what a function
-- returns, because it can't do anything else. This makes our life easier.
--
-- Even though doing some logic in IO is necessary, like opening files, it should
-- preferably be kept to a minimum. Pure functions are lazy by default, which
-- means that we don't know when they will be evaluated and that it really
-- shouldn't matter. However, once pure functions start throwing exceptions,
-- it matters when they are evaluated. That's why we can only catch exceptions
-- thrown from pure functions in the IO part of our code. And that's bad, because
-- we want to keep the IO part as small as possible. The solution? Don't mix
-- exceptions and pure code. Take advantage of Haskell's powerful type system
-- and use types like `Either` and `Maybe` to represent results that may have
-- failed.
--
-- That's why we'll be looking at how to use IO exceptions for now. An IO excpetion
-- is caused when something goes wrong while we are communicating with the outside
-- world in an IO action that's part of `main`. For example:

-- import System.Environment
-- import System.IO

-- main = do (fileName:_) <- getArgs
--           contents <- readFile fileName
--           putStrLn $ "The file has  " ++ show (length (lines contents)) ++ " lines."

-- the code above will through an exception if the file doesn't exist. We could
-- use the `doesFileExist` function from `System.Directory`.
--

-- import System.Environment
-- import System.IO
-- import System.Directory

-- main = do (fileName:_) <- getArgs
--           fileExists <- doesFileExist` fileName
--           if fileExists
--              then do contents <- readFile fileName
--                      putStrLn $ "Lines in file = " ++ show (length (lines contents))
--              else do putStrLn "The file doesn't exist!"

-- We did `fileExists <- doesFileExist fileName` because `doesFileExist` has
-- a type of `doesFileExist :: FilePath -> IO Bool`, which means that it returns
-- an IO action that has as its result a boolean value which tells us if the file
-- exists. We can't just use `doesFileExist` in an if expression directly.

-- Another solution here would be to use exceptions. It's perfectly acceptable
-- to use them in this context. A file not existing is an exception that arises
-- from IO, so catching it in IO is fine.
--
-- To deal with this by using exceptions, we're going to use the `catch` function
-- from `System.IO.Error`. Its type is `catch :: IO a -> (IOError -> a) -> IO a`
-- It takes two parameters. The first being an IO action. The second is the handler.
-- If the first IO action passed to `catch` throws an IO exception, that exception
-- gets passed to the handler, which then decides what to do. So the final result
-- is an IO action that will either act the same as the first parameter or it will
-- do what the handler tells it.
--
-- This behaviour is very similar to the try-catch blocks in languages like Python
-- or Java.
--
-- The handler takes a value of type `IOError`, which is a value that signifies
-- that an IO exception occurred. It also carries information regarding the type
-- of the exception that was thrown. How this type is implemented depends on the
-- implementation of the language itself, which means that we can't inspect values
-- of type `IOError` by pattern matching against them, just like we can't pattern
-- match against values of type `IO something`. We can, however, use a bunch of
-- useful predicates to find out stuff about values of type `IOError`:


-- import System.Environment
-- import System.IO
-- import System.IO.Error

-- main = toTry `catchIOError` handler

-- toTry :: IO ()
-- toTry = do (fileName:_) <- getArgs
--            contents <- readFile fileName
--            putStrLn $ "Lines in file = " ++ show (length (lines contents))

-- handler :: IOError -> IO ()
-- handler e = putStrLn "OOPSIE DOODLE! NO SUCH FILE EXISTS"


-- In this example we don't check to see what kind of error we have actually
-- had. We just hazardously assume that it is caused by the file not existing.
-- Just as in other languages, this is bad practice. The solution is to check
-- what kind of error we got:


-- import System.Environment
-- import System.IO
-- import System.IO.Error

-- main = toTry `catchIOError` handler

-- toTry :: IO ()
-- toTry = do (fileName:_) <- getArgs
--            contents <- readFile fileName
--            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- handler :: IOError -> IO ()
-- handler e
--   | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--   | otherwise = ioError e


-- Everyting stays the same except the handler, which we modified to only catch
-- a certain group of IO exceptions. Here we used two new functions from
-- `System.IO.Error` :
--                        `isDoesNotExistError` and `ioError`
--
-- `isDoesNotExistError` is a predicate over `IOError`s that quite intuitively will
-- return True or False depending on whether or not it is a `not exist error`.
--
-- If the exception is not caused by the exception we want to catch we simple
-- 're-throw' our exception with the `ioError` function. It has a type of:
--
--            `ioError :: IOException -> IO a`
--
-- so it takes an IOError and produces an IO action that will throw it. The IO
-- action has a type of `IO a` because it never actually yields a result so it
-- can act as IO anything.

-- There are several predicates that act on IOError like the one we just used:
--
--    `isAlreadyExistsError`
--    `isDoesNotExistError`
--    `isAlreadyInUseError`
--    `isFullError`
--    `isEOFError`
--    `isIllegalOperation`
--    `isPermissionError`
--    `isUserError`
--
-- Most of these are pretty self-explanatory. `isUserError` evaluates to true
-- when we use the `userError` function to make the exception, which is used
-- for making exceptions from our code and equipping then with a String.
-- For instance, you can do:
--                            `ioError $ userError "remote computer lost"`
--
-- although it's prefered to use types like `Either` or `Maybe` to express
-- possible failure.
--
--
-- `System.IO.Error` also exports functions that enable us to ask our exceptions
-- for some attributes, like what the handle of the file that caused the error is,
-- or what the filename is. These start with `ioe` and a full list of them is
-- available in the documentation.
--
-- For example we can use the function `ioeGetFileName :: IOError -> Maybe FilePath`

import System.Environment
import System.IO
import System.IO.Error

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
      case ioeGetFileName e of
        Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
        Nothing -> putStrLn "Can't find file! Unknown location!"
  | otherwise = ioError e



