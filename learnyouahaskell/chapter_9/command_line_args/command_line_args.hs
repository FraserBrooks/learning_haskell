


-- Command Line Arguments
--
-- Haskell's standard library has a nice way of getting command line
-- arguments of a program.
-- In the previous section, we made one program for adding an item to
-- our to-do list and one program for removing an item. But in those
-- programs we hard coded the file name of our to-do list.
--
-- The `System.Environment` module has two cool IO actions. One is `getArgs`,
-- which has a type of `getArgs :: IO [String]` and is an IO action that will
-- get the arguments that the program was run with and have as its contained
-- result a list with the arguments. `getProgName` has a type of
-- `getProgName :: IO String` and is an IO action that contains the program
-- name.

import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM putStrLn args
  putStrLn "The program name is:"
  putStrLn progName



