
-- Files and Streams

-- `getContents` is an IO action that reads everything from the standard
-- input until it encounters an end-of-file character. Its type is
--    getContents :: IO String
-- the cool thing about it though is that it does lazy IO. When we do
--    foo <- getContents
-- it doesn't read all of the input at once, store it in a massive chunk
-- of memory and then bind it to foo. Rather, it reads the input as and
-- when it's needed.
--
-- getContents is really useful when piping the output from one program
-- into the input of our program.
--
-- Piping:
-- if we take our capslocker.hs program:
--
--     |   import Control.Monad
--     |   import Data.Char
--     |
--     |   main = forever $ do
--     |     putStr "Give me some input: "
--     |     l <- getLine
--     |     putStrLn $ map toUpper l
--     |
--
-- we can pipe a text file directly into it in the terminal via:
--
--    | $ cat mytext.txt | ./capslocker
--
-- we can use getContents to make our program even shorter:
--
--    | import Data.Char
--    |
--    | main = do
--    |     contents <- getContents
--    |     putStr (map toUpper contents)
--    |


-- Let's make a program that takes some input and prints out only those
-- lines that are shorter than 10 characters:

main = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result


