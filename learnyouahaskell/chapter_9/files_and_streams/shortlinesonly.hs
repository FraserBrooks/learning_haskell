
-- getting some string from the input, transforming it with a function
-- and then outputting the new text is so common that there is a function
-- that makes it easier:
--
-- `interact` takes a function of type `String -> String` as a parameter
-- and returns an IO action that will take some input, run that function on
-- it and then print out the results:

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\l -> length l < 10) allLines
      result = unlines shortLines
  in result

-- or even terser:
--  main = interact $ unlines . filter ((<10) . length) . lines

