

-- main = putStrLn "hello, world"

-- the type of putStrLn is:
--    putStrLn :: String -> IO ()
--
-- it takes a String and returns an I/O action that has a result type of
-- () (i.e. the empty tuple, also known as unit).
--
-- An I/O action is something that, when performed, will carry out an
-- action with a side-effect (that's usually either reading from the
-- input or printing stuff to the screen) and will also contain some kind
-- of return value inside it. Printing a string to the terminal doesn't need
-- to return any value so the IO just has a value of ().
--
-- An I/O action will action will be performed when we give it a name of main
-- and then run our program. Having your whole program be just one I/O action
-- might seem kind of limiting. That's why we can use `do` to glue together
-- several IO actions into one:


main = do
  putStrLn "Hello world!, what's your name?"
  name <- getLine
  putStrLn ("HEy " ++ name ++ ".")

-- each line in the above is an IO action that we 'glued together' using
-- the `do` keyword. The action we got has a type of IO (), because that's
-- the type of the last IO action inside.
-- Because of that, main always has a type signature of
--        main :: IO `some concrete type`
-- By convention, we don't usually specify a type declaration for main.

-- name <- someIOAction means take the return value of the IO action and
-- bind it to `name`.
--
--
