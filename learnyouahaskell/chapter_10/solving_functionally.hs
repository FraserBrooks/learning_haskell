-------------------------------------
-- Functionally Solving Problems


-- Using what we have learned so far to solve some cool problems:

-- Reverse Polish notation calculator
--
--
-- Usually write mathematical expressions in an infix manner
--
-- eg. 10 - 3 where `-` is an infix operator just like `+` and `elem` in haskell.
--
-- The downside to this is that we need to use parentheses to denote precedence.
--
--
-- Reverse Polish Notation is another way of writing down mathematical expressions.
-- Initially it looks a bit weird, but it's actually very easy to understand and
-- use because there's no need for parentheses and it's very easy to punch into
-- a calculator.
--
-- Example:  10 - (4 + 3) * 2    becomes:    10 4 3 + 2 * -
--
-- to evaluate an expression it helps to think of a stack. Every time a number
-- is encountered we push it onto the stack. When we encounter an operator, pop
-- the two numbers on top of the stack, use the operator on these two, and then
-- push the resulting number back onto the stack. When you reach the end of the
-- expression, you should be left with a single number if the the expression was
-- well-formed; that remaining number is of course, the result.
-- eg. taking our example above:
-- 10
-- 4 10
-- 3 4 10
-- (+ 3 4) 10
-- 7 10
-- 2 7 10
-- (* 2 7) 10
-- 14 10
-- (- 14 10)
-- -4 = result
--
-- So how could we make a Haskell function that takes as its parameter a string
-- that contains a RPN expression, like "10 4 3 + 2 * -" and gives us back its
-- result?
--
-- type would look something like:
--
-- solveRPN :: (Num a) => String -> a
-- we can implement this with a list and a fold (the accumulator becomes our stack)
-- we'll use a list as our stack and keep the top of the stack as the head of the
-- list as it's far more efficient to add to the front of the list than the back.

import Data.List
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction xs numberString = read numberString:xs



-- Note that we have a class constraint of `Read a` because we call `read` on
-- our string to get the number. So this declaration means that the result can
-- be of any type that's  part of the `Num` and `Read` typeclasses (like Int
-- and Float)

-- we can easily modify our function to work with more complex operators:

solveRPNf :: String -> Float
solveRPNf = head . foldl foldFoo [] . words
  where foldFoo (x:y:ys) "*" = (x * y):ys
        foldFoo (x:y:ys) "+" = (x + y):ys
        foldFoo (x:y:ys) "-" = (y - x):ys
        foldFoo (x:y:ys) "/" = (y / x):ys
        foldFoo (x:y:ys) "^" = (y ** x):ys
        foldFoo (x:xs) "ln"  = log x:xs
        foldFoo xs "sum"     = [sum xs]
        foldFoo xs numString = read numString:xs

-- as it stands this function is not at all fault tolerant. When given input
-- that doesn't make sense, it will just crash everything. We'll be able to
-- make a fault tolerant version of this function with a type declaration of
-- `solveRPN :: String -> Maybe Float` once we take a look at monads.




