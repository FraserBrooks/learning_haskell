
-- The pattern of getting some string from the input, transforming it with a
-- function and then outputting that is so common that there is a function
-- to make it easier: `interact` takes a function of type `String -> String`
-- as a parameter and returns an IO action that will take some input, run that
-- function on that input, and then print out the function's result:

main = interact palindrome

palindrome :: String -> String
palindrome = unlines . map (\s -> if isPalindrome s then "Y" else "N") . lines

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs
