
import Data.Char
import Data.List
import Data.Function (on)

-- The Data.Char module is for, as the name suggests, dealing
-- with characters and, as strings are lists of characters, strings
-- too.

-----
  --Predicates:
  -- isControl
  -- isSpace
  -- isLower
  -- isUpper
  -- isAlpha (a letter)
  -- isAlphaNum (letter or number)
  -- isPrint (printable ie. not control)
  -- isDigit
  -- isOctDigit
  -- isHexDigit
  -- isLetter
  -- isMark (unicode mark characters)
  -- isNumber
  -- isPunctuation
  -- isSymbol
  -- isSeparator
  -- isAscii (first 128 chars of unicode)
  -- isLatin1 (first 256 of unicode)
  -- isAsciiUpper
  -- isAsciiLower

validUsername :: String -> Bool
validUsername = all isAlphaNum

sentence = "hey guys its me"
example1 = words sentence
-- ["hey", "guys", "its", "me"]

example2 = groupBy ((==) `on` isSpace) sentence
-- ["hey", " ", "guys", " ", "its", " ", "me"]
-- ^ not quite what we want. We can filter out the spaces though:

example3 = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ sentence
-- ["hey", "guys", "its", "me"]

example4 = map toUpper "hello"
-- "HELLO"

example5 = map toLower "HELLO"

example6 = map toTitle "hello"
-- "HELLO" (same as upper for most chars)

example7 = map digitToInt "34538"
-- [3,4,5,3,8]

exampleHex = map digitToInt "FF85AB"
-- [15,15,8,5,10,11]

example9 = map intToDigit [15,2,4]
-- "f24"

-- ord and char convert characters to and from their corresponding numbers
example10 = ord 'a'
-- 97
example11 = chr 97
-- 'a'



-- Caesar cipher:

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in  map chr shifted

example12 = encode 3 "Heeey"
-- "Khhh|"

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


example13 = let msg = encode 5 "Hello" in "Hello" == decode 5 msg
-- True












