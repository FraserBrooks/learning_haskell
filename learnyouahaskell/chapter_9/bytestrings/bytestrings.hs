import System.Environment

-- Bytestrings
--
-- Lists are cool and useful. Their laziness allows us to exchange the for
-- and while loops of other languages for filtering and mapping over lists,
-- because evaluation will only happen once it really needs to, so things
-- like infinite lists are possible. That's why we can also get away with
-- using lists to represent streams, either when reading from files or the
-- standard input.
--
-- However, there is a drawback, processing files as strings tends to be
-- slow. We can get away with it with small amounts but because Char's have
-- a variable size depending on the Char it can make large reads quite
-- inefficient.
--
-- This is where bytestrings come in. Bytestrings are sort of like lists,
-- only each element is one byte (8 bits) in size. The way they handle
-- haskell's laziness is also different.
--
-- Bytestrings come in two flavours: strict and lazy ones. Strict bytestrings
-- reside in `Data.ByteString` and don't use laziness at all. If you want to
-- evaluate the first byte of a strict bytestring, you have to evaluate it all.
-- This obvioulsy stops us from having infinite strict bytestrings.
-- The other kind of bytestring is the `Data.ByteString.Lazy`. This bytestring
-- is still lazy but less so than lists in a way. In a list, there are as many
-- thunks as there are elements, that's what makes them slow for some purposes.
-- Lazy bytestrings on the other hand work by storing data in chunks, each chunk
-- has a size of 64k. So if you evaluate a byte in a lazy bytestring, the first
-- 64k will be evaluated and then there will be a thunk for the rest of the
-- chunks. This means memory useage will be kept to a minimum.

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- the function `pack` has type:
--
--            pack :: [Word8] -> ByteString
--
-- what that means is that it takes a list of bytes of type Word8 and returns
-- a byteString. The Word8 type is just a number type, like Int, only it has
-- a much smaller range, namely 0-255. It represents an 8-bit number. And just
-- like Int, it's in the `Num` typeclass.

example1 = B.pack [99,97,110]
-- Chunk "can" Empty

-- Empty here is like [] for lists.
--
-- `unpack` is the inverse function of `pack`. It takes a bytestring and
-- turns it into a list of bytes.
--
-- `fromChunks` takes a list of strict bytestrings and converts it to a
-- lazy bytestring. `toChunks` takes a lazy bytestring and converts it to
-- a list of strict ones.

example2 = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
-- Chunk "()*" (Chunk "+,-"  (Chunk "./0" Empty))

-- The bytestring version of : is called `cons`. It takes a byte and a bytestring
-- and puts the byte at the beginning. It's lazy though, so it will make a new
-- chunk even if the first chunk in the bytestring isn't full. That's why it's
-- better to use the strict version `cons'` if you're going to be inserting a
-- lot of bytes at the beginning of a bytestring.

example3 = B.cons 85 $ B.pack [80,81,82,84]
-- Chunk "U" (Chunk "PQRT" Empty)

example4 = B.cons' 85 $ B.pack [80,81,82,84]
-- Chunk "UPQRT" Empty

example5 = foldr B.cons B.empty [50..60]
-- Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" ....

example6 = foldr B.cons' B.empty [50..60]
-- Chunk "23456789:;<" Empty

-- As you can see `empty` makes an empty bytestring.
-- There are a load of function analogous to the those in `Data.List`, including,
-- but not limited to:
--
--    `head`, `tail` , `init`, `null`, `length`, `map`, `reverse`, `foldl`,
--    `concat`, `takeWhile`, `filter` etc...

-- it also has functions that have the same name, and behave the same way as
-- some functions found in `System.IO`, only `String`s are replaced with
-- `ByteString`s. For instance, the `readFile` function in Sytem.IO has a
-- type of:
--              `readFile :: FilePath -> IO String`
--
-- whereas the bytestring one:
--
--              `readFile :: FilePath -> IO Bytestring`
--
-- Obviously one needs to be careful when reading files with strict bytestrings
-- as the program will attempt to read it into memory all at once.
--
-- Simple program that takes two filenames as command-line arguments and copies
-- the first into the second:

main = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents


