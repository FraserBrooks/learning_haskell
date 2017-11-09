
import System.IO
import Data.Char

main :: IO ()
main = do
  handle <- openFile "lonedigger.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle


-- here we are using a new function `openFile`
--
--      openFile :: FilePath -> IOMode -> IO Handle
--
-- FilePath is just a type synonym for String:
--
--      type FilePath = String
--
-- IOMode:
--
--      data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- Finally, it returns an IO Handle. If we bind that action to something
-- we get a `Handle`. A value of type `Handle` represents where our file
-- is. We can then use that handle to read and write from our file.
--
--
-- In the next line of our program we see `hGetContents`. It takes a `Handle`
-- and returns an `IO String` containing the contents. Pretty simple. It's
-- obviously very similar to `getContents` except instead of reading from the
-- standard input it reads from a handle. In all other aspects they work the same.


-- another way of doing exactly what we just did is to use the `withFile` function,
-- which has a type signature of:
--
--      withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--
-- it takes a path to a file and an IOMode and then it takes a function
-- that takes a handle and returns some I/O action. What it returns is an
-- IO action that will open that file, do something we want with it, and
-- then close the file:

  withFile "lonedigger.txt" ReadMode (\handle -> do
    content <- hGetContents handle
    putStr content)
    -- file is closed automatically via `withFile`

-- we could define our own withFile function:

-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--   handle <- openFile path mode
--   result <- f handle
--   hClose <- handle
--   return result

-- we know the result will be an IO action so we can just start off with
-- a `do`. First we open the file and get a handle from it then we apply
-- handle to our function to get back the IO action that does all the work.
-- We bind that action to result, close the handle, then return the result.
-- Pretty simple stuff.

-- Just like we have hGetContents as a file version of getContents. There is
-- also:
--        hGetLine    hPutStr    hPutStrLn    hGetChar    etc.
--
-- They work exactly like their counterparts for operating on standard input
-- and output only they take a handle for a file as a parameter.
--
-- Loading files and then treating their contents as strings is so common that
-- there are a few functions to make our lives easier:

-- `readFile` has a type signature of `readFile :: FilePath -> IO String`.
-- Remember, `FilePath` is just a fancy name for `String`. It takes a
-- filePath and returns an IO action that will read that file:

--(main =)
  do
    contents <- readFile "lonedigger.txt"
    putStr contents

-- because we don't get access to the handle, the readFile function will close
-- the file for us.


-- writeFile has a type of: `writeFile :: FilePath -> String -> IO ()`
-- if the file already exists it will be overwritten:

  do
    contents <- readFile "lonedigger.txt"
    writeFile "lonedigger_caps.txt" (map toUpper contents)

-- appendFile has a type signature that's just like writeFile, only it
-- doesn't overwrite a file if it already exists but instead appends to the end.





-- We talked about how doing contents <- hGetContents handle doesn't cause the
-- whole file to be read at once and stored in-memory. It's IO lazy, so doing
-- this:

-- main = do
--   withFile "something.txt" ReadMode (\handle -> do
--     contents <- hGetContents handle
--     putStr contents)

-- is actually just like connecting a pipe from the file to the standard output.
-- Just like you can think of lists as streams, you can also think of files as
-- streams. This will read one line at a time and print it out to the terminal
-- as it goes along. So how wide is the pipe then? How often will the disk be
-- accessed? For text files the default buffering is usually line-buffering.
-- That means that the smallest part of the file to be read at once is one line.
-- So the code above is actually reading one line, then printing it, reading
-- another, etc... For binary files, the default buffering is usually
-- block-buffering where it will read the file chunk by chunk where the chunk
-- size is determined by your OS.

-- You can control exactly how buffering is done by using `hSetBuffering` which
-- takes a handle and a `BufferMode` and returns an IO action that sets the
-- buffering. `BufferMode` is a simple enumeration data type and the possible
-- values it can hold are:
--
--    `NoBuffering` `LineBuffering` `BlockBuffering (Maybe Int)`
--
-- the `Maybe Int` is for how big the chunk should be in bytes where a value
-- of nothing will let the OS decide.
-- Reading our files in big chunks can help when we want to minimise disk
-- access or when our file is actually a slow network resource:

main = do
  withFile "something.txt" ReadMode (\handle -> do
    hSetBuffering handle $ BlockBuffering (Just 2048)
    contents <- hGetContents handle
    putStr contents)

-- We can also use `hFlush`, which is a function that takes a handle and
-- returns an IO action that will flush the buffer of the file associated
-- with the handle. When we're doing line-buffering, the buffer is flushed
-- after every line. When we're doing block-buffering, it's after we've read
-- a chunk. It's also flushed after closing a handle. That means that when
-- we've reached a newline character, the reading (or writing) mechanism reports
-- all the data so far. But we can use `hFlush` to force that reporting of data
-- that has been read so far. After flushing, the data is available to other
-- programs that are running at the same time.

