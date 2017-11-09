
-- example of `catch` to catch IO exceptions:

import System.Environment
import System.IO
import System.IO.Error

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "Lines in file = " ++ show (length (lines contents))

handler :: IOError -> IO ()
handler e = putStrLn "OOPSIE DOODLE! NO SUCH FILE EXISTS"


