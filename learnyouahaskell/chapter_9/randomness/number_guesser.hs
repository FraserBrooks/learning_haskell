

import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNum, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStr "Which number in the range of 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNum == number
       then putStrLn "You are correct!"
       else putStrLn $ "Nope, it was: " ++ show randNum
    askForNumber newGen


