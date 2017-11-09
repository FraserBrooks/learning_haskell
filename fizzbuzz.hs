

build_result :: (Num a, Integral a, Eq a) => a -> [(a, String)] -> String
build_result _ [] = ""
build_result k ((n,s):tl) = if k `mod` n == 0
                                  then s ++ rest
                                  else rest
                                  where rest = build_result k tl

aux :: (Num a, Show a) => a -> String -> String
aux a [] = show a
aux _ str = str

fizzbuzz' :: (Num a, Eq a, Integral a, Show a) => [(a,String)] -> a -> String
fizzbuzz' ls 0 = []
fizzbuzz' ls k = entry ++ "\n" ++ fizzbuzz' ls (k-1)
                where entry = aux (101-k) (build_result (101-k) ls)


main :: IO ()
main = putStr (fizzbuzz' [(3,"Fizz"),(5,"Buzz")] (100))


