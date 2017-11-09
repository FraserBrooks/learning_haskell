

-- Chapter 3 Pattern Matching

lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Not lucky"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- where:

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where ((f:_),(l:_)) = (firstname,lastname)

initials' (f:_) (l:_) = [f] ++  "." ++ [l] ++ "."

-- let statements

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in  sideArea + 2 * topArea

pattern_match_let x =
  (let (a,b,c) = (1,2,3) in a+b+c) * x

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

-- case matching

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
                                                [x] -> "a singleton."
                                                xs -> "a longer list."

-- or alternatively:

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton."
          what xs = "a longer list."


-- Chapter 5 Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0  = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  |a == x = True
  |otherwise = a `elem` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
    where smaller = quicksort [a | a <- xs, a <= x]
          bigger = quicksort [a | a <- xs, a > x]









