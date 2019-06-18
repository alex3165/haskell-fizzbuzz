import Data.List

filterComparison :: String -> (String -> Bool)
filterComparison x y = x == y

countString :: [String] -> String -> Int
countString l str =
  length (filter (filterComparison str) l)

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

convertNum :: Int -> String
convertNum l =
    if l `mod` 3 == 0 && l `mod` 5 == 0
        then "fizzbuzz"
        else if substring "3" (show l)
          then "lucky"
        else if l `mod` 3 == 0
          then "fizz"
        else if l `mod` 5 == 0
          then "buzz"
        else show l

makeSummary :: [String] -> String
makeSummary i =
  let nFizz = countString i "fizz"
      nBuzz = countString i "buzz"
      nFizzBuzz = countString i "fizzbuzz"
      nLucky = countString i "lucky"
  in "fizz: " ++ show nFizz ++ " " ++ "buzz: " ++ show nBuzz ++ " " ++ "fizzbuzz: " ++ show nFizzBuzz ++ " " ++ "lucky: " ++ show nLucky

main =
  let res = [convertNum x | x <- [1..20]]
      summary = makeSummary res
  in putStrLn ((intercalate " " res) ++ " " ++ summary)
