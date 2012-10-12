import Data.List (concat)

-- Helper function to change an Integer into a string
readNum :: String -> Integer
readNum = read

-- Reverse an integer (543 -> 345)
reverseInt :: Integer -> Integer
reverseInt num = readNum $ (reverse . show) num

-- Generate all of our products of 3 digit numbers
genCombos = concat $ map (\x -> map (*x) [100..999]) [100..999]

-- main function so it can be run
main = print . maximum $ filter (\x -> reverseInt x == x) genCombos
