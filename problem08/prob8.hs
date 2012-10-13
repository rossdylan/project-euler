import System.IO

readInt :: String -> Int
readInt = read

consecProds :: String -> [Int]
consecProds "" = [0]
consecProds str = [product $ map (\x -> readInt [x]) (take 5 str)] ++ consecProds (tail str)

main = do
    file <- readFile "1000.digits"
    let huge = lines file !! 0 in
        print $ maximum $ consecProds huge



