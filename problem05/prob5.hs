-- Divisability check (see if a number is divisable by all numbers from 1..20
checkDivisability :: Integer -> Bool
checkDivisability num = (length $ filter (\x -> num `mod` x == 0) checkList) == 20
    where
        checkList = [1..20]

-- Problem 5 solution (We increment by 20 for more speed (i guess)
problem5 = take 1 $ filter (checkDivisability) $ map (*20) [1..]

main = print problem5

