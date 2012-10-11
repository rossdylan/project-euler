eulerFib :: (Integral a) => a -> a -> [a]
eulerFib x y
    | y > 4000000 = []
    | even y = [y] ++ (eulerFib y (x+y))
    | otherwise = (eulerFib y (x+y))


main = print (sum $ eulerFib 0 1)


