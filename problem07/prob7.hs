--isqrt taken from a forum online
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

-- very but not entirely naive factoring system
-- It gets all the factors of n up to the sqrt(n)
-- used in the smarter factorer

naiveFactors num
    | odd num = filter (\x -> num `mod` x == 0) [1,3..isqrt num]
    | otherwise = filter (\x -> num `mod` x == 0) [1..isqrt num]

-- Smart factoring of a number
-- generates the factors of n up to sqrt(n) and then divides n by each to
-- produce the pairs of factors.
smarterFactors :: (Integral a) => a -> [a]
smarterFactors num = baseFactors ++ map (\x -> num `quot` x) baseFactors
    where baseFactors = naiveFactors num

-- Primality test using the smarterFactors function
prime :: (Integral a) => a -> Bool
prime num
    | num == 2 = True
    | even num = False
    | otherwise = (length $ smarterFactors num) == 2


-- Get the 10001st prime number
problem7 = filter prime [1..] !! 10001

-- Dat print statement
main = print problem7
