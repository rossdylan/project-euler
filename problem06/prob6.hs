-- Functions to find the sum of squares and the square of sums
sumOfSquares max = sum $ map (^2) [1..max]
squareOfSums max = (sum [1..max]) ^2

-- actual problem solution (finds the difference)
problem6 = (squareOfSums 100) - (sumOfSquares 100)

-- main function so it can be run with ./
main = print problem6
