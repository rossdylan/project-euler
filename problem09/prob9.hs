genTrips max = take 1 $ filter (\lst->(lst !! 0)^2 + (lst !! 1)^2 == (lst !! 2)^2) [[a,b,c] | a<-[1..max], b<-[1..max], c<-[1..max], a+b+c==1000]

solve trip = foldr (*) 1 trip

main = print . solve $ concat $ genTrips 500


