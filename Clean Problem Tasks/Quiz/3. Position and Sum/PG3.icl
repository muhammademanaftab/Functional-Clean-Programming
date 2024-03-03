module PG3
import StdEnv


sum :: [Int] -> Int
sum [] = 0
sum [x:xs] = x + sum xs

checkEqual :: [[Int]] -> [Bool]
checkEqual [] = []
checkEqual [x:xs] = [(sum x == x ) : checkEqual xs]



Start = checkEqual [[1,2,3,-1,-2,-3], [1,2,3,0,-2,-3], [1,2,3,1,-2,-3]]


