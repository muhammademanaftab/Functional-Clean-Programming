module MuhammadEmanAftabHW5GR1

import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW5GR1 ( e.g JohnSmithHW5GR1)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 


/* 
1. Given a list of lists, create a list of tuples that contains the index, 
product of all elements in the list, and a boolean value that 
checks if the elements in the list are in increasing order.

EXAMPLE:

Input : [[1, 2, 3], [4, 5], [6, 1, 8], [2, 3, 5, 7], []]
Output : [(0, 6, True), (1, 20, True), (2, 48, False), (3, 210, True),(4,0,True)]
*/

//checkProdAndOrder :: [[Int]] -> [(Int,Int,Bool)]

incr :: [Int] -> Bool
incr [] = True
incr [x] = True
incr [x,y:xs]
| x < y = incr [y:xs]
= False

product :: [Int] -> Int
product [] = 0
product x = foldr (*) 1 x

check :: [[Int]] -> [(Int,Int,Bool)]
check x = [(a,b,c) \\ a <- [0..((length x)-1)] & b <- (map product x) & c <- (map incr x) ]



//Start = check [[1, 2, 3], [4, 5, 5], [6, 1, 8], [2, 3, 5, 7], []] // [(0, 6, True), (1, 100, False), (2, 48, False), (3, 210, True),(4,0,True)]
//Start = check [[2, 4, 6], [1, 3, 5], [10, 20, 30], [3, 2, 1]] // [(0, 48, True), (1, 15, True), (2, 6000, True), (3, 6, False)]
//Start = check [[5, 10, 15], [0, 0, 0], [1, 2, 3, 4, 5]] // [(0, 750, True), (1, 0, False), (2, 120, True)]


/*
2. You are given a list of characters. Your task is to 
split this list into a tuple with three parts:

1. The first part should contain all the consecutive digits ('0' to '9') in the same order 
they appear in the original list.
2. The second part should contain all the consecutive uppercase letters 
('A' to 'Z') in the same order they appear in the original list.
3. The third part should contain all the remaining characters 
(special characters, lowercase letters, etc.) in the same order they appear in the original list.

You need to write a function that performs this split and returns the tuple as described.

EXAMPLE:

Input: ['H', 'e', '1', 'l', '2', 'l', '3', 'o', '4', '!', 'W', 'o', 'r', '5', 'l', 'd']
Output: (['1', '2', '3', '4', '5'], ['H', 'W'], ['e', 'l', 'l', 'o', '!','o', 'r', 'd'])
*/

//splitChars :: [Char] -> ([Char],[Char],[Char])

splitChars :: [Char] -> ([Char],[Char],[Char])
splitChars [] = ([],[],[])
splitChars [x:xs] = (num_list, capi_list, other_list)
    where  num_list = filter (\x= (toInt x) >= 48 && (toInt x) <= 57) [x:xs]
           capi_list = filter (\x= (toInt x) >= 65 && (toInt x) <= 90) [x:xs]
           other_list = filter (\x= (toInt x) < 48 || ((toInt x) > 57 && (toInt x) <65) || (toInt x) > 90) [x:xs]



//Start = splitChars ['H', 'e', '1', 'l', '2', 'l', '3', 'o', '4', '!', 'W', 'o', 'r', '5', 'l', 'd'] // (['1', '2', '3', '4', '5'], ['H', 'W'], ['e', 'l', 'l', 'o', '!','o', 'r', 'd'])
//Start = splitChars ['A', 'B', 'C', '1', '2', '3', '*', 'a', 'b', 'c', 'd', 'e'] // (['1', '2', '3'], ['A', 'B', 'C'], ['*', 'a', 'b', 'c', 'd', 'e'])
//Start = splitChars ['X', 'Y', 'Z', '9', '8', '7', '6', '5', '=', 'a', 'b', 'c'] // (['9', '8', '7', '6', '5'], ['X', 'Y', 'Z'], ['=', 'a', 'b', 'c'])








