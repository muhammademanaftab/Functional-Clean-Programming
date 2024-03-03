module MuhammadEmanAftabHW3GR1

import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW3GR1 ( e.g JohnSmithHW3GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here: IJE4R1

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

/*
1. You are given a list of lists, where each inner list contains numbers as integers. 
Create a function that filters out the inner lists containing any even number 
and then merges the remaining lists into a single list, removing any duplicates. 
Hint : you can use built-in functions like filter, flatten
*/

isOdd:: [Int]->[Int]
isOdd [] = []
isOdd [x:xs]
|x rem 2 == 1 = [x:isOdd xs]
=isOdd xs

filterAndMergeNumbers:: [[Int]] -> [Int]
filterAndMergeNumbers x = isOdd (removeDup (flatten (x)))


//Start = filterAndMergeNumbers [[1, 3, 5, 7], [2, 4, 6, 8], [9, 0, 1, 3],[3,7,11,13,15]] // [1,3,5,7,9,11,13,15]
//Start = filterAndMergeNumbers [[2, 4, 6, 8], [10, 12, 14], [3, 5, 7], [15, 15, 17 ,19]] // [3,5,7,15,17,19]





/*
2. You are given a list of integers. Find the second-largest element in the list and
remove all occurrences of it from the list.

*/


largest :: [Int] -> Int
largest [] = 0
largest [x] = x
largest [x:xs] 
|x > largest xs = x
=largest xs

secondLargest :: [Int] -> Int
secondLargest []=0
secondLargest [x:xs]
|(x > (secondLargest xs)) && (x < y)  = x
| x == largest xs = x
= secondLargest xs 
	where y = largest [x:xs]

removeSecondLargest :: [Int] -> [Int]
removeSecondLargest x = filter ((<>) (secondLargest x)) x

//Start = removeSecondLargest [12, 5, 7, 10, 3, 8, 10, 5, 10] // [12,5,7,3,8,5]
//Start = removeSecondLargest [1,2,3,4,5] // [1,2,3,5]
//Start = removeSecondLargest [6,6,6,6,6,6,6] // [] // 6 is largest and second largest element

 