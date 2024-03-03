module HW2GR1
import StdEnv

/* When you submit this homework, change the filename to YourFullNameHW2GR1 ( e.g JohnSmithHW2GR1)
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
1. Implement a recursive function countPalindromicNumbers that accepts
an integer n as input and computes the count of 
palindromic numbers within the range from 1 to n. 
A palindromic number is a number that reads the same forwards and backwards. 
For instance, numbers like 121 and 1331 are considered palindromic numbers.
Hint: you may need to use auxiliary function(s)
*/



//countPalindromicNumbers :: Int -> Int


to_arr :: Int -> [Int]
to_arr a 
| a == 0 = []
=[a rem 10 : to_arr (a/10)]


pali :: Int -> Bool
pali  x = y == reverse y 
	where y =  (to_arr x) 


Palind :: Int -> [Int]
Palind 0=[]
Palind n 
| pali n =  [ n : Palind (n-1)]
| otherwise = Palind (n-1)


countPalindromicNumbers:: Int -> Int
countPalindromicNumbers n = length (Palind n) 


//Start = countPalindromicNumbers 100 // 18
//Start = countPalindromicNumbers 8 // 8
//Start = countPalindromicNumbers 1000 // 108




/*
2. Implement a recursive function calculateSumOfSequence that takes
two positive integers start and end as input. 
The function should calculate and return the sum of all positive 
integers in the sequence starting from start until end. If start is greater than end, 
the function should abort and return a message: "Start value cannot be bigger than end value."
If start is equal  to end, the function should return 0.

For example, if start = 3 and end = 7, the function should return 3 + 4 + 5 + 6  = 18.

*/

calculateSumOfSequence :: Int Int -> Int
calculateSumOfSequence x y
| x == y = 0
| x > y  = abort "Start value cannot be greater than end value." 
| x < y  = x + calculateSumOfSequence (x + 1) y

//Start = calculateSumOfSequence 10 18 // 108
//Start = calculateSumOfSequence 3 7 // 18
//Start = calculateSumOfSequence 16 16 // 0
//Start = calculateSumOfSequence 20 9 // "Start value cannot be bigger than end value."

