module midterm4

import StdEnv
/* Instructions
You are given 13 exercises each of 10 points. Min 5 perfect solutions (50 points) is required for passed, max is 100 points, you can choose which ones to solve.

Find on local C: drive the Clean compiler.

Copy the text into a  .icl file, save into your documents folder where you can compile.

Only this browser and compiler should be active during programming.

KEEP this browser open until you submit. MULTIPLE  task  taking IS NOT POSSIBLE.

After you finish coding UPLOAD here the file, JUST .icl file, and PRESS SUBMIT.

After SUBMIT the FILE CAN NOT BE CHANGED, MULTIPLE UPLOADS, MULTIPLE SUBMITS IS NOT POSSIBLE.

 

Your submission should not have any errors when running the code.

It is possible to get partial points for not fully working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add your tests as well.

Don't change the given function signatures, however, you can add as many functions as you wish, just make sure to name them appropriately.

Make sure that you comment all 'Start'-s before submitting the code.

*/



/*---------------------------------------------------------------
-- Functional Programming & mid-term, 2022. Mar. 23.

-- This solution was submitted and prepared by
-- <NAME, NEPTUN> for the mid-term programming assignment of
-- the Functional Programming course.

-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- I have not passed my solution to my classmates, neither made it public.

-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.)
-- states that as long as a student presents another student’s work -
-- or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/


//// Disarium number
/*1.
Given a positive integer number, check if the given number
is a Disarium number or not.
A Disarium number is a number defined by the following:
Sum of its digits powered with their respective position
is equal to the original number.
Example: 135 is a Disarium number, 1^1+3^2+5^3 = 135.
*/


helper_dis :: Int Int -> Int
helper_dis 0 n = 0
helper_dis x  n = (x rem 10) ^ n + helper_dis (x/10) (n-1)



to_arr :: Int -> [Int]
to_arr 0 = []
to_arr x = [x rem 10] ++ to_arr (x/10)


isDisariumNum :: Int -> Bool
isDisariumNum 0 = False
isDisariumNum x 
| helper_dis x (length (to_arr x)) == x = True
=False


//Start = isDisariumNum 135 // True
//Start = isDisariumNum 598 // True
//Start = isDisariumNum 518 // True
//Start = isDisariumNum 220 // False
//Start = isDisariumNum 110 // False


//// Harshad numbers
/*2.
Given a list of positive integer numbers, return a list that
contains the Harshad numbers of the list.
A Harshad number is an integer that is divisible by the sum of
its digits when written in that base.
Examples:
200 True, the sum of digits is 2+0+0=2 and 200 is divisible by 2.
171 True, the sum of digits is 1+7+1=9 and 171 is divisible by 9.
*/

helper_harshad_sum :: Int -> Int
helper_harshad_sum 0 = 0
helper_harshad_sum x = (x rem 10) + helper_harshad_sum (x/10)

//Start = helper_harshad_sum  200 


helper_harshad :: Int -> Bool
helper_harshad 0 = False
helper_harshad x
| x rem (helper_harshad_sum x) == 0 = True
=False


harshadNums :: [Int] -> [Int]
harshadNums ls = filter (helper_harshad) ls

//Start = harshadNums ([8, 9, 10, 12, 18, 20, 21, 24, 27, 30] ++ [13..17]) // [8, 9, 10, 12, 18, 20, 21, 24, 27, 30]
//Start = harshadNums ([31..35] ++ [36, 17,40, 42, 45, 13, 48, 50, 54, 11, 60, 63]) // [36, 40, 42, 45, 48, 50, 54, 60, 63]
//Start = harshadNums [] // []


//// Leader numbers of a list
/*3.
Given a list of integer numbers, return all the leaders in the list.
A number is leader if it is strictly greater than all the elements
to its right side in a list.
Example: [10,9,14,23,15,0,9] -> [23,15,9]
23 is greater than all the numbers to its right 15,0,9.
15 is greater than all the numbers to its right 0,9.
9 there are no numbers in its right.
*/

largest :: [Int] Int -> Bool
largest [] n = True
largest [x:xs] n
| n >= x && largest xs n = True
=False


//Start = largest [15,0,9] 15

leaders :: [Int] -> [Int]
leaders [x:xs] = filter (\a = largest xs a) [x:xs]

//Start = leaders [10,9,14,23,15,0,9] // [23,15,9]
//Start = leaders [1..10] // [10]
//Start = leaders [10,9..1] // [10,9,8,7,6,5,4,3,2,1]
//Start = leaders [7,8,10,9,5,3,6,4] // [10,9,6,4]
//Start = leaders [] // []


//// Replacing
/*4.
Given the list and a number K, remove all numbers that are divisible by K
and replace all other with reminder by K. Return resulting list.
Example: [1,3,8,6,2], K=3 -> [1,2,2]
3 and 6 are removed as they are divisible by 3.
1,8,2 are replaced with 1, 2, 2 reminders.
*/

filteredRem :: Int [Int] -> [Int]
filteredRem _ [] = []
filteredRem n [x:xs]
| x rem n == 0 = filteredRem n xs
= [x rem n] ++ filteredRem n xs

//Start = filteredRem 4 [4,6]
//Start = filteredRem 3 [1,3,8,6,2] // [1,2,2]
//Start = filteredRem 5 [5,10,30] // []
//Start = filteredRem 2 [2,8,3,4,1] // [1,1]
//Start = filteredRem 100 [20,17] // [20,17]


//// GoodNumbers
/*5.
Write a function that takes a list as an argument and
counts how many numbers are:
greater or equal to 10 AND less or equal to 99 AND divisible by 3.
*/

countGoodNums :: [Int] -> Int
countGoodNums [] = 0
countGoodNums [x:xs]
| (x >= 10) && (x <= 99) && (x rem 3 == 0) = 1 + countGoodNums xs
= countGoodNums xs

//Start = countGoodNums []
//Start = countGoodNums [1,12,10,99] // 2
//Start = countGoodNums [12,15,30,33,39,96,99] // 7
//Start = countGoodNums [9, 10, 100, 102, 105] // 0
//Start = countGoodNums [] // 0


//// Valid Triangles
/*6.
Given a list of tuples, each with 3 numbers.
For each tuple check if these 3 numbers
can be used as sides of a triangle,
replace the tuple either with True or False.
3 numbers can be sides of triangles if each pair's
sum is greater than the remaining 3rd number.
A number cannot be a side if it is negative or 0.
*/

helper_tri :: (Int,Int,Int) -> Bool
helper_tri (a,b,c) 
| (a+b > c && a+c > b && c+b > a) && (a<>0 && a > 0)= True
=False

//Start = helper_tri (10,8,3)


validTriangles :: [(Int,Int,Int)] -> [Bool]
validTriangles ls = map (\a = helper_tri a) ls


//Start = validTriangles [] // []
//Start = validTriangles [(3,3,3), (2,4,5), (4,2,5), (3,3,10)] // [True, True, True, False]
//Start = validTriangles [(8,2,4), (3,10,3), (1,2,3)] // [False, False, False]
//Start = validTriangles [(10,8,3), (-10,4,2)] // [True, False]


//// Replicate
/*7.
Given a list of tuples, where each tuple contains a string and a number N.
For each tuple generate a list that contains N copies of the given string.
For negative number N generate empty list.
Example: the tuple ("ab", 3) should be replaced with ["ab","ab","ab"].

*/

help_copy :: (String,Int) -> [String]
help_copy (str,n) 
| n == 0 = []
|n < 0 = []
= [str] ++ help_copy (str, (n-1))

//Start = help_copy ("x", 3)



stringCopy :: [(String,Int)] -> [[String]]
stringCopy [] = []
stringCopy [x:xs] = [help_copy x] ++ stringCopy xs


//Start = stringCopy [("X",3),("AA",2)] // [["X","X","X"],["AA","AA"]]
//Start = stringCopy [("Clean", 1),("?!",0),("Empty",-1)] // [["Clean"],[],[]]
//Start = stringCopy [] // []


//// Integers' insertion
/*8.
Given two integers, insert the second integer into the first one.
After each digit considered in the first integer, insert a digit
from the second integer. Both given numbers are of equal length.
Example: 123 321 -> 132231
13 13 -> 1133
*/

help_arr:: [Int] [Int] -> [Int]
help_arr [] [] = []
help_arr [x:xs] [y:ys] = (flatten [ [a] ++ [b] \\ a <- [x:xs] & b <- [y:ys] ])

con_ls_num :: [Int] -> Int
con_ls_num [] = 0
con_ls_num [x:xs]= x * (10^((length [x:xs])-1)) + con_ls_num xs

//Start = con_ls_num [1,2,3,4,5]

intInsertion :: Int Int -> Int
intInsertion 0 0 = 0
intInsertion x y = con_ls_num ((help_arr (reverse (to_arr x)) (reverse (to_arr y))))

//Start = intInsertion 123 123 // 112233
//Start = intInsertion 123 321 // 132231
//Start = intInsertion 13 13 // 1133
//Start = intInsertion 1 2 // 12
//Start = intInsertion 2 1 // 21


//// Failed-passed students
/*9.
Given list of tuples and an integer value representing the 'pass_marks',
each tuple represents a student (name,marks), write a function which
groups the students into two categories based on their marks obtained in a test.
The function should return a tuple containing the list of the students
who failed, and the list of the students who passed.
Example:
List: [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)], pass_marks: 30
Output: ([("Ramesh",23)],[("Vivek",40), ("Harsh",88), ("Mohammad",60)])
--failed-- -------------passed------------------------
'Ramesh' failed as his marks 23 are less than the given number 30, all others passed.
*/

//group_by_marks :: [(String, Int)] Int -> ([(String,Int)], [(String,Int)])


//Start = group_by_marks [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)] 30
// ([("Ramesh",23)],[("Vivek",40),("Harsh",88),("Mohammad",60)])
//Start = group_by_marks [("Ramesh",50),("Vivek",20),("Harsh",10),("Abdullah",90),("Mohammed",30),("Ahmed",0),("Othman",70)] 50
// ([("Vivek",20),("Harsh",10),("Mohammed",30),("Ahmed",0)],[("Abdullah",90),("Othman",70)])
//Start = group_by_marks [] 1 // ([],[])


//// Ciphering
/*10.
Given a list of characters, extract all the vowels and count them.
After that, cipher the list of characters by that count.
Ciphering here means just shift the character by that count.
English vowels are: a, e, i, o, and u.
Example: let's assume that the vowels' count is 2, then:
'a' + 2 = 'c' ... Here we ciphered 'a' into 'c'
'c' + 2 = 'e' ... We did the same as above
For the input ['m', 'o', 'h', 'i','d','o'] count of vowels is 3 o,i,o.
Cipher of the list: ['m', 'o', 'h', 'i','d','o']->['p','r','k','l','g','r'].
*/
helper_ciph :: [Char] -> Int
helper_ciph [] = 0
helper_ciph [x:xs]
| x == 'a' || x =='e' || x =='i' || x =='o' || x =='u' = 1 + helper_ciph xs
=helper_ciph xs

help_shift :: Char Int -> Char
help_shift chr n = toChar ((toInt chr ) + n )


cipherList :: [Char] -> [Char]
cipherList [] = []
cipherList [x:xs] = map (\a = help_shift a (helper_ciph [x:xs]))[x:xs]

//Start = cipherList []
//Start = cipherList ['m', 'o', 'h', 'i','d','o'] // ['p','r','k','l','g','r']
//Start = cipherList ['t', 'a', 'r', 'i', 'q'] // ['v','c','t','k','s']
//Start = cipherList ['b', 'e', 'k', 'a'] // ['d','g','m','c']
//Start = cipherList ['a','b','d','u','l','l','a','h'] // ['d','e','g','x','o','o','d','k']


//// Reachable points
/*11.
Given coordinates of a source point (x1, y1) determine if it is possible to reach
the destination point (x2, y2). All coordinates are positive.
From any point (x, y) there are only two types of valid movements: (x, x + y)
and (x + y, y). Return a Boolean True if it is possible, else return False.
Example: source point: (2, 10)
destination point: (26,12)
output: True (2, 10)->(2, 12)->(14, 12)->(26, 12) is a valid path.
*/
help_reach :: (Int,Int) Int -> (Int,Int)
help_reach (a,b) n
| n == 5 = (a,b)
| (isEven n)  = help_reach (a+b,b) (n+1)
| (isOdd n)  = help_reach (a,b+a) (n+1)

//Start = help_reach (8,40) 1

help_reach_check :: (Int,Int) Int Int -> Bool
help_reach_check (a,b) x y
|c == x || d == x || c== y || d == y = True
=False
	where (c,d) = (help_reach (a,b) 1)
//Start = help_reach_check (3,12) 20 10

isReachable :: (Int,Int) (Int,Int) -> Bool
isReachable (a,b) (x,y) = help_reach_check (a,b) x y

//Start = isReachable (2, 10) (26, 12) // True
//Start = isReachable (4, 20) (52, 24) // True
//Start = isReachable (8, 40) (104,48) // True
//Start = isReachable (6, 12) (20, 10) // False
//Start = isReachable (3, 15) (58, 69) // False


//// Evaluate
/*12.
Given a list of integer numbers representing coefficients of a polynomial,
the polynomial coefficients are given in increasing order of power.
Implement a function which evaluates the polynomial
according to a given value (substituting the given value into it).
You can assume that the given list is not empty.
Example:
List: [2,3,-5,1], the polynomial is 2x^0 + 3x^1 - 5x^2 + 1x^3.
Given value is 1: 2 + (3 * 1) + (-5 * 1^2) + (1 * 1^3) = 1.
*/
eva_help :: [Int] Int Int -> [Int]
eva_help [] _ _ = []
eva_help [x:xs] y n  = [(x*(y^n))] ++ eva_help xs y (n-1)

//Start = eva_help [1,-5,2,-8] -2 3

evaluate :: [Int] Int -> Int
evaluate [] n = 0
evaluate [x:xs] n = sum (eva_help [x:xs] n (length [x:xs]))

//Start = evaluate []
//Start = evaluate [2,3,-5,1] 1 // 1
//Start = evaluate [1,-5,2,-8] -2 // 83
//Start = evaluate [1,1,1,1,1,1,1,1] 1 // 8

 

//// Moving digit
/*13.
Complete the function Mover that takes three integers: init, digit and target
and calculates the amount of places the digit, that is equal to the digit in
the number init, has to be shifted to the right in order to get the target number.
It is guaranteed that the digit exists in init and has to be shifted to the right.
Example: 134442 3 144423 -> 4
The digit 3 exists in the init number, and it has to be moved 4 places
in order to get the target number.
*/

//move_help :: Int Int -> Int
//move_help 0 _ = 0
//move_help x n 
//| target rem 10 == n = n
//= move_help (target/10) (n+1)_
//	where target = (con_ls_num(reverse (to_arr x)))
//	
//Start = move_help 1234556 4

//Mover :: Int Int Int -> Int


//Start = Mover 123 2 132 // 1
//Start = Mover 134442 3 144423 // 4
//Start = Mover 100020001 2 100002001 // 1

helper_count :: [Int] -> Int
helper_count [] = 0
helper_count [x:xs] = 1 + helper_count xs


help :: [[Int]] -> [Int]
help [] =[]
help x =(map (hd) x)

Start = help [[1,2,3],[4,5,6]] 

