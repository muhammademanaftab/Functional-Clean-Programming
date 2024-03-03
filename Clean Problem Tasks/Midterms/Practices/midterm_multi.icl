module midterm_multi
import StdEnv 


/* Please fill in the data required below:
  <Name>
  <Neptun_code>
  Functional Programming: midterm
  2022.October.21
  I declare that this solution is my own work.
  I have not copied or used third party solutions.
  I have not passed my solution to my classmates, 
  neither made it public.
*/


///////////////////////////////// TASK 1 /////////////////////////////////  
/*1- Unique digits - 10 points
    Given an integer n, return the count of all unique digits of n.
    Input: n = 1232
    Output: 2 (only 1 and 3 are unique - appeared only once in n).
    Input: n = 1111
    Output: 0 (There is no unique digit in n.)
*/
to_arr :: Int -> [Int]
to_arr 0 = []
to_arr x = to_arr (x/10) ++ [x rem 10]

count :: Int [Int] -> Int
count n [] = 0
count n [x:xs]
| n == x = 1 + count n xs
= count n xs 

count_unique_digits :: Int -> Int 
count_unique_digits 0 = 0
count_unique_digits x = length (filter (\a= count a (to_arr x) == 1 ) (to_arr x))

//Start = count_unique_digits 1234 // 4
//Start = count_unique_digits 12325332 // 2
// Start = count_unique_digits 111111 // 0
// Start = count_unique_digits 0 // 1


///////////////////////////////// TASK 2 /////////////////////////////////
/*2- Count good lists - 10 points
    Given a list of lists of integer numbers, count the good sublists in 
    the given list. A list is considered to be good if the numbers at 
    even positions are even and the numbers at odd positions are prime.    
    Input:  [[2,2,4,5], [2,3,3,5]]
    Output: 1 (Only the [2,2,4,5] sublist is good as the numbers at 
    0th, 2nd (even) positions are even and the numbers at 1st, 3rd (odd) 
    positions are prime.
*/
isprime :: Int -> Bool
isprime 0 = False
isprime x = ((length [ a \\ a <- [1..x] | x rem a == 0]) == 2)

//Start = isprime 3

is_good :: [Int] -> Bool
is_good [] = False
is_good [x] = isEven x
is_good [x,y] = isEven x && isprime y 
is_good [x,y:xs] = isEven x && isprime y && is_good xs

//Start = is_good [2,2,4,5]

count_good_lists :: [[Int]] -> Int
count_good_lists [] = 0
count_good_lists ls = length (filter (is_good) ls)


//Start = count_good_lists [[2,2,4,5],[2,3,3,5]] // 1
//Start = count_good_lists [[2,23,22],[2,29,22,5],[1,2,3]] // 2
//Start = count_good_lists [[2,2,4,5],[2,2,6,7,8,11,12,17],[12,23,4]] // 3
//Start = count_good_lists [] // 0


///////////////////////////////// TASK 3 /////////////////////////////////  
/*3- Increase by position - 10 points
    Given a list of real numbers, add the position of every number to the number.    
    Input:  [1.0,2.1,3.5,2.0]
    Output: [1.0,3.1,5.5,5.0] (the position of 1.0 is 0 -> 1.0 + 0 = 1.0  
                               the position of 2.1 is 1 -> 2.1 + 1 = 3.1 
                               the position of 3.5 is 2 -> 3.5 + 2 = 5.15
                               the position of 2.0 is 3 -> 2.0 + 3 = 5.0)
*/



incre:: [Real] Real -> [Real]
incre [] _ = []
incre [x:xs] count = [ (x+count) : incre xs (count+1.0)]

//Start= incre [1.0,2.1,3.2] 0.0

inreaseByPos :: [Real] -> [Real]
inreaseByPos [] = []
inreaseByPos ls = incre ls 0.0

// Start = inreaseByPos [1.0,2.1,3.5,2.0] // [1,3.1,5.5,5]
// Start = inreaseByPos [55.12,22.45,2.10,15.1,20.20] // [55.12,23.45,4.1,18.1,24.2]
//Start = inreaseByPos [] // []


///////////////////////////////// TASK 4 /////////////////////////////////  
/*4- Reverse integers - 10 points
    Given a list of integer numbers, reverse every number in the list.
    Reversing a number means to write its digits in the reversed order. 
    Input:  [1,234,5677,43,0]
    Output: [1,432,7765,34,0] Reverse of 1 is 1    
       Reverse of 234: the digits of 234 in reversed order are 4,3 and 2, 
       and by combining these digits we get the number 432
    Note: reverse of e.g. 230 is 32 NOT 032  
*/


conv_num_ls :: Int -> [Int]
conv_num_ls x 
| x>=0 && x<10 =[x]
= [x rem 10] ++ conv_num_ls (x/10) 


conv_ls_num :: [Int] -> Int
conv_ls_num [] = 0
conv_ls_num [x:xs] = (x * (10^((length[x:xs]) - 1))) + conv_ls_num xs


rev_num :: Int -> Int
rev_num 0 = 0
rev_num n = conv_ls_num (conv_num_ls n)


rev_ls_num :: [Int] -> [Int]
rev_ls_num [] = []
//rev_ls_num ls = map (\x= conv_ls_num (conv_num_ls x)) ls
rev_ls_num ls= map rev_num ls


//Start = rev_ls_num [1,234,5677,43,0] // [1,432,7765,34,0]
// Start = rev_ls_num [1..5] // [1,2,3,4,5]
// Start = rev_ls_num [222..240] 
// [222,322,422,522,622,722,822,922,32,132,232,332,432,532,632,732,832,932,42]
// Start = rev_nums [] // []


///////////////////////////////// TASK 5 ///////////////////////////////// 
/*5- Passed students - 10 points
    Given a list of tuples and an integer number (let's call it x), where 
    the first element of the tuple represents a student's name and 
    the second element of the tuple represents the points of the student 
    that he/she got in a particular subject (its type is a list of real numbers).
    Return those students whose points have the following property:
    if the sum of the INTEGER parts of the points is greater than or equal 
    to the given number x.
    Input: [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 
    Output: ["Abdullah"] ( the sum of the integer parts of [55.55,66.55,77.75,65.07,65.57] 
                           = 55 + 66 + 77 + 65 + 65 = 328 >= 320 (the given x)
                           - the sum of the integer parts of [27.55,20.55,10.75,30.07,20.57]
                           = 27 + 20 + 10 + 30 + 20 = 107 < 320 (the given x) )
*/

//passedStudents :: [(String,[Real])] Int -> [String] 

// Start = passedStudents [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 // ["Abdullah"]
// Start = passedStudents [("Sara" , [5.55,44.55,55.75,30.07,90.57]),("Rayan",[56.55,66.55,7.75,77.07,77.57]),("Ali",[1.55,6.55,66.75,6.07,7.57]),("Maria",[54.55,60.55,66.75,20.07,74.57])] 200 
// ["Sara","Rayan","Maria"]
// Start = passedStudents [] 100 // []


///////////////////////////////// TASK 6 /////////////////////////////////  
/*6- Eliminate - 10 points
    Given a list of numbers eliminate the first number of 
    every two numbers in the list, until only one number is left.  
    Input: a = [1, 2, 3, 4, 5, 6, 7, 8, 9]
           a = [2, 4, 6, 8]
           a = [4, 8]
           a = [8] 
*/

eliminate :: [Int] -> [Int]
eliminate [] = []
eliminate [x] = [x]
//eliminate [x,y] = [y]
eliminate [x,y:xs]= ([y] ++ eliminate xs)


//Start = eliminate [1..9] // [8]
//Start = eliminate [1,2,3,4] // [4]
//Start = eliminate [0] // [0]
//
//Start = eliminate [] // []


///////////////////////////////// TASK 7 /////////////////////////////////  
/*7- Delete third - 10 points
    Delete every third element from a list.
*/

//helper :: [Int] Int -> [Int]
//helper [] count = []
//helper [x:xs] count
//| count == 3 = helper xs (count+1)
//=[x: helper xs (count+1)]

//Start= helper [1,2,3,4,5] 1

//del3 :: [Int] -> [Int]
//del3 [] = [] 
//del3 ls = helper ls 1

//Start = del3 [1..7]   // [1,2,4,5,7]
//Start = del3 [1..20] // [1,2,4,5,7,8,10,11,13,14,16,17,19,20]
//Start = del3 [1..5]  // [1,2,4,5]
//Start = del3 []      // []


///////////////////////////////// TASK 8 ///////////////////////////////// 
/*8- Fibonacci lists - 10 points
    Write a function that takes a list of integers and for every integer 
    returns a list of Fibonacci sequence less than or equal to the integer.
    A Fibonacci sequence is a sequence of numbers where each number is 
    the sum of the previous two numbers: 0, 1, 1, 2, 3, 5 ..... and so on
    Input: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    Output: [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],[0,1,1,2,3,5]
            ,[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
*/

fib :: Int -> Int
fib 0 = 0
fib x
|x==0=0
|x==1=1
= fib (x-1) + fib (x-2)

//Start= fib 45


fib_ls :: Int -> [Int]
fib_ls 0 = [0]
fib_ls x =  filter ((>=) x) [fib a \\  a <- [0..x] ]

//Start = fib_ls 88


FibList :: [Int] -> [[Int]]
FibList [] = []
FibList [x:xs] = [fib_ls x : FibList xs]


//Start = FibList [0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  
// [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],
// [0,1,1,2,3,5],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
//Start = FibList [0,45,88,87,21] 
// [[0],[0,1,1,2,3,5,8,13,21,34],[0,1,1,2,3,5,8,13,21,34,55],
// [0,1,1,2,3,5,8,13,21,34,55],[0,1,1,2,3,5,8,13,21]]
//Start = FibList [] // []


///////////////////////////////// TASK 9 /////////////////////////////////  
/*9- Carry-less addition - 10 points
    Given 2 lists of integers of same length, perform carry-less addition 
    between 2 integers at the same position and return the list of integers.
    Carry-less addition: 9+7 results 6, not 16.
                        35+48 results 73, not 83                        
    Here, for the sake of simplicity, there will be only 1-digit integer.
*/

carrylessDigitAddition :: [Int] [Int] -> [Int]
carrylessDigitAddition [] [] = []
carrylessDigitAddition [x:xs] [y:ys] 
| x + y < 10 = [ x + y] ++ carrylessDigitAddition xs ys
= [ (x + y) - 10 ] ++ carrylessDigitAddition xs ys

//Start = carrylessDigitAddition [] []
//Start = carrylessDigitAddition [9,6,3,4,3,4,5] [4,8,9,0,9,6,5] // [3,4,2,4,2,0,0]
//Start = carrylessDigitAddition [7,6,5,9,8,7,6] [9,8,7,6,3,2,9] // [6,4,2,5,1,9,5]
//Start = carrylessDigitAddition [7,8,3,8] [6,2,7,8]             // [3,0,0,6]
//Start = carrylessDigitAddition [2,3,4,3,2] [9,8,-7,6,5]        // [1,1,-3,9,7]


///////////////////////////////// TASK 10 ///////////////////////////////// 
/*10- Zip with LCM - 10 points
    Write a function that takes two lists of integers and returns a 
    list of tuples where the first element of the tuple is an element of 
    the first list and the second element of the tuple is an element 
    of the second list at the same position and the third element is the 
    LCM of the first two elements (LCM - least common multiple of two numbers).
    LCM(2,3) = 6 LCM(3,4) = 12 LCM(12,15) = 60
    If the lists are of different lengths, the function should return a list 
    of tuples of the same length as the shorter list.
*/

helper_lcm :: [Int] [Int] -> [Int]
helper_lcm [] [] = []
helper_lcm [x:xs] [y:ys] = [lcm x y] ++ helper_lcm xs ys

//Start = helper_lcm [4,24] [24,25]

ZipWithLCM :: [Int] [Int] -> [(Int, Int, Int)]
ZipWithLCM [] [] = []
ZipWithLCM [] _ = []
ZipWithLCM _ [] = []
ZipWithLCM [x:xs] [y:ys] = [ (a,b,c) \\ a <- [x:xs] & b <- [y:ys] & c <- (helper_lcm [x:xs] [y:ys])]

//Start = ZipWithLCM [12,14,22,57,66] [13,15,17,19,21] 
// [(12,13,156),(14,15,210),(22,17,374),(57,19,19),(66,21,462)]
//Start = ZipWithLCM [78,43,12,33,65] [32,77,21,11,9,43] 
// [(78,32,1248),(43,77,3311),(12,21,84),(33,11,33),(65,9,585)]
//Start = ZipWithLCM [] [32,77,21,11,9,43] // []
//Start = ZipWithLCM [78,43,12,33,65] [] // []
//Start = ZipWithLCM [] [] // []


///////////////////////////////// TASK 11 ///////////////////////////////// 
/*11- Split number - 10 points
    Write a function that takes a number, splits in the middle and interchanges the 
    two halves. If digits' number is odd, the second half contains the middle digit.
    1234 -> 12 | 34 -> 34 | 12 -> 3412
    12345 -> 12 | 345 -> 345 | 12 -> 34512
*/




helper_fun :: [Int] Int -> [Int]
helper_fun [] n = []
helper_fun [x:xs] n 
| n <= (length [x:xs]/2) = [x] ++ helper_fun xs (n+1)
=[]
//Start= helper_fun [1,2,3,4,5,6,7] 0

//FunNum :: Int -> Int
//FunNum 0 = 0
//FunNum x)
//| (hd (to_arr ( x rem (10 ^ (((length ls) / 2) + 1 ))))) rem 2 == 1 =conv_ls_num ([ls !! ((length ls) /2)] ++ (helper_fun (reverse ls) 0 ) ++ (helper_fun ls 0 ))
//=conv_ls_num ((helper_fun (reverse (to_arr x)) 0 ) ++ (helper_fun (to_arr x) 0 ))
	//where ls = to_arr x

//Start= conv_ls_num([(to_arr 12345) !! (length (to_arr 12345)/2)] ++ (helper_fun (reverse (to_arr 12345)) 0 ) ++ (helper_fun (to_arr 12345) 0 ))
//Start= (to_arr 12345) !! (length (to_arr 12345)/2)
// Start = FunNum 0 // 0
//Start = FunNum 1234 //3412
// Start = FunNum 12345 //34512
// Start = FunNum 123456 //456123


///////////////////////////////// TASK 12 /////////////////////////////////  
/*12- Fold if true - 10 points
    Write function foldiftrue which reduces only those elements of a list which 
    satisfy a given predicate. There are 4 reduce options which are given in String:
    "max" return max number, "min" return min number, "*" return product, "+" return sum. 
*/


//foldiftrue :: (Int -> Bool) String [Int] -> Int

// Start = foldiftrue ((>)5) "max" [6,1,2,3] // 3
// Start = foldiftrue ((>)5) "min" [6,1,2,3] // 1
// Start = foldiftrue (isEven) "+" [6,1,2,3, 233, 287] // 8
// Start = foldiftrue (isEven) "*" [6,1,2,3, 233, 287] // 12


///////////////////////////////// TASK 13 ///////////////////////////////// 
/*13- Salary calculation - 30 points this task has 3 parts, each of 10 points 
    You are given list of tuples with employees' name, age and salaries, do some analysis. 
    Find about all the given queries using functions.
    1. What is the average salary of the employees?
    2. If the employer is to deduct 15% of the salaries of employees younger than 35 years old,
       how much money would he save?
    3. Give only the list of names where the employee is older than 35 but earns more than 300.
    [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), 
     ("Marie", 55, 573), ("Lucy", 37, 400)]
    1. 424.71...
    2. 87.6
    3. ["Bob", "Anna", "Marie", "Lucy"]
*/

//sum :: [Int] -> Int
//sum [] = 0
//sum [x:xs] = x + sum xs 

average :: [Int] -> Real
average [] = 0.0
average ls = (toReal(sum ls)) / (toReal(length ls))

//Start = average [1,2,3,4,5,6]

Ls :: [(String, Int, Int)] -> [Int]
Ls [] = []
Ls [(x,y,z):xs] = [z] ++ Ls xs

//Start=  Ls [("ab",0,1),("cd", 2,3)]

averageSalary :: [(String, Int, Int)] -> Real
averageSalary [] = 0.0
averageSalary [x:xs] = average (Ls [x:xs])

//Start = averageSalary [] 
//Start = averageSalary [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

//savedMoney :: [(String, Int, Int)] -> Real

//Start = savedMoney [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]


str_help :: [(String, Int, Int)] -> [String]
str_help [] = []
str_help [(x,y,z):xs] = [x] ++ str_help xs

namesOlder35 :: [(String, Int, Int)] -> [String]
namesOlder35 [x:xs] = str_help (filter (\(a,b,c) = b> 35 && c > 300) [x:xs])

//Start = namesOlder35 [("John", 23, 200), ("Bob", 60, 700)]

//Start = namesOlder35 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]


//////////////////////////////////////////////////////////////////