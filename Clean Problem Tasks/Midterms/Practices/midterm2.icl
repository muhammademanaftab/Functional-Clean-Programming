module midterm2

import StdEnv 


// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> 
//for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither made it public.
//Students’ regulation of Eotvos Lorand University 
//(ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, 
//it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be 
//dismissal of the student from the University.


/* 1. Armstrong number

 If sum of cubes of each digit of the number is equal to the number itself,
 then the number is called an Armstrong number.
 153 = 1^3 + 5^3 + 3^3
 Given a positive integer number, write a function to determine whether it is
 an Armstrong number or not.
*/

armstrong :: Int -> Int
armstrong 0 = 0
armstrong x = (x rem 10)^3 + armstrong (x/10)

check :: Int -> Bool
check 0 = True
check x = armstrong x == x

//Start = check 153 // True
//Start = check 370 // True
//Start = check 0 // True
//Start = check 12 // False


/* 2. Occurrences

 Given a list of integers, replace every element in the list with its number
 of occurrences in the list.
*/

fc :: [Int] Int -> Int
fc [] _ = 0
fc [x:xs] n
| x == n = 1+ fc xs n
=fc xs n

// [1,2,3,4,1,1,1] 1

replace :: [Int] -> [Int]
replace [] = []
replace x = map (fc x) x


//occNum :: [Int] -> [Int]

//Start = replace [1,1,1,1,2,3,2,5,6,2,2,2,5] // [4,4,4,4,5,1,5,2,1,5,5,5,2]
// Start = occNum [1..5] // [1,1,1,1,1]
// Start = occNum ([1..5] ++ [1..7]) // [2,2,2,2,2,2,2,2,2,2,1,1]
// Start = occNum([7..9] ++ [7..9] ++ [7..9]) // [3,3,3,3,3,3,3,3,3]


/* 3. Gap2
 
 Given a list of numbers, convert the list in such a way that 
 the difference between two consecutive elements is always 2,
 you may have to add numbers in order to achieve that.
 e.g: [1,5,8] = [1,3,5,7,9]
*/

//gap2 :: [Int] -> [Int]
//gap2 [] = []
//gap2 [x,y:xs]
//|x < y = [x] ++ [x+2] ++ gap2 
//=[x]
	//where y = last [x:xs]
//gap2 [x] = [x]
//gap2 [x,y]= abs(x-y)==2=[x,y]
//gap2 [x,y:xs]
//|abs(x-y) == 2 = [x] ++ gap2 [y:xs]
//=[x] ++ [x+2] ++ gap2 [y:xs]
//|x rem 2 <> 0 && not (((abs(x-y))==2)) = [x] ++ [x+2] ++ gap2 [y:xs]
//=[x+1] ++ gap2 xs


//Start = gap2 [1,5,8] // [1,3,5,7,9]
//Start = gap2 [1,5,15] // [1,3,5,7,9,11,13,15]
//Start = gap2 [] 


/* 4. Not Palindrome
 Given a list of lists of integers,
 write a function that gets rid of Palindrome numbers.
 A palindrome number is a number that can be read from left to right or
 from right to left and gets the same number, 
 e.g. 12521 is a palindrome number. 
*/
to_arry :: Int -> [Int]
to_arry 0 = []
to_arry x
|x<10=[x]
|x>10 = to_arry (x/10) ++ [x rem 10] 


ispali :: Int -> Bool
ispali x = y == reverse y && x > 10
	where y = to_arry x

getRidPal :: [Int] -> [Int]
getRidPal x = filter (\x = ispali x <> True) x

getRidPal2:: [[Int]] -> [[Int]]
getRidPal2 [] = []
getRidPal2 [x:xs] = [getRidPal x: getRidPal2 xs] 


//Start = getRidPal2 [[1,2,1111],[151,22,3455]] // [[1,2],[3455]]
//Start = getRidPal2 [[1,222],[151,202,50505]] // [[1],[]]
//Start = getRidPal2 [[],[22]] // [[],[]]


/* 5. Not Primes
 Given a list of integers, write a function which removes the prime numbers   from the list.
 There will be no negative integers and consider the number 1 not a prime.
*/


isPrime :: Int -> Bool
isPrime x = (length [a \\ a <- [1..x] | x rem a == 0]) == 2

//Start= isPrime 1

removeNotPrime :: [Int] -> [Int]
removeNotPrime [] = []
removeNotPrime [x:xs]
| not (isPrime x) = [x] ++ removeNotPrime xs
=removeNotPrime xs



//Start = removeNotPrime [1..10] // [1,4,6,8,9,10]
//Start = removeNotPrime [13..20] // [14,15,16,18,20]
//Start = removeNotPrime [2,4,8,9,10,23] // [4,8,9,10]
//Start = removeNotPrime [] // []


/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!
addTwoNumber x y = x + y
prodTwoNumber x y = x * y
niceTwoNumber x y = x rem y
//

zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]
zipWith func x y = [ func a b \\ a <- x & b <- y]

//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]


/* 7. Collatz conjecture

 Given a positive number greater than 1, return how many iterations does it 
 take for that number to fall down to "2" if we keep applying the
 Collatz equation on it.
 Collatz conjecture equation:
 If the number is even -> x/2
 If the number is odd -> 3x+1
 e.g: input: 10 
      steps: 10 -> 5 -> 16 -> 4 -> 2
      output: 4 recursive calls
*/

count :: Int Int -> Int
count x y 
|x==2=y-1
|isEven x = count (x / 2) (y+1)  
=count ((3*x)+1) (y+1) 

//Start = count 3 0
//collatz :: Int -> Int
//collatz x = count x 0
//Start = collatz 10 // 4
//Start = collatz 20000000 // 144
//Start = collatz 5 // 3
//Start = collatz 0 // "The number must be greater than 1"


/* 8. Good Lists

 Given a list of lists, count how many of the sublists are good lists.
 A list is good if it is empty or its 1st number is odd, 2nd is even, 
 3rd is odd, 4th is even and so on.
 E.g: [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return
 3 as only [], [1,2,3,4] and [9] are "good".
*/

//check_good :: Int [Int] -> Bool
//check_good _ [] = True
//check_good [x:xs]
//|isOdd x && isEven y && check_good xs = True
//=False

//Start= check_good [1,2,3]

//isGood :: Int [Int] -> Int

// Start = isGood [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] // 3
// Start = isGood [[1,2,3,4],[3,4,4],[3,42],[12,2,1,2]] // 2
// Start = isGood [[],[1,2,3,4],[],[8,3,4],[1],[2],[9],[3,4,4]] // 5
// Start = isGood [] // 0


/* 9. Symmetrical lists
 Given a list of lists of integers, write a function 
 which returns a list of symmetrical lists, 
 if the sum of the sublist is greater than 10.
*/




symSumGreater10 :: [[Int]] -> [[Int]]
symSumGreater10 x = map (\x = x ++ reverse x) x 
where fil = filter (\y= sum y > 10) x

//Start = symSumGreater10 [[1,2,3],[3,4,5,6],[4,5,1,2]] // [[3,4,5,6,6,5,4,3],[4,5,1,2,2,1,5,4]]
//Start = symSumGreater10 [] // []
//Start = symSumGreater10 [[1..10],[1,2]] // [[1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]]

 
/* 10. Elements in interval

 Given a list of triple tuples consisting of two integer values and 
 and a list of integers (left,right,[Int]),
 for every tuple return only the elements from the list 
 which positions' are inside the interval [left..right]
 Assume that the indexes are all valid.
*/

//elementInInterval :: [(Int ,Int,[Int])]-> [[Int]]
//elementInInterval 

//Start = elementInInterval [(2,5,[1..10])] //[[3,4,5,6]]
//Start = elementInInterval [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]
//Start = elementInInterval [(4,7,[1,2,3,4,5,6,7,8,9])] //[[5,6,7,8]]

