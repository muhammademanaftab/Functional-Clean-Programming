module midterm3

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


/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 × 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/
to_arry :: Int -> [Int]
to_arry 0 = []
to_arry x =  to_arry (x/10) ++ [x rem 10] 
//Start = length (to_arry 41)

conv_ls_num :: [Int] -> Int
conv_ls_num [] = 0
conv_ls_num [x:xs] = (x * (10^((length[x:xs]) - 1))) + conv_ls_num xs

parasitic :: Int Int -> Bool
parasitic 0 n = False
parasitic x  n = x * n == ((x rem 10) * (10 ^ ((length (to_arry x))-1))) + conv_ls_num (init (to_arry x))



//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
//Start = parasitic 105263157894736842 2 // True


/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/

count :: Int -> Int
count 0 = 0
count x 
| (x rem 10) == 1 = 1 + count(x/10) 
=count (x/10)

//Start= count 121


doubleOne :: [Int] -> [Int]
doubleOne [] = []
doubleOne x = filter (\a= count a > 1) x
// Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
// Start = doubleOne [12,1,11,33] // [11]
// Start = doubleOne [11,111,21] // [11,111]
//Start = doubleOne [] // []
// Start = doubleOne [21,3,1] // []


/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiple :: Int -> [Int]
multiple 0 = []
multiple n = [a*n \\ a <- [2..n] ]

//Start = multiple 5

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [4]
//Start = multiple 1 // []


/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/

dif :: [Int] [Int] -> [Int]
dif setA setB = [a \\  a<-setA   | not (isMember a setB)]

difference :: [[Int]] [[Int]] -> [[Int]] 
difference [] [] = []
difference [x:xs] [y:ys] = [dif x y] ++ difference xs ys 

//Start = difference [[1,2,3,4,5]] [[4,5,6,7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 

 
/* 5. Replace middle

 Given a list of lists of integers and an integer, write a function that replaces 
 
 the middle element with the given integer in every sublist. 
*/

rep :: [Int] Int -> [Int]
rep [] n = []
rep x n = (take (((length x)/2)) x) ++ [n] ++ (drop (((length x)/2)+1) x)

repMid :: [[Int]] Int -> [[Int]]
repMid [] n = []
repMid [x:xs] n = [rep x n] ++ repMid xs n 

//Start = repMid [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = repMid [[1..6], [9,8..1], [(-1),(-2)..(-10)]] 5 
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
//Start = repMid [[1,3],[]] 5 // [[1,5],[5]]
//Start= repMid [] 5


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/

prime_7 :: Int -> Bool
//prime_7 0 = False
prime_7 x = length ([ a \\ a <- [1..x] | x rem a == 0 && x rem 10 == 7 ]) == 2
//Start= prime_7 17

primes7 :: [Int] -> [Int]
primes7 ls = filter prime_7 ls 

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []


/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/

holdsTrue :: [(Int, Int)] -> Bool
holdsTrue [] = False
//holdTrue ls = [ a \\ a <- ls | isEven (fst a) && isOdd (snd a) ] 
//holdsTrue ls =and (map (\(x,y) = isEven x && isOdd y) ls)
holdsTrue ls =and (map (\x = isEven (fst x) && isOdd (snd x)) ls)

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue [] // False


/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/

superfr :: Int -> Int
superfr 0 = 0
superfr x = (x rem 10) + (superfr (x/10))

supersr :: Int -> Int
supersr 0 = 0
supersr x 
| x > 10 = supersr (superfr x)
= x


super_digit :: [Int] -> [Int]
super_digit [] = []
super_digit [x:xs] = [supersr x] ++ super_digit xs


//Start = super_digit [148148148 , 9875 ] // [3,2]
//Start = super_digit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = super_digit [] // [] 


/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/
helper :: Int Int Int -> Bool
helper x 1 _ = False
helper 1 n _ = True
helper x n y
| n^y > x = False
| x == n^y = True 
| n^y < x = helper x (n) (y+1)

helper2 :: Int Int -> Bool
helper2 base targ 
|base == 0 = False
|targ <> 1 && base == 1= False
|targ == 0 = False
|targ == 1 = True
|otherwise = helper2 (base) (targ/base)

//Start= helper2 0 9




powersList :: [Int] Int -> [Int]
powersList ls n = filter (\x = helper x n 1) ls

//Start = powersList [2,4,8,16,10,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]


/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

prime_gen :: Int Int -> [Int]
prime_gen 0 0 = []
prime_gen x y = [a \\ a <- [x..y] | (length [ c \\ c <- [1..a] | a rem c ==0 ] == 2)]

//Start= prime_gen 0 2

pair :: [Int] -> [[Int]]
pair [] = []
pair [x] = []
pair [x,y] 
|abs (x-y) == 2 = [[x,y]]
=[]
pair [x,y:xs] 
|abs(x-y) == 2 = [[x,y]] ++ pair [y:xs] 
=pair [y:xs] 

//Start = pair (prime_gen 0 2)

twinPrimes :: Int Int -> Int
twinPrimes _ 0 = 0
twinPrimes fs sc = (length (pair (prime_gen fs sc)) )

//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5
//Start = twinPrimes 0 -5 // 0

