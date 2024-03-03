module midterm1 

import StdEnv

// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students’ regulation of Eötvös Loránd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.



/*1. List ends
 Given a list of lists, append to the end of every sublist 
 the sum and the length of the sublist
*/

append :: [[Int]] -> [[Int]]
append x = map (\x = x ++ [sum x] ++ [foldr (+++) "" x ]++ [length x] ) x

//Start = append [[1..5],[1..4],[],[5,6]]  // [[1,2,3,4,5,15,5],[1,2,3,4,10,4],[0,0],[5,6,11,2]]

//Start = append [[(-1),(-2)..(-10)],[12],[5]]  // [[-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-55,10],[12,12,1],[5,5,1]]
//Start = append []  // []

/* 2. Fractions
 
 Given a list of real numbers, keep only the fraction part of the number
*/

fraction :: [Real] -> [Real]
fraction [] = []
fraction [x:xs]
|(x*10.0) > 10.0 = [(toReal((toInt (x*10.0)) rem 10)) / 10.0] ++ fraction xs
|(x*10.0) < 10.0 = [x] ++ fraction xs

//Start= (toReal((toInt(1.2*10.0)) rem 10))/10.0



//Start = fraction [1.2,1.5,0.6] //[0.2,0.5,0.6]
//Start = fraction [1.25, 8.2115548896, 53.21,45.58,0.005] //[0.25,0.2115548896,0.21,0.58,0.00005]
//Start = fraction [] // []


/*3. Famous nums

 Given a list of integers, write a function which gets rid of the numbers that is occurring
 less than 5 times in the list.
*/

//count :: [Int] -> Bool
//count []=0
//count [x:xs] = length (filter ((==) x) xs) == 5 
//|x==y= 1 + count xs
//=count xs
//r
//where y= removeDup x

//Start= count [1,2,1,1,1,3]

fc :: [Int] Int -> Int
fc [] _ = 0
fc [x:xs] n 
| n == x = 1 + fc xs n
= fc xs n

famousNum :: [Int] -> [Int]
famousNum x = filter (\a = fc x a >= 5) x

//Start = famousNum [1,1,1,1,1,1,2,3,4,4,4,4,5,5,5,5,5] // [1,1,1,1,1,1,5,5,5,5,5]
//Start = famousNum [] // []
//Start = famousNum [1,2,3,4,5,6,1,1,1,2,2,2,2,1,1,5,10,3] // [1,2,1,1,1,2,2,2,2,1,1]




/*4. Search
 
 Implement a search algorithm that searches through a list for Int n and returns the value in the list before n. 
 If there is no value, or the list is empty, return -1. e.g., findPrev 5 [1,2,3,4,5,6] should return 4, 
 while findPrev 5 [0, 10, 20, 30] returns -1.
*/

findPrev :: Int [Int] -> Int 
findPrev n [] = -1
findPrev n [x] = -1
findPrev n [x,y:xs]
| isMember n [x,y:xs] == False = -1
|n == y = x
= findPrev n [y:xs]


 

Start = findPrev 5 [1,2,3,4,5,6] // 4
//Start = findPrev 1 [1,2,3,4,5,6] // -1
//Start = findPrev 1 [] // -1 

 

/* 5. Symmetric difference 

 Given two lists of integer numbers , return a sorted list containing the symmetric difference of the two lists; 
 The symmetric difference of two lists A and B is the list (A – B) U (B – A); 
 where A - B is The difference of two lists  defined as follows:  
 The List A-B consists of elements that are in A but not in B.
 And (U) the union of two lists is a list containing all the elements of A and B without duplicates 
*/

//simdif :: [Int] [Int] -> [Int]
//simdif [] _ = []
//simdif [x:xs] [y:ys]
//|x==y= simdif xs [y:ys]
//=[x] ++ simdif xs [y:ys]
 

simdif :: [Int] [Int] -> [Int]
simdif setA setB = AminusB ++ BminusA 
    where AminusB = [a \\  a<-setA | not (isMember a setB)]
    	  BminusA = [b \\  b<-setB | not (isMember b setA)]

//Start = simdif  [1,2,3,4,5] [2,4,6] //  [1,3,5,6]
//Start = symmetricDif  [1..5] [1..10] // [6,7,8,9,10]
//Start = simdif [1..5] [] // [1,2,3,4,5]



/*6. Not N

 Given a list of integers and an integer N, 
 eliminate from the list elements that are positioned before N in the list and are not equal to N,
 then compute the biquadrate of the numbers left in the list.
*/

notN :: Int [Int] -> [Int]
notN _ [] = [] 
notN num list = map (\x=x^4) ((takeWhile (\x=x<>num) list))

//Start = notN 3 [1..5] // [1,16]
//Start = notN 2 []  // []
//Start = notN 6 [10,8..1] // [10000,4096]




/* 7.  Gap2 continued 

 Given a list of numbers, return True if the  
 the difference between two consecutive elements is always 2
 otherwise return False
*/



//famnum :: [(Int,Int)] -> [Int]
//famnum x = flatten (map (\(x,y)= [x,y])  x) 

//Start = famnum [1,1,1,1,1,1,2,3,4,4,4,4,5,5,5,5,5] // [1,1,1,1,1,1,5,5,5,5,5]
//Start = famnum [] // []
//Start = famnum [1,2,3,4,5,6,1,1,1,2,2,2,2,1,1,5,10,3] // [1,2,1,1,1,2,2,2,2,1,1]




gap2C :: [Int] -> Bool
gap2C [] = False
gap2C [x] = False
gap2C [x,y] = abs (x-y) ==2
gap2C [x,y:xs]
|abs (x-y)==2 && gap2C [y:xs] = True
= False

//Start = gap2C [1,3,5,7] // True
//Start = gap2C [1,3,5,7,9,11,13,15] // True
//Start = gap2C [1,5,8] // False
//Start = gap2C [] // False




/* 8. Good Lists
 Given the list of lists and a list of unique numbers. 
 Numbers that are given in this second unique number list are considered to be good numbers. 
 A List is considered good if at least half of its numbers are good. Count how many good lists 
 are in the given list of lists.

 Ex. If you are given [[1,2,3], [1,3,3,4,9,6], [3..6]]  and [1,2,3], good numbers are 1, 2 and 3. 
 First list [1,2,3] has 3 good numbers out of total 3 numbers, hence it is good. 
 Next one [1,3,3,4,9,6] has 3 good numbers (1,3,3) which is half of total length, hence it is a good one as well.
 Last list [3..6] has only one good number and is not a good list. Therefore, answer for this example is 2.
*/
//count :: [Int] Int -> Int
//count [] n = 0
//count [x:xs] n 
//|x==n = 1 + count xs n
//=count xs n

//Start= count [1,2,3,4,1,1] 1

checker :: [Int] [Int] -> Bool
checker _ [] = True
checker good_list [x:xs] = length (good_elements) >= half_len
	where half_len = (length [x:xs]) / 2
		  good_elements = [a \\ a<-[x:xs] | isMember a good_list]

//
goodLists :: [[Int]] [Int] -> Int
goodLists [] good_list = 0
goodLists [x:xs] good_list = length (filter (checker good_list) [x:xs])


//goodLists x y= map (\a = map (count x) a ) y

// Start = goodLists [[1,2,3], [1..6], [3..6]] [1,2,3] // 2
// Start = goodLists [[1], [1..6], [3,8,5]] [1,2,3,8] // 3
// Start = goodLists [[], [3,2,5], [1,1,2,2]] [1] // 2
// Start = goodLists [] [1,2,3] // 0


/*9. CoPrimes
 Given 2 numbers, check if they are co-prime.
 Numbers are called co-prime if they do not have
 common divisor.
*/


//check :: [Int] [Int] -> Bool
//check x y = [a \\a <-x | not(isMember a y)]
//Start= check [1..5] [5..10]

div :: Int -> [Int]
div x = [a \\ a <- [2..x] | x rem a == 0 ] 


coPrimes :: Int Int -> Bool
coPrimes x y = and [ not (isMember a b) \\ a <- div x]
	where b = div y  
      

//Start = coPrimes 12 9 // False
//Start = coPrimes 12 12 // False
// Start = coPrimes 12 13 // True
//Start = coPrimes 5 7 // True


/* 10. Clean Sequence
 The Clean sequence is defined in following way:
 s(0) = a
 s(1) = b
 s(2) = c
 and for every k greater than 2:
 s(k) = ( s(k-1)*s(k-2) + s(k-3) ) rem 1000
 
 Given n, a, b and c - generate first n numbers from Clean sequence.
*/

//Start= ((3)*79+123) rem 1000

func :: Int Int Int Int -> Int
func k a b c 
| k == 0 = a
| k == 1 = b
| k == 2 = c
= (func (k-1) a b c)*(func (k-2) a b c)+(func (k-3) a b c)  

clean :: Int Int Int Int -> [Int]
clean -1 a b c  =  []
clean n a b c  = clean (n-1) a b c ++ [func n a b c] 

//Start = clean 4 123 79 3

//Start = clean 5 1 2 3 // [1,2,3,5,11]
// Start = clean 11 123 79 3 // [123,79,3,720,957,117,157,126,495,277,647]
// Start = clean 2 1 2 3 // [1,2]
// Start = clean 1 1 2 3 // [1]


