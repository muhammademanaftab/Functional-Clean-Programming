module HW6GR1


import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW6GR1 ( e.g JohnSmithHW6GR1)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

/*
1. You are given a list of lists, each containing tuples of integers.
 For each sublist, find the tuple with the maximum sum of its first and second elements. 
 Then, add these selected tuples together to find the sum of their first and second elements.
 
 Example :
 Input : [[(1, 2), (2, 3), (10, 12), (5, 8)], [(0, 1), (3, 3), (9, 11)]]
 Output: (19,23)
 
 explanation: we have 2 lists inside outer list. tuple with maximum sum of its first and 
 second elements in the first list and second list  are (10,12) and  (9,11) respectively. 
 when we add these numbers together it becomes (19,23)
*/

large :: [Int] -> Int
large [] = 0
large [x:xs]
| x > large xs = x
= large xs

tuple_list :: [(Int,Int)] -> [Int]
tuple_list [] = []
tuple_list [(a,b):xs] = [a+b] ++ tuple_list xs

//Start  = tuple_list [(1,2), (3,4)]

tuple_find :: [(Int,Int)] -> (Int,Int)
tuple_find [x:xs] = hd (filter (\(a,b) = a+b == lrg) [x:xs])
	where lrg = (large (tuple_list [x:xs]))

//Start = tuple_find [(0, 1), (3, 3), (9, 11)]


findMaxAndSum_helper :: [[(Int,Int)]] -> [(Int,Int)]
findMaxAndSum_helper [] = []
findMaxAndSum_helper [x:xs] =  [tuple_find x : findMaxAndSum_helper xs]

findMaxAndSum :: [[(Int,Int)]] -> (Int,Int)
findMaxAndSum ls = ((fst a + fst b),(snd a + snd b))
	where a = (findMaxAndSum_helper ls) !! 0
	      b = (findMaxAndSum_helper ls) !! 1
 

//Start = findMaxAndSum [[(1, 2), (2, 3), (10, 12), (5, 8)], [(0, 1), (3, 3), (9, 11)]] // (19,23)
//Start = findMaxAndSum [[(5, 5), (1, 9), (8, 3)], [(2, 8), (7, 7)]] // (15,10)
//Start = findMaxAndSum [[(3, 6), (2, 4), (7, 1)], [(5, 5), (8, 2)]] // (8,11)
//Start = findMaxAndSum [[(1, 3), (5, 5), (4, 9)], [(8, 6), (2, 2)]] // (12,15)

/*
2. You are given a list of tuples of integers, where each tuple is in the form (a, b, c). 
For each tuple, if the sum of the first two elements (a + b) is not greater than the third element (c),
eliminate that tuple from the list. If the sum (a + b) is greater than the third 
element (c), repeat that tuple in the output list a + b - c times.

Example:
Input: [(1, 3, 5), (2, 7, 8), (4, 2, 6), (9, 6, 10), (5, 5, 10)]
Output : [(2, 7, 8), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10)]

explanation: (1, 3, 5),(4, 2, 6),(5, 5, 10) are eliminated from list because
a+b is smaller or equal to c. (2,7,8) is repeated 1 times because 2+7-8 =1, 
(9,6,10) is repeated 5 times because 9+6-10 = 5.

*/

checker :: (Int,Int,Int) -> Bool
checker (a,b,c)
| (a+b) > c = True
=False

fil_check :: [(Int,Int,Int)] -> [(Int,Int,Int)]
fil_check ls = filter (\a = checker a) ls


Repeat_ind :: (Int,Int,Int) -> Int
Repeat_ind (a,b,c) =  abs (a+b-c) 


Repeat :: (Int,Int,Int) Int -> [(Int,Int,Int)]
Repeat _ 0 = []
Repeat x n
|n>0 = [x] ++ Repeat x (n-1)
=[]

eliminateAndRepeat :: [(Int,Int,Int)] -> [(Int,Int,Int)]
eliminateAndRepeat [] = []
eliminateAndRepeat ls =  flatten[(Repeat x (Repeat_ind x))  \\  x <-  [a \\ a <- (fil_check ls)]   ] 

//Start = eliminateAndRepeat [(1, 3, 5), (9, 6, 10), (4, 2, 6)]

//Start  = eliminateAndRepeat [(1, 3, 5), (2, 7, 8), (4, 2, 6), (9, 6, 10), (5, 5, 10)] // [(2, 7, 8), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10)]
//Start = eliminateAndRepeat [(3, 4, 7), (5, 6, 10), (2, 2, 3), (8, 5, 11), (7, 7, 13)] // [(5,6,10),(2,2,3),(8,5,11),(8,5,11),(7,7,13)]
//Start = eliminateAndRepeat [(2, 2, 4), (3, 5, 9), (7, 6, 13), (8, 6, 12)] // [(8,6,12),(8,6,12)]
//Start = eliminateAndRepeat [(1, 1, 3), (2, 2, 5), (3, 4, 8), (4, 3, 7)] // []