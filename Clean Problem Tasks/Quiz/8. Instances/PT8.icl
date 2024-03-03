module PT8
import StdEnv


/*
Create an * instance of lists such that list1 * list2 will give a
list of the even numbers in each position
if both numbers are odd, delete them
e.g: [1,2,3,4,5,7] * [2,3,4,5,6,7] = [2,2,4,4,6]
*/

helper :: [Int] [Int] -> [Int]
helper [] [] = []
helper _ [] = []
helper [] _ = []
helper [x:xs] [y:ys]
| isOdd x && isOdd y = helper xs ys
| isEven x && isEven y = [x,y:helper xs ys] 
| isEven x = [x:helper xs ys]
| isEven y = [y:helper xs ys]
= helper xs ys



instance * [Int]
where (*) list1 list2  = helper list1 list2

// Start = [1,2,3,4,5,7] * [2,3,4,5,6,7] // [2,2,4,4,6]
// 
Start = [1,5] * [2] // [2]
// sStart = [1,5] * [] // []