module PT4
import StdEnv 


/*
Split List into Chunks: Write a function that splits a list into chunks of
a specified size. The function should return a list of lists, where each inner list
contains the specified number of elements.
Example:
Input: [1, 2, 3, 4, 5, 6, 7, 8] (chunk size = 3)
Output: [[1, 2, 3], [4, 5, 6], [7, 8]]
*/

splitIntoChunks :: [Int] Int -> [[Int]]
splitIntoChunks [] _ = []
splitIntoChunks ls n =  [take n ls] ++ splitIntoChunks (drop n ls) n

//Start = splitIntoChunks [3,4,5,6,4] 2 //  [[3,4],[5,6],[4]]




//Start = splitIntoChunks [1, 2, 3, 4, 5, 6, 7, 8] 3 //  [[1, 2, 3], [4, 5, 6], [7, 8]]
//Start = splitIntoChunks [3,4,5,6,4] 2 //  [[3,4],[5,6],[4]]
Start = splitIntoChunks [1, 2, 3, 4, 5, 6, 7, 8,9] 3 //  [[1, 2, 3], [4, 5, 6], [7, 8,9]]
//Start = splitIntoChunks [1, 2, 3, 4, 5, 6, 7, 8, 9] 0 // [1, 2, 3, 4, 5, 6, 7, 8, 9]
//Start = splitIntoChunks [] 2 // []



