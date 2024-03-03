module endterm
import StdEnv
/* Functional Programming endterm C, 2023. Dec 14.
-- This solution was submitted and prepared by
WRITE NAME: MUHAMMAD EMAN AFTAB AND NEPTUN IJE4R1: !! <name, neptun>
-- for the Functional Programming course.
-- I declare that this solution is my own work.
-- I have not copied or used third-party solutions.
-- I have not passed my solution to my classmates, neither made it public. */
/*---------------*/
/* 1.
Create a function that takes an array of 
strings and returns a tuple for each string,
separating the vowels and consonants.
Vowels are 'a', 'e', 'i', 'o', and 'u'; 
all other alphabetic characters are 
considered consonants.
*/

make_tup :: String -> (String,String)
make_tup str=  ((toString [ c \\ c <-ls_chr | (isMember c ['a','e','i','o','u'])]),(toString [ c \\ c <-ls_chr | not(isMember c ['a','e','i','o','u'])]))
	where ls_chr = [a\\a<-:str]


splitVowelsCons :: {String} -> {(String, String)}
splitVowelsCons str_arr = { make_tup x \\ x<-ls_arr}
	where ls_arr = [a \\ a<-:str_arr]

//Start = splitVowelsCons {"Mountain", "Stream", "Forest", "Cloud"} // {("ouai", "Mntn"), ("ea", "Strm"), ("oe", "Frst"), ("ou", "Cld")}
//Start = splitVowelsCons {"Python", "Haskell", "Java", "CSharp"} // {("o", "Pythn"), ("ae", "Hskll"), ("aa", "Jv"), ("a", "CShrp")}
//Start = splitVowelsCons {"aeiou", "bcd", "xyz", "qrst"} // {("aeiou",""), ("", "bcd"), ("", "xyz"), ("", "qrst")}
/*---------------*/
/* 2.
Implement a function called count that takes 
a string as input and counts the repeated 
characters in the string. The function should return 
tuples where the first elements are the repeated 
characters, and the seconds are their respective counts.
Eg. Input: "hellotherehowareyou"
Output: {('h',3),('e',4),('l',2),('o',3),('r',2)}
*/

helper_count :: String Char -> Int
helper_count str chr = length[1 \\ a<-:str | a==chr]

count :: String -> {(Char,Int)}
count str = { s \\ s<- ls_str }
	where ls_str = removeDup[ (a,helper_count str a)  \\ a <-:str | (helper_count str a)>1 ]

//Start = count "hellotherehowareyou" // {('h',3),('e',4),('l',2),('o',3),('r',2)}
//Start = count "programmingisfun" // {('r',2),('g',2),('m',2),('i',2),('n',2)}
//Start = count "abcde" // {}
/*---------------*/
/* 3.
Given an array of integers, write a function 
that transforms the array by excluding all 
occurrences of unique elements that appears 
only once and multiplies others by 2. 
*/


occ :: {Int} Int -> Int
occ arr_num n = length [1 \\ a<-:arr_num | a==n]

//Start = occ {1,2,3,1,1} 1

exclude :: {Int} -> {Int}
exclude num_arr = { x*2 \\ x<-:num_arr | (occ num_arr x) >1}


//Start = exclude {1, 4, 5, 3, 3, 2, 4, 5, 1, 6, 7} // {2,8,10,6,6,8,10,2}
//Start = exclude {2, 2, 3, 4, 4, 5, 5, 6, 6} // {4,4,8,8,10,10,12,12}
//Start = exclude {10, 10, 20, 30, 40, 50, 50} // {20,20,100,100}
/*---------------*/
/*
 4. Note this task has 4 parts each of 5 points!
Write four instances for two lists of integers:


1. '*' - Takes two lists of integers and creates a 
new list by pairing the corresponding elements and 
applying a function to each pair. 
The function takes two integers and returns the 
sum of their squares. If the lists have different 
lengths, use the shortest.
Ex: [2,3,4] * [1,2,3,5] = [5,13,25]
*/





/*
2. '+' - Takes two lists of integers and creates a 
new list by adding each element in the first list 
to the sum of all elements in the second list. 
If the lists have different lengths, add the sum 
of remaining elements of longer list to last element 
of new list 
Ex: [1, 2, 3] + [4, 5, 6, 7, 8] = [5, 7, 24]



3. '/' - Takes two lists of integers and creates a 
new list by dividing each element in the first list 
by the corresponding element in the second list, 
plus the modulo of the two elements. If the lists 
have different lengths, use the shortest. 
Ex : [10, 25, 30] / [3, 5, 4] = [4, 5, 9]



4. '^' - Takes two lists of integers and creates a 
new list by exponentiating each element in the first 
list to the power of the corresponding element in the 
second list. If the lists have different lengths, use 
the shortest. Additionally, if the exponentiation 
results in an odd number, add 1; 
if it results in an even number, subtract 1.
Ex : [2, 3, 4] ^ [3, 2, 0] = [7, 10, 2]
*/
// write instances here


helper_mul :: [Int] [Int] -> [Int]
helper_mul ls1 ls2 = [ ((a^2)+(b^2)) \\ a<-ls1 & b<-ls2 ]

instance * [Int]
	where (*) a b = helper_mul a b
	

helper_add :: [Int] [Int] -> [Int]
helper_add ls1 ls2 
| length ls1 == length ls2 = [a+b \\ a<-ls1 & b<-ls2]
| length ls1 > length ls2 = [a+b \\ a<-ls1 & b<-ls2] ++ [sum ls1]
= [a+b \\ a<-ls1 & b<-ls2] ++ [sum ls2]

	
instance + [Int]
	where (+) a b = helper_add a b
	
	
helper_div :: [Int] [Int] -> [Int]
helper_div ls1 ls2 = [((a/b) + (a rem b))  \\ a<-ls1 & b<-ls2]
	
instance / [Int]
	where (/) a b = helper_div a b	


helper_power :: [Int] [Int]
helper_power ls1 ls2 = [ ((a^b)+1) \\ a<-ls1 & b<-ls2 | (isOdd a^b)] ++ [[ ((a^b)) \\ a<-ls1 & b<-ls2 | (isEven a^b)]


instance ^ [Int]
	where ^ a b = helper_power a b
	

//Start = [2,3,4] * [1,2,3,5] // [5,13,25]
//Start = [4, 5, 6, 7, 8] + [1, 2, 3] // [5,7,24]
//Start = [10, 25, 30] / [3, 5, 4] // [4,5,9]
//Start = [2, 3, 4] ^ [3, 2, 0] // [7,10,2]
/*---------------*/
/* 5.
Write a function that takes an array of employees and 
returns the name of the employee who has the earliest 
(lowest) joining year. Assume that the array is not empty.
*/
             
:: Employee = {name :: String
              ,id :: String
              ,joiningYear :: Int }
              
              
findEarliestJoiner :: {Employee} -> String
findEarliestJoiner emplye = hd[x.name \\ x <-:emplye | x. joiningYear == min_yr]
	where min_yr = minList [ a.joiningYear \\ a <-: emplye]




employee1 = {name="Alice", id="emp1", joiningYear=2015}
employee2 = {name="Bob", id="emp2", joiningYear=2013}
employee3 = {name="Charlie", id="emp3", joiningYear=2014}
employee4 = {name="Diana", id="emp4", joiningYear=2017}
//Start = findEarliestJoiner {employee1} // "Alice"
//Start = findEarliestJoiner {employee1, employee2, employee3, employee4} // "Bob"
//Start = findEarliestJoiner {employee4, employee3} // "Charlie"
/*---------------*/
:: Light = { num :: Int,
             state :: Bool,
             level :: Int,
             duration :: Int}
Light1={num = 1,state = True,level = 4,duration = 99}
Light2={num = 2,state = False,level = 2,duration = 150} 
Light3={num = 3,state = True,level = 5,duration = 400}
Light4={num = 4,state = False,level = 1,duration = 78}
Light5={num = 5,state = True,level = 2,duration = 99}
Light6={num = 6,state = True,level = 3,duration = 99}
Light7={num = 7,state = False,level = 4,duration = 120}
Light8={num = 8,state = False,level = 6,duration = 370}
Light9={num = 9,state = True,level = 2,duration = 160}
Light10={num = 10,state = True,level = 1,duration = 99}
/* 6.
Implement a function that takes two parameters:  
a list of Light and a list of integers representing 
times of state changes. After changing state, return 
the final state:: Bool of each light (num :: Int) as 
a list of tuples (Int, Bool).
Eg.:Turning [Light1,Light2] [1,2] that light1 changes state 
once (True->False) and Light2 (False -> True -> False) 
changes state twice.
*/
//Turning :: [Light] [Int] -> [(Int,Bool)]
//Start= Turning [Light1,Light2] [1,2] //[(1,False),(2,False)]
//Start= Turning [Light1,Light2,Light3,Light4,Light5] [5,4,7,6,8]//[(1,False),(2,False),(3,False),(4,False),(5,True)]
//Start= Turning [Light7,Light9,Light10,Light2,Light5,Light3,Light4] [5,4,7,3,8,9,10]//[(7,True),(9,True),(10,False),(2,True),(5,True),(3,False),(4,False)]
//Start= Turning [Light5,Light7,Light3,Light6,Light8] [5,4,7,6,8] //[(5,False),(7,False),(3,False),(6,True),(8,False)]
/*---------------*/
/* 7.
Sort the list of Light by their remaining usage 
time (from shortest to longest). The light's remaining usage 
time = level * 200 - duration. Return a list of Int 
representing the num of each light in order.
Hint:write the instance '==' and '<' for Light before sort.
*/

instance == Light
	where (==) a b = (a.level * 200 - a.duration) == (b.level * 200 - b.duration)
	
instance < Light
	where (<) a b = (a.level * 200 - a.duration) < (b.level * 200 - b.duration)


Sortlights :: [Light] -> [Int]
Sortlights ls = [a.num \\ a<- sorted_lg]
	where sorted_lg = sort ls

//Start= Sortlights [Light1,Light2,Light3,Light4,Light5]//[4,2,5,3,1]
//Start= Sortlights [Light2,Light6,Light8,Light9,Light10]//[10,9,2,6,8]
//Start= Sortlights [Light1,Light3,Light5,Light7,Light9]//[9,5,3,7,1]
/*---------------*/
/* 8.
Given a binary tree, determine the sum of all 
node values at the given level.
e.g for level 1 sum -> 22
          07           <- Level 0
        /      \          
     02       20        <- Level 1
    / \            / \ 
   01  04  10  30      <- Level 2
*/ 

helper_level :: Int Int (Tree Int) -> Int
helper_level n c (Node x le ri)
| n == c  = x 
= max ((helper_level n (c+1) le) (helper_level n (c+1) ri))
 
sumAtLevel :: Int (Tree Int) -> Int
sumAtLevel 0 _ = 0
sumAtLevel n (Node x le ri)
| n == (helper_level n 0 (Tree Int))


//Start = sumAtLevel 0 tree11 // 7
//Start = sumAtLevel 1 tree11 // 22
//Start = sumAtLevel 2 tree11 // 45
//Start = sumAtLevel 1 tree22 // 4
/*---------------*/
/* 9.
Given a binary tree, invert the tree. Eg.
Original:            Inverted:
    2                        2
   / \                      / \
  1   3                  3   1
Original:            Inverted:
    4                          4
   / \                          / \
  2   7                      7   2
 / \      / \                 / \     / \
1  3    6  9             9  6   3   1
*/
tree1 = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)
tree2 = Node 4 
(Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
(Node 7 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))
//invertTree :: (Tree Int) -> (Tree Int)
//Start = invertTree tree1 //(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))
//Start = invertTree tree2
// (Node 4 
// (Node 7 (Node 9 Leaf Leaf) (Node 6 Leaf Leaf)) 
//  (Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf)))
/*---------------*/
/* 10.
Given a tree of any type a values, 
count all the node that has a right child (non leaf).
     5   => this node has a right child, so count as 1.
    /   \
  3     7      => node 3 has a right child, so count as 1, node 7 doesn't have a right child, so count as 0
  / \    / \
 2   4 6 L   => none of these have a right child
Result: 2, total 2 nodes have a right child
     5   => this node has a right child, so count as 1.
    /   \
  3      7      => node 3 has no children, count as 0, node 7 has a right child, so count as 1
  / \      / \
 L   L   6  8   => none of these have a right child 
Result: 2, total 2 nodes have a right child
   
       5   => this node doesn't have a right child, so count as 0.
     /   \
   3      L      => node 3 has no children, count as 0
  / \ 
 L   L   
Result: 0, no node has a right child
*/

collect :: (Tree Int) -> [Int]
collect Leaf= []
collect (Node x le ri) = collect le ++ [x] ++ collect ri

depth :: (Tree Int) -> Int
depth Leaf= 0
depth (Node x le ri) = 1 + max ((depth le)  (depth ri))


treeA = Node 5 (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 7 (Node 6 Leaf Leaf) Leaf)
treeB = Node 5 (Node 3 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf))
treeC = Node 5 (Node 3 Leaf Leaf) Leaf
treeD = Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)

isNonLeaf :: (Tree a) -> Bool
isNonLeaf Leaf = False
isNonLeaf _ = True

countNodesWithRight :: (Tree a) -> Int
countNodesWithRight Leaf = 0
countNodesWithRight (Node x l r)
| isNonLeaf r = 1 + countNodesWithRight l + countNodesWithRight 1
| otherwise = countNodesWithRight l + countNodesWithRight 1

//Start = countNodesWithRight treeA // 2
//Start = countNodesWithRight treeB // 2
//Start = countNodesWithRight treeC // 0
//Start = countNodesWithRight treeD // 1
/*---------------*/
:: Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
/* 11. 
Create an instance == for the Weekday.
Write a function that when given a weekday, 
returns the next weekday.
Eg. Monday -> Tuesday, Tuesday -> Wednesday, ..., Sunday -> Monday
*/

:: Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
instance == Weekday
	where (==) Monday Monday = True
		  (==)   Tuesday Tuesday = True
		  (==)  Wednesday Wednesday = True
		  (==)   Thursday Thursday = True
		  (==)   Saturday Saturday = True
		  (==)   Sunday Sunday = True
		  (==)   _ _ = False

nextWeekday :: Weekday -> Weekday
nextWeekday wkday
|wkday == Monday = Tuesday
|wkday == Tuesday = Wednesday
|wkday == Wednesday = Thursday
|wkday == Thursday = Friday
|wkday == Friday = Saturday
|wkday == Saturday = Sunday
=Monday
 
//Start = nextWeekday Monday // Tuesday
//Start = nextWeekday Wednesday // Thursday
//Start = nextWeekday Sunday // Monday
/*---------------*/