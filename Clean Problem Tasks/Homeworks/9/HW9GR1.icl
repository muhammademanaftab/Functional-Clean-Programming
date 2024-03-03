module HW9GR1

import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW9GR1 ( e.g JohnSmithHW7GR12)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf


tree1 = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf))
tree2 = Node 15 (Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)) (Node 18 (Node 16 Leaf (Node 14 Leaf Leaf)) (Node 20 Leaf Leaf))
tree3 = Node 12 (Node 8 (Node 4 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) (Node 10 Leaf (Node 15 Leaf (Node 14 Leaf Leaf)))

/*

Given a binary tree, where each node has an integer value, 
calculate the sum of all the node values that are greater than 5 at a given depth d.

example:
             10                level 1
            / \
           5   15            level 2
          / \  / \
         3   7 12 18           level 3
        / \
       1   9					level 4
       
       
       d = 3;
       output : 40
       explanation : node values at level 3 are 3, 7, 12 and 18. 3 is smaller than 5. 
       sum of others are 37
       
*/
       
      
helper :: (Tree Int) Int Int -> Int
helper Leaf _ _ = 0
helper (Node x l r) d level
| level == d && x > 5 = x + (helper l d (level+1)) + (helper r d (level+1))
= (helper l d (level+1)) + (helper r d (level+1))

sumDepth :: (Tree Int) Int -> Int
sumDepth Leaf _ = 0
sumDepth (Node x l r) d = helper (Node x l r) d 1



//Start = sumDepth tree1 3 // 37
//Start = sumDepth tree2 4 // 21
//Start = sumDepth tree3 3 // 22
//Start = sumDepth tree2 2 // 28


:: Degree =    MSC | PHD | DSC 
:: University = {universityName :: String, numberOfProfessors :: Int, professors :: {Professor}} 

:: Professor = { pname :: String,
                 teachingLoad :: {Int}, // array containing the teaching load for each semester
                 age :: Int, 
                 degree :: Degree   
               }

instance == Degree
	where (==) MSC MSC = True
		  (==) DSC DSC = True
		  (==) PHD PHD = True
		  (==) _ _ = False

p1 :: Professor
p1 = {pname = "p1" , teachingLoad = {2,3,2,3} , age = 40 , degree = MSC}
p2 :: Professor
p2 = {pname = "p2" , teachingLoad = {1,1,1,1} , age = 35 , degree = MSC}
p3 :: Professor
p3 = {pname = "p3" , teachingLoad = {2,2,2,2} , age = 50 , degree = PHD}
p4 :: Professor
p4 = {pname = "p4" , teachingLoad = {1,1,1,1} , age = 45 , degree = PHD}
p5 :: Professor
p5 = {pname = "p5" , teachingLoad = {3,3,3,3} , age = 55 , degree = DSC}
p6 :: Professor
p6 = {pname = "p6" , teachingLoad = {1,1,1,1} , age = 42 , degree = DSC}


u1 :: University
u1 = {universityName = "MIT", numberOfProfessors = 3, professors = {p1,p3,p5}}
u2 :: University
u2 = {universityName = "Harvard", numberOfProfessors = 2, professors = {p1,p2}}
u3 :: University
u3 = {universityName = "Cambridge", numberOfProfessors = 2, professors = {p1,p3}}
u4 :: University
u4 = {universityName = "Stanford", numberOfProfessors = 3, professors = {p1,p3,p5}}
u5 :: University
u5 = {universityName = "Princeton", numberOfProfessors = 3, professors = {p2,p3,p6}}

/*

Write a function that takes an array of universities and a Degree 
and returns the name of university with the highest average teaching load for professors
with the given degree.

The average teaching load of a university is the average of the average teaching 
loads of its professors with the given degree.
If the list of universities is empty, the function should return an error message 
"No universities in the list".
If there are more than one university with the same average teaching load, 
the function should return the first university in the list.


*/

average :: [Int] -> Int
average [] = 0
average [x:xs] = avg [x:xs]


highestAverageTeachingLoad :: {University}  Degree -> String
highestAverageTeachingLoad uni d
| size uni == 0 = abort "no university in the list"
| otherwise = hd [ u.universityName \\ u<-:uni &  i<- averageTeachingLoads | i == maxLoad]
	where averageTeachingLoads = [ average [ sum [t \\ t<-:(p.teachingLoad)] \\ p<-:(u.professors) | p.degree == d ] \\ u<-:uni ]
		  maxLoad = maxList averageTeachingLoads


//Start =  highestAverageTeachingLoad {u1,u2,u3,u4,u5} MSC // MIT
//Start = highestAverageTeachingLoad {u2,u3} PHD // Cambridge
//Start = highestAverageTeachingLoad {u3,u4,u5} DSC // Stanford  
//Start = highestAverageTeachingLoad {} DSC // "no university in the list"  





       
       
 