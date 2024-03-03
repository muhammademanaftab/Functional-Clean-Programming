module EndQuestions_todo
import StdEnv

:: Q = { nom :: Int , den :: Int }

:: Beer = {name :: String, price :: Real, ratings :: [Int]}
// instances originally were not given



:: Tree a = Node a (Tree a) (Tree a) | Leaf

btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))

ctree = Node 1 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

atree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))

:: Place = {x :: Int, y :: Int, name1 :: String}

Deak = {x = 0, y = 0, name1 = "Deak Ferenc"}

ELTE = {x = 5, y = 6, name1 = "ELTE"}

Nyugati = {x = 4, y = 2, name1 = "Nyugati Palyaudvar"}

Corvin = {x = 4, y = 3, name1 = "Corvin Negyed"}

KoKi = {x=10, y=12, name1="Kobanya Kispest"}

Keleti = {x=5, y=2, name1="Keleti Palyaudvar"}


Coors = {name="Coors", price=2.75, ratings=[2,3,2,2,1]}

Miller = {name="Miller", price=3.00, ratings=[3,2,2,3,2,2,2]}

SamAdams = {name="Samuel Adams", price=4.00, ratings=[3,3,4,2]}

Guinness = {name="Guinness", price=5.00, ratings=[2,4,4,3,5,3]}

Pabst = {name="Pabst Blue Ribbon", price=2.00, ratings=[1,1,2,1,1,2,1,1,2,3]}

BlueMoon = {name="Blue Moon", price=3.75, ratings=[4,3]}

TreeOne = (Node SamAdams (Node Coors (Node Pabst Leaf Leaf) (Node Miller Leaf Leaf)) (Node BlueMoon (Node Guinness Leaf Leaf) Leaf))

TreeTwo = (Node Miller (Node Coors Leaf Leaf) (Node SamAdams Leaf Leaf))

ListOne = [Coors, Miller, Coors, Coors, SamAdams, Guinness, Guinness, Guinness, BlueMoon]

ListTwo = [Coors, Miller, SamAdams, Guinness]

ListThree = [Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst]



/** * 1. Given a list, write a function that creates sublists with number of elements equal to the first element of that sublist.
For example: [1,2,3,4,5,6,7,8] -> [[1], [2,3], [4,5,6,7], [8]] */


f1 :: [Int] -> [[Int]]
f1 [] = []
f1 [x:xs]
| x > length [x:xs] = [[x:xs]]
| otherwise = [take x [x:xs]] ++ f1 (drop x [x:xs])

//Start = f1 [1..20] //[[1],[2,3],[4,5,6,7],[8,9,10,11,12,13,14,15],[16,17,18,19,20]]
//Start = f1 [5,2,4,3,4,1,2,5,3,6,2,6,7] //[[5,2,4,3,4],[1],[2,5],[3,6,2],[6,7]]




/** * 2. Given a tree, find the level between max node and min node. */

collect :: (Tree Int) -> [Int]
collect Leaf= []
collect (Node x l r) = collect l ++ [x] ++ collect r

level :: Int Int (Tree Int) -> Int
level _ _ Leaf = 0
level n lev (Node x l r)
| x == n = lev
| otherwise = max ( level n (lev+1) l) ( level n (lev+1) r)

f2 :: (Tree Int) -> Int
f2 tree = abs ( (level min 1 tree) - (level max 1 tree))
	where t2l = collect tree
		  min = minList t2l
		  max = maxList t2l

//Start = f2 ctree //4
//Start = f2 btree//0


/** * 3. Given a tree and an integer n, find the nodes equal to n and replace “its parent” by ‘-1’ (the value of leaf default to be 0) */

getNode :: (Tree Int) -> Int
getNode Leaf = 0
getNode (Node x _ _) = x

f3 :: Int (Tree Int) -> (Tree Int)
f3 _ Leaf = Leaf
f3 n (Node x l r)
| n == getNode l || n == getNode r = (Node -1 (f3 n l) (f3 n r))
| otherwise = (Node x (f3 n l) (f3 n r))

//Start = f3 3 atree //(Node 4 (Node -1 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node -1 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)))
//Start = f3 2 ctree //(Node -1 (Node 2 (Node 8 Leaf Leaf) (Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node -1 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)))


/** * 4. Given three rational numbers, add the first two and multiply by the third one
(a b c -> (a+b)*c, you must simplify the answer (Q 2 4) -> (Q 1 2))
*/

simplify :: Q -> Q
simplify {nom=n, den=d}
| d == 0 = abort "Undefined"
| d > 0 = {nom=n/g, den=d/g}
| otherwise = {nom=(~n/g), den=(~d/g)}
	where g = gcd n d

makeQ :: Int Int -> Q
makeQ a b = simplify {nom=a, den=b}

instance + Q
	where (+) {nom=a, den=b} {nom=c, den=d} = makeQ (a*d + b*c) (b*d) 
instance * Q 
	where (*) {nom=a, den=b} {nom=c, den=d} = makeQ (a*c) (b*d) 

f4 :: Q Q Q -> Q
f4 q1 q2 q3 = (q1 + q2) * q3

//Start = f4 {nom = 1, den = 4} {nom = 3, den = 4} {nom = 3, den = -3} //(Q -1 1)
//Start = f4 {nom = 3, den = 4} {nom = 0, den = -3} {nom = 3, den = 4} //(Q 9 16)
//Start = f4 {nom = 3, den = 0} {nom = 0, den = -3} {nom = 1, den = 4} // denominator is 0


/** * 5. For a given n generate an array that has as elements 1’s separated by n-many n’s.
For example, if n=4 the result is {1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1} */


f5 :: Int-> {Int}
f5 n = {f \\ f<-final}
	where final = flatten [ [i,1] \\ i<-[1..n] , j<-[1..i] ]
//List to Array
//Start :: {Int}
//Start = f5 4 //{1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1}
//Start = f5 6 //{1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1,5,1,5,1,5,1,5,1,5,1,6,1,6,1,6,1,6,1,6,1,6,1}


/** * 6. Write a function that given a list of arrays, sorts them by their greatest element. 
The order of elements in the arrays must be preserved. 
In the case of arrays with equal greatest elements, their original order in the list must be preserved.
For example: sortArrays [{2,3,4},{1,2,3},{3,4}] will return [{1,2,3},{2,3,4},{3,4}] */

max1:: {Int} -> Int
max1 arr = maxList ls
	where ls = [a \\ a<-:arr]
	
list_arr :: {Int} -> [Int]
list_arr arr = [a\\a<-:arr]

sortArrays :: [{Int}] -> [{Int}]
sortArrays list = sort list

instance < {Int}
	where (<) a b = (maxList [e \\ e<-:a]) < (maxList [e \\ e<-:b])

//Start = sortArrays [{4,2,5,6},{1,4,2},{5,2,1,0,3,2}] //[{1,4,2},{5,2,1,0},{4,2,5,6}]
//Start = sortArrays [{1,2,3},{2},{2,5,2},{3,1},{1,2},{0}] //[{0},{2},{1,2},{1,2,3},{3,1},{2,5,2}]
//Start = sortArrays [] //[]


/** * 7. Write a function that takes a list of records containing information on various beers, 
and sorts it based on the beer’s average rating but discards beers with an average rating under 3.0.
Note: Be sure to remove duplicates! */



removeB :: [Beer] -> [Beer]
removeB [] = []
removeB [b:bs]
	| avg [ toReal e \\ e<-b.ratings] < 3.0 = removeB bs
	| otherwise = [b: removeB bs]

instance < Beer
	where (<) a b = avg [ toReal e \\ e<-a.ratings] < avg [ toReal e \\ e<-b.ratings]

instance == Beer
	where (==) a b = avg [ toReal e \\ e<-a.ratings] == avg [ toReal e \\ e<-b.ratings] && a.name == b.name && a.price == b.price

SortBeer :: [Beer] -> [Beer]
SortBeer list = sort (removeDup (removeB list))

//Start = SortBeer ListTwo//[(Beer “Samuel Adams” 4 [3,3,4,2]),(Beer “Guinness” 5 [3,4,4,5,5,3])]
//Start = SortBeer ListOne//[(Beer “Samuel Adams” 4 [3,3,4,2]),(Beer “Guinness” 5 [3,4,4,5,5,3]),(Beer “Blue Moon” 3.75 [4,5])]
//Start = SortBeer ListThree //[]


/** * 8. Write a function that takes a list of records containing information on various beers, 
and creates a level balanced binary search tree sorted by their average rating. */

//BeerTree :: [Beer] -> (Tree Beer)

//Start = BeerTree [Coors, Miller, SamAdams, Miller] //(Node (Beer “Miller” 3 [3,2,2,3,2,2,2]) (Node (Beer “Coors” 2.75 [2,3,2,2,1]) Leaf Leaf) (Node (Beer “Samuel Adams” 4 [3,3,4,2]) Leaf Leaf))
//Start = BeerTree [Coors, Miller, SamAdams, Guinness, Pabst, BlueMoon] //(Node (Beer “Samuel Adams” 4 [3,3,4,2]) (Node (Beer “Coors” 2.75 [2,3,2,2,1]) (Node (Beer “Pabst Blue Ribbon” 2 [1,1,2,1,1,2,1,1,2,3]) Leaf Leaf) (Node (Beer “Miller” 3 [3,2,2,3,2,2,2]) Leaf Leaf)) (Node (Beer “Blue Moon” 3.75 [4,3]) (Node (Beer “Guinness” 5 [2,4,4,3,5,3]) Leaf Leaf) Leaf))
//Start = BeerTree [] //Leaf


/** * 9. Write a function that takes a tree of Beer records and returns a tree with tuples containing 
the Name and average rating of a beer with the tuples in the same locations as the originating record.
For example: A node with {name=“Coors”, price=2.75, ratings=[2,3,2,2,1]}
would become a tuple of (“Coors”, 2) in the new tree. */

RatingsTree :: (Tree Beer) -> (Tree (String, Int))
RatingsTree Leaf = Leaf
RatingsTree (Node b l r) = Node (b.name, avg [ e \\ e<-b.ratings]) (RatingsTree l) (RatingsTree r)

//Start = RatingsTree TreeOne //(Node (“Samuel Adams”,3) (Node (“Coors”,2) (Node (“Pabst Blue Ribbon”,1) Leaf Leaf) (Node (“Miller”,2) Leaf Leaf)) (Node (“Blue Moon”,3) (Node (“Guinness”,3) Leaf Leaf) Leaf))
//Start = RatingsTree TreeTwo //(Node (“Miller”,2) (Node (“Coors”,2) Leaf Leaf) (Node (“Samuel Adams”,3) Leaf Leaf))


/** * 10. Write a function that takes a list of records of places and their coordinates 
and print the names of the 2 places where the taxicab distance between two of them is minimum. 
The taxicab distance is the sum of the absolute differences of their Cartesian coordinates) |x1 - x2| + |y1 - y2| */

//:: Place = {x :: Int, y :: Int, name1 :: String}

//instance == Place 

//dist :: Place Place -> Real
//dist a b = abs (a.x - b.x) + abs (a.y - b.y)

//NearestPair :: [Place] -> (String, String)
///NearestPair list 

//Start = NearestPair [Deak, Nyugati, ELTE,  Corvin] //("Nyugati Palyaudvar","Corvin Negyed")
//Start = NearestPair [KoKi, Keleti] //(“Kobanya Kispest”, “Keleti Palyaudvar”)
//Start = NearestPair [Nyugati, Keleti, ELTE, Deak, KoKi] //(“Nyugati Palyaudvar”, “Keleti Palyaudvar”)


//11. Make a string with given tuples as follows
//[(12,“hours”), (5,“minutes”)] -> [“12hours”,“5minutes”]
//[(1,3), (5,7)] -> [“13”, “57”]
//[(‘a’,“pple”), (‘b’,“anana”)] -> [“apple”, “banana”] 

makeString :: [(a,b)] -> [String] | toString a & toString b
makeString list = [ toString a +++ toString b \\ (a,b)<-list ]

//Start = makeString [(12,"hours"), (5,"minutes")] // [“12hours”,“5minutes”]
//Start = makeString [(1,3), (5,7)] // [“13”, “57”]
//Start = makeString [("a","pple"), ("b","anana")] // [“apple”, “banana”] 


//12. Create an instance of ‘==’ by checking equality of trees.

:: TreeL a = LeafL a
		   | NodeL (TreeL a) (TreeL a)

instance == (TreeL Int)
	where (==) (LeafL x) (LeafL y) = x == y
		  (==) (NodeL l r) (NodeL d b) = and [l==d, r==b]
		  (==) _ _ = False

//Start = (LeafL 2) == (NodeL (LeafL 2) (LeafL 3)) // False
//Start = (LeafL 2) == (LeafL 2) // True
//Start = (NodeL (LeafL 2) (LeafL 3)) == (NodeL (LeafL 2) (LeafL 3))

//13. Calculate the distance between two 2-dimensional vectors.
//formula : {v0 = 5, v1 = 6} {v0 = 3, v1 = 2} -> sqrt((3-5)2+(2-6)2)

:: Vector2 a = {v0 :: a, v1 :: a}

instance + (Vector2 a) | + a where (+) x y = {v0 = x.v0 + y.v0, v1 = x.v1+y.v1}
instance - (Vector2 a) | - a where (-) x y = {v0 = x.v0 - y.v0, v1 = x.v1-y.v1}
instance * (Vector2 a) | * a where (*) x y = {v0 = x.v0 * y.v0, v1 = x.v1*y.v1}


distance :: (Vector2 Int) (Vector2 Int) -> Real
distance v1 v2 = sqrt(toReal  (((v1.v0-v2.v0)^2) + ((v1.v1-v2.v1)^2))  )

//Start = distance {v0 = 5, v1 = 6} {v0 = 3, v1 = 2} /// (4.47213595499958…)
//Start = distance {v0 = 5, v1 = 6} {v0 = 5, v1 = 6} /// 0

