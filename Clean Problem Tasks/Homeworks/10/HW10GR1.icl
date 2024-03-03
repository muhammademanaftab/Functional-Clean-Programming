module HW10GR1

import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW10GR1 ( e.g JohnSmithHW10GR1)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

::Movie = {
			title::String,
			casts::[Cast],
			year::Int,
			rating::Real,
			country::String
			}
::Gender = Male | Female
::Cast = {
			name::String,
			gender::Gender
			}

cast1 = { name="Jackie Chan", gender= Male}
cast2 = { name="Jet Li", gender= Male }
cast3 = { name="Millie Bobby Brown", gender=Female}
cast4 = { name="Chris Hemsworth", gender=Male }
cast5 = { name="Zendaya", gender=Female }
cast6 = { name="Emma Stone", gender=Female }
cast7 = { name="Emma Watson", gender = Female }
cast8 = { name="Sandra Bullock", gender = Female}
cast9 = { name="Chris Evans", gender= Male}
cast10 = { name="Tom Holland", gender=Male}
cast11 = { name="Tobey Maguire", gender=Male}
cast12 = { name="Bae Suzy", gender=Female}
cast13 = { name="Park Seo Joon", gender=Male}

movie1 = { title="MOVIE I.", casts=[cast1, cast6, cast10], year=2019, rating=8.5, country="USA"}
movie2 = { title="MOVIE II.", casts=[cast4, cast8, cast7], year=2020, rating=8.0, country="Spain"}
movie3 = { title="MOVIE III.", casts=[cast13, cast12, cast9], year=2019, rating=9.0, country="Korea"}
movie4 = { title="MOVIE IV.", casts=[cast3, cast11, cast8], year=2021, rating=6.5, country="India"}
movie5 = { title="MOVIE V.", casts=[cast2, cast4, cast10], year=2022, rating=7.4, country="Hungary"}
movie6 = { title="MOVIE VI.", casts=[cast3, cast5, cast8], year=2022, rating=7.4, country="Hungary"}





/*
1. Given a list of movies, return the count of movies where all the cast are of same gender.
*/

instance == Gender 
	where (==) Male Male = True
		  (==) Female Female = True
		  (==) _ _ = False

allAreEqual :: [Gender] -> Bool 
allAreEqual [] = False
allAreEqual [x] = True
allAreEqual [x, y] = x == y
allAreEqual [x,y:xs] = x == y && allAreEqual [y:xs]

sameGender :: [Movie] -> Int
sameGender movies = length [m \\ m<-movies | (allAreEqual [ c.gender \\ c<-m.casts ])  ]

//Start = sameGender [movie1,movie2,movie3,movie4,movie5, movie6] // 2



/*
 2. Create an instances +, -, <,<>, == for RGBColor
 + should add respective parameters
 - should subtract respective parameters
 == is true if all three parameters are equal 
 <> is false if all three parameters are equal, true otherwise.
 < Compare them lexicographically (if reds are equal compare greens and so on)
*/


:: RGBColor = { r :: Int, g :: Int, b :: Int}

// 
instance + RGBColor
	where (+) a x = { r = a.r + x.r , g = a.g + x.g, b = a.b + x.b}

// instance - RGBColor
instance - RGBColor
	where (-) a x = { r = a.r - x.r , g = a.g - x.g, b = a.b - x.b}



// instance < RGBColor

comp :: RGBColor RGBColor -> Bool 
comp a x 
| (a.r < x.r) = True
| (a.r == x.r) && (a.g < x.g) = True
| (a.r == x.r) && (a.g == x.g) && (a.b < x.b) = True
= False

instance < RGBColor
	where (<) a x = comp a x



// instance == RGBColor
instance == RGBColor
	where (==) a x = (a.r == x.r) && (a.g == x.g) && (a.b == x.b)
// instance <> RGBColor
//Automatically Defined With (==) 

// Start = {r = 0, g = 0, b = 0} == {r = 0, g = 0, b = 0} // True
//Start = {r = 0, g = 0, b = 0} <> {r = 0, g = 0, b = 0} // False
// Start = {r = 30, g = 150, b = 231} == {r = 10, g = 30, b = 231} // False
// Start = {r = 30, g = 150, b = 231} - {r = 1, g = 1, b = 1} // {r = 29, g = 149, b = 230}
//Start = {r = 30, g = 150, b = 231} + {r = 1, g = 1, b = 1} // {r = 31, g = 152, b = 232}
// Start = {r = 30, g = 150, b = 231} > {r = 10, g = 30, b = 231} // False
// Start = {r = 30, g = 150, b = 231} < {r = 30, g = 150, b = 231} // False
// Start = {r = 30, g = 150, b = 231} < {r = 30, g = 151, b = 231} // True










