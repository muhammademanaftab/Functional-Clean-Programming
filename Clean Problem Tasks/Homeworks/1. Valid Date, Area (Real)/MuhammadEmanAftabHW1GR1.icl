module MuhammadEmanAftabHW1GR1

import StdEnv 

/* When you submit this homework, change the filename to YourFullNameHW1GR1 ( e.g JohnSmithHW1GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here: IJE4R1

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/


/*
Write a function that takes three integers as input: day, month, and year.

The program should determine if the provided date is valid

and return true if it's a valid date; otherwise, return false.

 

To be considered a valid date, the following conditions must be met:

 
1. The year must be a positive integer.

2. The month must be in the range of 1 to 12 (inclusive).

3. The day must be in the range of 1 to 31, regardless of the month.

 
Your program should return true if the input represents a valid date based on these conditions and false otherwise.

Note: This task focuses on basic date validation without considering the varying number of days in each month or leap years
*/

IsValidDate :: Int Int Int -> Bool
IsValidDate day month year 
|(year > 0) && (month >0 && month < 13) && (day > 0 && day < 32) = True
=False

//Start = IsValidDate 25 2 2020 // True
//Start = IsValidDate 14 5 1990 // True
//Start = IsValidDate 35 8 2012 // False
//Start = IsValidDate 20 14 2015 // False
//Start = IsValidDate 11 5 -2011 // False



/* Write a function that calculates

the area of a triangle given its base and height as input.

The function should return the area. If either the base or height is negative,

return -1 to indicate an invalid input.

*/



CalculateArea :: Real Real -> Real
CalculateArea base height
|base > 0.0 && height > 0.0 = (0.5*(base*height))
= -1.00
//Start = CalculateArea 5.00 9.00 // 22.50
//Start = CalculateArea 4.00 12.00 // 24.00
//Start = CalculateArea 3.00 4.00 // 6.00
//Start = CalculateArea -3.00 7.00 // -1.00
//Start = CalculateArea 4.00 -6.00 // -1.00
//Start = CalculateArea -2.00 -4.00 // -1.00

