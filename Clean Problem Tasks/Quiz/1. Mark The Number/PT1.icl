module PT1
import StdEnv

/*

Name: Muhammad Eman Aftab
Neptune Code: IJE4R1

 Write a function called 'convertMarkToString'  that takes an integer input called
'mark' and returns the corresponding string based on the following:
if mark is 1, the function should return "Fail".
if mark is 2, the function should return "Pass".
if mark is 3, the function should return "Satisfactory".
if mark is 4, the function should return "Good".
if mark is 5, the function should return "Excellent".

If the mark is not in [1 - 5], return "Invalid input".
*/

convertMarkToString :: Int -> String
// write your code here ...
convertMarkToString mark
|mark==1 = "Fail"
|mark==2 = "Pass"
|mark==3 = "Satisfactory"
|mark==4 = "Good"
|mark==5 = "Excellent"
="Invalid Input"

//Start = convertMarkToString 1 // "Fail"
//Start = convertMarkToString 2 // "Pass"
Start = convertMarkToString 3 // "Satisfactory"
//Start = convertMarkToString 4 // "Good"
//Start = convertMarkToString 5 // "Excellent"
//Start = convertMarkToString 6 // "Invalid input"