module MuhammadEmanAftabHW7GR1

import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW7GR1 ( e.g JohnSmithHW7GR1)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

/*
1. You are provided with a text where every word is written in all uppercase letters. 
Your task is to convert this text into a more readable format. 
You should make sure that each sentence starts with a capital letter, 
and the rest of the words are in lowercase.
Example:
Input:
"THIS IS AN EXAMPLE TEXT. IT DEMONSTRATES HOW YOUR TEXT SHOULD LOOK AFTER THE MODIFICATION."
Output:
"This is an example text. It demonstrates how your text should look after the modification."
*/

helper_small:: [Char]  -> [Char]
helper_small []  = []
helper_small [x:xs]  
|x <> ' ' && x <> '.' =  [toChar((toInt x) + 32)] ++ helper_small xs 
= [x] ++ helper_small xs 

//Start = helper_small['A','B',' ', 'C','.'] 
//Start= helper_small 'B'

to_list :: String -> [Char]
to_list str= [a \\ a<-: str]


helper_capital :: [Char] -> [Char]
helper_capital [] = []
helper_capital [x,y]=[x,y]
helper_capital [x,y,z:xs] 
| x == '.' = [x]++[y]++[toChar((toInt z)-32)] ++ helper_capital xs
=[x] ++ helper_capital [y,z:xs]
//Start = helper_capital ['h','l','.',' ','c','a','b']

capitalizeFirst :: String -> String
capitalizeFirst str = toString( [(toChar((toInt(hd (helper_capital (helper_small (to_list str)))))-32))] ++ tl(helper_capital (helper_small (to_list str))))


//Start = capitalizeFirst "THIS IS AN EXAMPLE TEXT. IT DEMONSTRATES HOW YOUR TEXT SHOULD LOOK AFTER THE MODIFICATION." // "This is an example text. It demonstrates how your text should look after the modification."
//Start = capitalizeFirst "THE SUN SETS IN THE WEST. THE MOON RISES IN THE EAST. STARS SHINE BRIGHT AT NIGHT." // "The sun sets in the west. The moon rises in the east. Stars shine bright at night."
//Start = capitalizeFirst "COMPUTER SCIENCE IS FASCINATING. ALGORITHMS SOLVE PROBLEMS EFFICIENTLY. DATA STRUCTURES ORGANIZE INFORMATION." // "Computer science is fascinating. Algorithms solve problems efficiently. Data structures organize information."


/*
2. You are given a string consisting of words separated by spaces. 
Your task is to encrypt each word in the string by 
shifting the letters of each word forward by a fixed number of positions. 
The number of positions to shift is provided as an integer.

For example:


Given the string "hello world" with the integer 3, return "khoor zruog."
Explanation:

Shift each letter in "hello" by 3 positions to get "khoor"
Shift each letter in "world" by 3 positions to get "zruog"
*/

helper_encrypt :: String Int Int -> String
helper_encrypt str i x
|i == size str = ""
|str.[i] == ' ' = " " +++ helper_encrypt str (i+1) x
|(toInt(str.[i])+x)>122 = (toString(toChar(96+((toInt(str.[i])+x)-122)))) +++ helper_encrypt str (i+1) x
= toString(toChar(toInt(str.[i])+x)) +++ helper_encrypt str (i+1) x

encrypt :: String Int -> String
encrypt str n = helper_encrypt str 0 n

//Start = encrypt "hello world" 3 // "khoor zruog"
//Start = encrypt "encrypt this" 2 // "gpetarv vjku"
//Start = encrypt "programming is awesome" 5 // "uwtlwfrrnsl nx fbjxtrj"
