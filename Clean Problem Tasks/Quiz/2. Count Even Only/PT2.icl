module PT2
import StdEnv

/* Your NAME and NEPTUN code :  Muhammad Eman Aftab IJE4R1  */

/* 1. Write a function to count the event digits in a number. */




evenDigitsCount :: Int -> Int
evenDigitsCount x


 // | x==0=0
  //|x==1=0
  //|x==2=1 + evenDigitsCount (x / 10)
  //|x==3=0
  //|x==4=1 + evenDigitsCount (x / 10)
  //|x==5=0
  //|x==6=1 + evenDigitsCount (x / 10)
  //|x==7=0
  //|x==8=1 + evenDigitsCount (x / 10)
  //|x==9=0
  
  
|x<10 && (x rem 2 <> 0) = 0
|x<10 && (x rem 2==0) = 1
|x rem 2 == 0 = 1 + evenDigitsCount (x / 10)
//|x<10 && (x rem 2 == 0) = 1+ evenDigitsCount (x / 10)
=evenDigitsCount (x / 10)

//Start = evenDigitsCount 456 // 2
//Start = evenDigitsCount 1111 // 0
//Start = evenDigitsCount 2 // 1
//Start = evenDigitsCount 12345678 // 4

