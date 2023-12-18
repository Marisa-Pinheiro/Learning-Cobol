       IDENTIFICATION DIVISION.
       PROGRAM-ID. "OBTAININPUT".
      *This is a comment in COBOL
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Place to store variables
      *To declare a variable: start with a level number
      *(Level number: A level number is a one-digit or two-digit int
      *between 01 and 49, or one of three special level numbers: 66, 77, or 88)  
      *01 -> the lowest level number === highest level of element.
       01 NAME PIC A(20).
    
     
       PROCEDURE DIVISION. 
           0100-START-HERE.
               DISPLAY "Hello World!".
       STOP RUN.
       END PROGRAM OBTAININPUT.
