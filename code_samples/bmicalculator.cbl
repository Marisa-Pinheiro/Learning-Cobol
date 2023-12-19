       IDENTIFICATION DIVISION.
       PROGRAM-ID. "BMICALCULATOR"

       ENVIRONMENT DIVISION. 

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
      * 9-> The value of the variable is numeric, and, in this case,
      *     ranges from 0-999. 
       01 WEIGHT PIC 999.
       01 HEIGHT_INCHES PIC 999.
      * V99 -> Implied decimal, allows decimal portion with two numbs. 
       01 BMI PIC 999V99.

       PROCEDURE DIVISION.
       0100-START-HERE .
           DISPLAY "Enter your height in inches.".
           ACCEPT HEIGHT_INCHES.
           DISPLAY "Enter your wight in pounds".
           ACCEPT WEIGHT.
      * Calculation of BMI, COMPUTE is a reserved verb.
      * (The COMPUTE command assigns the value of an arithmetic 
      * expression to a specified reference. 
      * The COMPUTE keyword cannot be abbreviated.)
           COMPUTE BMI = WEIGHT*703/(HEIGHT_INCHES*HEIGHT_INCHES).
           DISPLAY "Your BMI is: ", BMI.
           STOP RUN.
       END PROGRAM BMICALCULATOR.
