       IDENTIFICATION DIVISION.
       PROGRAM-ID. "ENHANCEDBMICALCULATOR".


       ENVIRONMENT DIVISION. 

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT BMI-FILE ASSIGN TO "BMI-INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "BMI-REPORT.DAT".

       DATA DIVISION.
       FILE SECTION.
         FD BMI-FILE.

         01 BMIDETAILS.
                88 ENDOFBMI VALUE HIGH-VALUES.
            05 PERSON-NAME.
                10 LASTNAME      PIC X(20).
                10 FIRSTNAME     PIC X(20).
            05 HEIGHT-INCHES     PIC 999.
            05 WEIGHT            PIC 999.

        FD PRINT-FILE.

        01 PRINT-LINE        PIC X(132).
           
       WORKING-STORAGE SECTION.
        01 WS. 
             05 WS-BMI           PIC 99V99.

        01  HEADING-LINE.
            05 FILLER            PIC X(5) VALUE SPACES.
            05 FILLER            PIC X(40) VALUE 'NAME'.
            05 FILLER            PIC X(22) VALUE 'HEIGHT IN INCHES'.
            05 FILLER            PIC X(30) VALUE 'WEIGHT IN POUNDS'.
            05 FILLER            PIC X(12) VALUE 'BMI'.
            05 FILLER            PIC X(22) VALUE SPACES.

        01  DETAIL-LINE.
            05 FILLER           PIC X(5)  VALUE SPACES.
            05 DET-NAME         PIC X(40).
            05 FILLER           PIC X(5)  VALUE SPACES.
            05 DET-HEIGHT       PIC X(5).
            05 FILLER           PIC X(20)  VALUE SPACES.
            05 DET-WEIGHT       PIC X(12).
            05 FILLER           PIC X(10)  VALUE SPACES.
            05 DET-BMI          PIC 999.99.
            05 FILLER           PIC X VALUE '%'.
         
       PROCEDURE DIVISION.  
               
        0050-OPEN-FILE.
           OPEN INPUT BMI-FILE.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0400-STOP-RUN.

        0100-PROCESS-RECORDS.

           PERFORM 0300-WRITE-HEADING-LINE.
      *    This is a priming read of the data file
           READ BMI-FILE
                AT END SET ENDOFBMI TO TRUE
                END-READ.
           PERFORM 0200-CALCULATE-BMI UNTIL ENDOFBMI. 

        0200-CALCULATE-BMI.
            COMPUTE WS-BMI = WEIGHT * 703 / (HEIGHT-INCHES * 
             HEIGHT-INCHES).
            MOVE PERSON-NAME TO DET-NAME.
            MOVE HEIGHT-INCHES TO DET-HEIGHT.
            MOVE WEIGHT TO DET-WEIGHT.
            MOVE WS-BMI TO DET-BMI.
            PERFORM 0320-WRITE-DETAIL-LINE.
            READ BMI-FILE
               AT END SET ENDOFBMI TO TRUE
            END-READ.

        0300-WRITE-HEADING-LINE.
            MOVE HEADING-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
            MOVE SPACES TO PRINT-LINE.
            WRITE PRINT-LINE.

        0320-WRITE-DETAIL-LINE.
            MOVE DETAIL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.


        0400-STOP-RUN.
           CLOSE BMI-FILE.
           CLOSE PRINT-FILE.

           STOP RUN.
       END PROGRAM ENHANCEDBMICALCULATOR.