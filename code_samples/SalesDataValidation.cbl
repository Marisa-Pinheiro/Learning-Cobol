       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALESDATAVALIDATION.
       AUTHOR.     PEGGY FISHER.
      ***************************************************************
      *  This program reads a file containing sales person yearly   *
      *   sales information and prints a report.                    * 
      *   The version includes a sales commission report            *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 
       OBJECT-COMPUTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SALESFILE ASSIGN TO "INVALIDSALES.DAT"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT NEWSALESFILE ASSIGN TO "NEWSALESFILE.DAT" 
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT ERROR-REPORT ASSIGN TO "SALESERRORS.DAT".


       DATA DIVISION.
       FILE SECTION.
       FD SALESFILE 
            RECORD CONTAINS 57 CHARACTERS. 

       01 SALESDETAILS.
          88 ENDOFSALES                       VALUE HIGH-VALUES.
       05 SALESPERSON-ID           PIC 9(5).
          05 SALESPERSON-NAME.
             10 LASTNAME           PIC X(20).
             10 FIRSTNAME          PIC X(20).
          05 REGION                PIC X(5).
          05 YEARLYSALES           PIC 9(6).
          05 GENDER                PIC X.
             88 VALID-GENDER                  VALUES 'M' 'F' 'U'.

        FD NEWSALESFILE 
           RECORD CONTAINS 57 CHARACTERS.
       01 VALID-SALES-RECORD       PIC X(57).

        FD ERROR-REPORT.
       01 PRINT-LINE               PIC X(132).
        
        WORKING-STORAGE SECTION.
       01 WS-DATE.
          05 WS-YEAR               PIC 99.
          05 WS-MONTH              PIC 99.
          05 WS-DAY                PIC 99.
       
       01 WS-FIELDS.
          05 WS-BAD-RECORDS        PIC 99999.

       01 ERROR-DESCRIPTIONS.
          05 NON-NUMERIC-EMP-ID    PIC X(40)
                                              VALUE
                'EMPLOYEE ID WAS NOT NUMERIC'. 
          05 NON-NUMERIC-SALES     PIC X(40)
                                              VALUE
                'SALES AMOUNT IS INVALID'.
          05 MISSING-LNAME         PIC X(40)
                                              VALUE 'MISSING LAST NAME'.
          05 MISSING-FNAME         PIC X(40)
                                              VALUE 'MISSING FIRST NAME'
           .
          05 INVALID-NAME          PIC X(40)
                                              VALUE 'INVALID NAME'.
          05 INVALID-REGION        PIC X(40)
                                              VALUE 'INVALID REGION'.
          05 INVALID-GENDER        PIC X(40)
                                              VALUE
                'INVALID GENDER, SHOULD BE M, F, U'.
       01 VALID-RECORD-SWITCH      PIC X(3). 
          88 VALID-RECORD                     VALUE 'YES'.

       01 HEADING-LINE.
          05 FILLER                PIC X(2)   VALUE 'ID'.
          05 FILLER                PIC X(6)   VALUE SPACES.
          05 FILLER                PIC X(16)  VALUE 'SALESPERSON NAME'.
          05 FILLER                PIC X(24)  VALUE SPACES.
          05 FILLER                PIC X(6)   VALUE 'REGION'.
          05 FILLER                PIC X(4)   VALUE SPACES.
          05 FILLER                PIC X(12)  VALUE 'YEARLY SALES'.
          05 FILLER                PIC X(2)   VALUE SPACES.
          05 FILLER                PIC X(6)   VALUE 'GENDER'.
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 FILLER                PIC X(40)  VALUE 'ERROR MESSAGE'. 
 
       01 HEADING-LINE1. 
          05 FILLER                PIC X(100) VALUE SPACES.
          05 HEAD-MM               PIC 9(2).
          05 FILLER                PIC X      VALUE '/'.
          05 HEAD-DD               PIC 9(2).
          05 FILLER                PIC X      VALUE '/'.
          05 HEAD-YY               PIC 9(2).

       01 HEADING-LINE2.
          05 FILLER                PIC X(5)   VALUE '-----'.
          05 FILLER                PIC X(3)   VALUE SPACES.
          05 FILLER                PIC X(10)  VALUE '----------'.
          05 FILLER                PIC X(10)  VALUE SPACES.
          05 FILLER                PIC X(10)  VALUE '----------'.
          05 FILLER                PIC X(10)  VALUE SPACES.
          05 FILLER                PIC X(8)   VALUE '--------'.
          05 FILLER                PIC X(2)   VALUE SPACES.
          05 FILLER                PIC X(11)  VALUE '-----------'.
          05 FILLER                PIC X(14)  VALUE SPACES.
          05 FILLER                PIC X(40)  VALUE '-----------------'.
          05 FILLER                PIC X(20)  VALUE SPACES.

       01 DETAIL-LINE.
          05 DET-SALES-ID          PIC X(5).
          05 FILLER                PIC X(3)   VALUE SPACES.
          05 DET-SALESPERSON-NAME  PIC X(40).
          05 DET-REGION            PIC X(5).
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 DET-YEARLYSALES       PIC X(12).
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 DET-GENDER            PIC X. 
          05 FILLER                PIC X(7).
          05 DET-ERROR-MSG         PIC X(40).

       
        PROCEDURE DIVISION.

       0050-OPEN-FILE.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT NEWSALESFILE, ERROR-REPORT.
           INITIALIZE WS-DATE. 
           ACCEPT WS-DATE FROM DATE.
           MOVE WS-MONTH TO HEAD-MM.
           MOVE WS-DAY TO HEAD-DD.
           MOVE WS-YEAR TO HEAD-YY.
           PERFORM 0090-READ-SALES-RECORD.
           PERFORM 0110-WRITE-HEADING-LINE. 
           PERFORM 0100-PROCESS-RECORDS UNTIL ENDOFSALES.
           PERFORM 0200-STOP-RUN.

       0090-READ-SALES-RECORD.
           READ SALESFILE 
           AT END
              SET ENDOFSALES TO TRUE
           END-READ.

       0100-PROCESS-RECORDS.
           MOVE 'YES' TO VALID-RECORD-SWITCH.
           PERFORM 0150-VALIDATE-SALES.
           READ SALESFILE
           AT END
              SET ENDOFSALES TO TRUE
           END-READ.

       0110-WRITE-HEADING-LINE.
           WRITE PRINT-LINE FROM HEADING-LINE1
              AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADING-LINE
              AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADING-LINE2
              AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

       0150-VALIDATE-SALES.
           PERFORM 0155-PRINT-FILE-INFO.
           PERFORM 0160-VALIDATE-ID THRU 0180-VALIDATE-GENDER.

           IF VALID-RECORD 
              WRITE VALID-SALES-RECORD FROM SALESDETAILS
           ELSE 
              ADD 1 TO WS-BAD-RECORDS
           END-IF.
           IF WS-BAD-RECORDS > 5 
              DISPLAY "Too many invalid records"
              GO TO 0200-STOP-RUN
           END-IF.   
            
       0155-PRINT-FILE-INFO.
           MOVE SALESPERSON-ID TO DET-SALES-ID.
           MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME.
           MOVE REGION TO DET-REGION.
           MOVE YEARLYSALES TO DET-YEARLYSALES.
           MOVE GENDER TO DET-GENDER.

       0160-VALIDATE-ID.
           IF SALESPERSON-ID NOT NUMERIC THEN 
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE NON-NUMERIC-EMP-ID TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.

       0165-VALIDATE-NAME.
           IF LASTNAME NOT ALPHABETIC 
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-NAME TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.

           IF FIRSTNAME NOT ALPHABETIC 
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-NAME TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.

       0170-VALIDATE-REGION.
           IF REGION NOT ALPHABETIC
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-REGION TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.
           IF REGION = 'EAST' OR 'WEST' OR 'NORTH' OR 'SOUTH'
           NEXT SENTENCE
           ELSE 
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-REGION TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
      *if not rewrites the line
                 ADVANCING 1 LINE
           END-IF.
    
               
       0175-VALIDATE-SALES.
           IF YEARLYSALES NOT NUMERIC 
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE NON-NUMERIC-SALES TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.

       0180-VALIDATE-GENDER.
           IF NOT VALID-GENDER
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-GENDER TO DET-ERROR-MSG
              WRITE PRINT-LINE FROM DETAIL-LINE AFTER
                 ADVANCING 1 LINE.

       0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE NEWSALESFILE, ERROR-REPORT.
           STOP RUN.

        END PROGRAM SALESDATAVALIDATION.