       IDENTIFICATION DIVISION.
       PROGRAM-ID. PETSTORE.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
            SELECT PETSALESFILE ASSIGN TO "PETSTORESALES.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PETSALESREPORT ASSIGN TO "PETSALESREPORT.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION. 
       FILE SECTION. 
       FD PETSALESFILE.
       01 SALESDETAILS.
          88 ENDOFSALESFILE                        VALUE HIGH-VALUES.
       02 CUSTOMER-ID            PIC 9(7).
          02 CUSTOMERNAME.
             05 LASTNAME         PIC X(15).
             05 FIRSTNAME        PIC X(15).
          02 PETITEM OCCURS 3 TIMES.
             05 DESCRIPTION      PIC X(20).
             05 PRICE            PIC 999999V99.
             05 QUANTITY         PIC 99999.
       FD PETSALESREPORT.
       01 PRINT-LINE             PIC X(132).

       WORKING-STORAGE SECTION. 
       01 WS-FIELDS.
          05 WS-SUBTOT-QUANT     PIC 999.
          05 WS-TOTAL-QUAT       PIC 999.
          05 WS-ITEM-SUBTOTAL    PIC 9(6)V99.
          05 WS-ITEM-TOTAL       PIC 9(6)V99.
          05 WS-TOTAL-SALE       PIC 9(7)V99.
          05 WS-INDEX            PIC 999.

       01 WS-DATE.
          05 WS-YEAR             PIC 99.
          05 WS-MONTH            PIC 99.
          05 WS-DAY              PIC 99.

       01 HEADING-LINE.
          05 FILLER              PIC X(45).
          05 FILLER              PIC X(21)         VALUE
                'PET SUPPLIES AND MORE'.

       01 HEADING-LINE1.
          05 FILLER              PIC X(16)         VALUE
                'ITEM DESCRIPTION'.
          05 FILLER              PIC X(20)         VALUE SPACES.
          05 FILLER              PIC X(11)         VALUE "PRICE".
          05 FILLER              PIC X(2)          VALUE SPACES.
          05 FILLER              PIC X(11)         VALUE 'QUANTITY'.
          05 FILLER              PIC X(2)          VALUE SPACES.
          05 FILLER              PIC X(11)         VALUE 'TOTAL'.

       01 DETAIL-LINE.
          05 FILLER              PIC X(5)          VALUE SPACES.
          05 DET-DESCRIPTION     PIC X(20).
          05 FILLER              PIC X(9)          VALUE SPACES.
          05 DET-PRICE           PIC $,$$9.99.
          05 FILLER              PIC X(8)          VALUE SPACES.
          05 DET-QUANTITY        PIC Z9.
          05 FILLER              PIC X(7)          VALUE SPACES.
          05 DET-ITEM-TOTAL      PIC $,$$9.99.

       01 DETAIL-SUBTOTAL-LINE.
          05 FILLER              PIC X(20)         VALUE SPACES.
          05 FILLER              PIC X(1)          VALUE "="
                OCCURS 60 TIMES.

       01 DETAIL-SUBTOTAL-LINE1.
          05 FILLER              PIC X(20)         VALUE SPACES.
          05 DET-LASTNAME        PIC X(15)         VALUE SPACES.
          05 FILLER              PIC X(10)         VALUE "QUANTITY: "
           .
          05 DET-SUBTOTAL-QUANT  PIC 999.
          05 FILLER              PIC XX.
          05 FILLER              PIC X(14)         VALUE
                " SUB-TOTAL : "
           .
          05 FILLER              PIC X(1)          VALUE SPACES.
          05 DET-SUBTOT-SALES    PIC $$,$$$,$$9.99.
          05 FILLER              PIC X(3)          VALUE SPACES.

       01 DET-TOTAL-LINE.
          05 FILLER              PIC X(7)          VALUE SPACES.
          05 FILLER              PIC X(20)         VALUE
                "   TOTAL QUANTITY: ".
          05 DET-TOTAL-QUANT     PIC 999.
          05 FILLER              PIC XX.
          05 FILLER              PIC X(7)          VALUE "TOTAL  ".
          05 FILLER              PIC X(1)          VALUE SPACES.
          05 DET-TOT-SALES       PIC $$,$$$,$$9.99.
          05 FILLER              PIC X(3)          VALUE SPACES.

       
       PROCEDURE DIVISION.
       0100-START.
           OPEN INPUT PETSALESFILE.
           OPEN OUTPUT PETSALESREPORT.
           READ PETSALESFILE
           AT END
              SET ENDOFSALESFILE TO TRUE
           END-READ.
           PERFORM 0110-WRITE-HEADING-LINES.
           PERFORM 0200-PROCESS-ITEMS UNTIL ENDOFSALESFILE.
           PERFORM 0290-PRINT-TOTAL.
           PERFORM 0300-STOP-RUN.
       0100-END.

       0110-WRITE-HEADING-LINES.
           WRITE PRINT-LINE FROM HEADING-LINE AFTER
              ADVANCING PAGE.
           WRITE PRINT-LINE FROM HEADING-LINE1
              AFTER ADVANCING 1 LINE.
       0110-END.

       0200-PROCESS-ITEMS.
      * Every time a record is read the index goes back to 0
           MOVE 1 TO WS-INDEX.
           MOVE 0 TO WS-ITEM-SUBTOTAL, WS-SUBTOT-QUANT.
           MOVE LASTNAME TO DET-LASTNAME.
      * Lines inside the perform don't have a period because they will
      * perform the three times for each record
           PERFORM 3 TIMES
                   MOVE DESCRIPTION(WS-INDEX) TO DET-DESCRIPTION
                   MOVE PRICE(WS-INDEX) TO DET-PRICE
                   MOVE QUANTITY(WS-INDEX) TO DET-QUANTITY

                   COMPUTE WS-ITEM-TOTAL = PRICE(WS-INDEX) * QUANTITY
                      (WS-INDEX)
                   COMPUTE WS-ITEM-SUBTOTAL = WS-ITEM-SUBTOTAL
                      + WS-ITEM-TOTAL
                   COMPUTE WS-TOTAL-SALE = WS-TOTAL-SALE + WS-ITEM-TOTAL
                   COMPUTE WS-TOTAL-QUAT = WS-TOTAL-QUAT
                      + QUANTITY(WS-INDEX)
                   ADD QUANTITY(WS-INDEX) TO WS-SUBTOT-QUANT

                   MOVE WS-ITEM-TOTAL TO DET-ITEM-TOTAL
                   WRITE PRINT-LINE FROM DETAIL-LINE
                      AFTER ADVANCING 1 LINE
                   ADD 1 TO WS-INDEX
           END-PERFORM.
           MOVE WS-ITEM-SUBTOTAL TO DET-SUBTOT-SALES.
           MOVE WS-SUBTOT-QUANT TO DET-SUBTOTAL-QUANT.
           WRITE PRINT-LINE FROM DETAIL-SUBTOTAL-LINE1
              AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           READ PETSALESFILE
           AT END
              SET ENDOFSALESFILE TO TRUE
           END-READ.
       0200-END.

       0290-PRINT-TOTAL.

           MOVE WS-TOTAL-QUAT TO DET-TOTAL-QUANT.
           MOVE WS-TOTAL-SALE TO DET-TOT-SALES.
           WRITE PRINT-LINE FROM DET-TOTAL-LINE
              AFTER ADVANCING 1 LINE.

       0290-END.

       0300-STOP-RUN.
           CLOSE PETSALESFILE, PETSALESREPORT.
           STOP RUN.

       END PROGRAM PETSTORE.
       