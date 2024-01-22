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
          88 ENDOFSALESFILES                       VALUE HIGH-VALUES.
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
          05 DET-LASTNAME        PIC X(15)         VALUE "QUANTITY: ".
          05 FILLER              PIC XX.
          05 FILLER              PIC X(14)         VALUE " SUB-TOTAL : "
                                                                      .
          05 FILLER              PIC X(1)          VALUE SPACES.
          05 DET-SUBTOT-SALES    PIC $$,$$$,$$9.99.
          05 FILLER              PIC X(3)          VALUE SPACES.