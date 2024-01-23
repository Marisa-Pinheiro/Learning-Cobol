       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ACMEEMPLOYEES ASSIGN TO "ACME.DAT"
        FILE STATUS IS ACME-CHECK-KEY
		ORGANIZATION IS LINE SEQUENTIAL.

	   SELECT FUSESEMPLOYEES ASSIGN TO "FUSESINC.DAT"
           FILE STATUS IS FUSES-CHECK-KEY
	     ORGANIZATION IS LINE SEQUENTIAL.
    
       SELECT SORTEDFILE ASSIGN TO "EMPLOYEES.NEW"
        ORGANIZATION IS LINE SEQUENTIAL.

       SELECT REPORTFILE ASSIGN TO "REPORT.LPT"
           ORGANIZATION IS LINE SEQUENTIAL. 
		
       SELECT WORKFILE ASSIGN TO "WORK.TMP".
	               
       DATA DIVISION.
       FILE SECTION.
	   FD ACMEEMPLOYEES.
	   01 ACMEDETAILS     PIC X(47).
	
       FD FUSESEMPLOYEES.
	   01 FUSESDETAILS  PIC X(47).

       FD SORTEDFILE.
	   01 SORTDETAILS.	
            88 SORTEOF            VALUE HIGH-VALUES.	
          02 SF-SSN         PIC 9(9).
          02 SF-LASTNAME   PIC X(10).
          02 SF-FIRSTNAME  PIC X(10).	
          02 FILLER        PIC X(18).  

      * sd temporary file			
       SD WORKFILE.
       01 WORKREC.
          	02 WF-SSN        PIC 9(9).
			02 WF-LASTNAME   PIC X(10).
            02 WF-FIRSTNAME  PIC X(10).	
            02 FILLER        PIC X(18).   
        
       FD REPORTFILE.
       01 REPORTDETAIL       PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
     
	   01  WS-WORK-AREAS.
	       05  ACME-CHECK-KEY   PIC X(2).
           05  FUSES-CHECK-KEY  PIC X(2).	

       01  WS-REPORT-DETAIL.
           05  WS-SSN           PIC 9(9).
           05  FILLER           PIC X(5).
           05  WS-LASTNAME      PIC X(20).
           05  FILLER           PIC X(5).
           05  WS-FIRSTNAME     PIC X(20).
           05  FILLER           PIC X(73). 

       PROCEDURE DIVISION.
       0100-READ-EMPLOYEES.

		   OPEN INPUT ACMEEMPLOYEES, FUSESEMPLOYEES.
           IF ACME-CHECK-KEY NOT = "00"
		      DISPLAY "ERR: OPEN FILE ERROR ACME FILE: ",
	         	 ACME-CHECK-KEY
		      GO TO 9000-END-PROGRAM
		   END-IF.
           IF FUSES-CHECK-KEY NOT = "00"
		      DISPLAY "ERR: OPEN FILE ERROR FUSES FILE: ",
				 FUSES-CHECK-KEY
		      GO TO 9000-END-PROGRAM
		   END-IF.
		   				
		   MERGE WORKFILE ON ASCENDING KEY 
		      WF-SSN
		      USING ACMEEMPLOYEES 
			    FUSESEMPLOYEES
			  GIVING SORTEDFILE.
      
      * Use the sorted file as an input, needs to be opened
              OPEN INPUT SORTEDFILE.
          
              OPEN OUTPUT REPORTFILE.
              READ SORTEDFILE
			    AT END SET SORTEOF TO TRUE
			    END-READ.
              PERFORM 0200-PRINT-EMPLOYEES THRU 
                 0200-END UNTIL SORTEOF.
		 
		   PERFORM 9000-END-PROGRAM.
		   
	   0100-END.
	   
       0200-PRINT-EMPLOYEES.
           MOVE SF-SSN TO WS-SSN.
           MOVE SF-LASTNAME TO WS-LASTNAME.
           MOVE SF-FIRSTNAME TO WS-FIRSTNAME.
           WRITE REPORTDETAIL FROM WS-REPORT-DETAIL AFTER 
              ADVANCING 1 LINE.
           READ SORTEDFILE
			    AT END SET SORTEOF TO TRUE
			    END-READ.

       0200-END.
	   
	   9000-END-PROGRAM.
           CLOSE ACMEEMPLOYEES, FUSESEMPLOYEES, 
              SORTEDFILE, REPORTFILE. 
              
           STOP RUN.
           
          END PROGRAM SOLUTION.
