       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TSTPROB8.
       AUTHOR. HILL.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT INVENTORY-INPUT-FILE ASSIGN TO 'INVENT8.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-OUTPUT-FILE ASSIGN TO 'INV8OUT.DOC'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD  INVENTORY-INPUT-FILE RECORDING MODE IS F.
       01                              PIC X(80).

       FD  INVENTORY-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-LINE                  PIC X(132).

       WORKING-STORAGE SECTION.

       01  WORKING-VARIABLES.
           05  TOTAL-RECORDS-WS        PIC 999         VALUE ZERO.

           05  TOTAL-DOLLARS-WS        PIC S9(8)V99    VALUE ZERO.

           05  PAGE-LINE-COUNT         PIC 999         VALUE ZERO.

           05  ACCUM-DOLS-WS           PIC S9(9)V99    VALUE ZERO.

           05  ACCUM-REC-WS            PIC 999         VALUE ZERO.

           05  ACCOUNT-HOLD-WS         PIC X(5)        VALUE SPACES.

       01  EOF-WS.
           05  EOF-INVENTORY-WS        PIC X(3) VALUE 'NO'.

      ****************************************************************
           05  HOLD-DATE-WS.
               10  HOLD-YR-WS          PIC XXXX.
               10  HOLD-MO-WS          PIC XX.
               10  HOLD-DY-WS          PIC XX.
               10  HOLD-HR-WS          PIC XX.
               10  HOLD-MN-WS          PIC XX.
               10  HOLD-SC-WS          PIC XX.

      ****************************************************************

       01  INVENT-INPUT-RECORD.
           05  RECORD-TYPE             PIC X.
           05  BATCH-NUMBER            PIC XX.
           05  SUPPLIER-NUMBER         PIC X(5).
           05  VOUCHER-NUMBER          PIC X(6).
           05  INVOICE-NUMBER          PIC X(8).
           05  ACCOUNT-NUMBER          PIC X(4).
           05  STORE-NUMBER            PIC X(3).
           05  DATE-IN                 PIC X(8).
           05  FILLER                  PIC X(12).
           05  AMOUNT                  PIC S9(6)V99.
           05  SUPPLIER-NAME           PIC X(23).

       01  REPORT-HEADER-LINE-SETUP.
           05  FILLER                  PIC X VALUE SPACE.
           05                          PIC X(16) VALUE
           'RUN DATE: '.
      **************************************************************
           05  REPORT-HEADER-DATE-OUT.
               10  HEADER-MO-OUT       PIC 99.
               10                      PIC X    VALUE '/'.
               10  HEADER-DY-OUT       PIC 99.
               10                      PIC X    VALUE '/'.
               10  HEADER-YR-OUT       PIC 9999.


           05                          PIC X(4)    VALUE SPACES.
           05                          PIC X(37)   VALUE
           'INVENTORY REPORT FOR CHRISTOPHER HILL'.

           05                      PIC X(6) VALUE SPACES.
           05                      PIC X(6) VALUE 'TIME: '.
           05  HEADER-HR-OUT       PIC 99.
           05                      PIC X    VALUE ':'.
           05  HEADER-MN-OUT       PIC 99.
           05                      PIC X    VALUE ':'.
           05  HEADER-SC-OUT       PIC 99.
      **************************************************************


       01  COLUMN-HEADER-LINE1-SETUP.
           05  FILLER              PIC X.
           05                      PIC X(6)  VALUE 'RECORD'.
           05                      PIC XXX   VALUE SPACES.
           05                      PIC X(4)  VALUE 'DATE'.
           05                      PIC X(11) VALUE SPACES.
           05                      PIC X(10) VALUE 'AMOUNT'.
           05                      PIC XXX   VALUE SPACES.
           05                      PIC X(7)  VALUE 'ACCOUNT'.
           05                      PIC X(3)  VALUE SPACES.
           05                      PIC X(7)  VALUE 'INVOICE'.
           05                      PIC X(3)  VALUE SPACES.
           05                      PIC X(5)  VALUE 'BATCH'.
           05                      PIC XX    VALUE SPACES.
           05                      PIC X(7)  VALUE 'VOUCHER'.
           05                      PIC XX    VALUE SPACES.
           05                      PIC X(5)  VALUE 'STORE'.
           05                      PIC XX    VALUE SPACES.
           05                      PIC X(8)  VALUE 'SUPPLIER'.
           05                      PIC X(6)  VALUE SPACES.
           05                      PIC X(8)  VALUE 'SUPPLIER'.


       01  COLUMN-HEADER-LINE2-SETUP.
           05  FILLER              PIC X.
           05                      PIC X     VALUE SPACES.
           05                      PIC XXXX  VALUE 'TYPE'.
           05                      PIC X(5)  VALUE SPACES.
           05                      PIC XXXX  VALUE 'OUT'.
           05                      PIC X(11) VALUE SPACES.
           05                      PIC X(10) VALUE 'OUT'.
           05                      PIC X(4)  VALUE SPACES.
           05                      PIC X(7)  VALUE 'NUM'.
           05                      PIC X(3)  VALUE SPACES.
           05                      PIC X(7)  VALUE 'NUM'.
           05                      PIC XX    VALUE SPACES.
           05                      PIC X(4)  VALUE 'NUM'.
           05                      PIC X(4)  VALUE SPACES.
           05                      PIC X(3)  VALUE 'NUM'.
           05                      PIC X(5)  VALUE SPACES.
           05                      PIC X(7)  VALUE 'NUM'.
           05                      PIC X     VALUE SPACES.
           05                      PIC X(7)  VALUE 'NUM'.
           05                      PIC X(7)  VALUE SPACES.
           05                      PIC X(8)  VALUE 'NAME'.


       01  INVENT-OUTPUT-RECORD.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RECORD-TYPE-OUT         PIC X.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  DATE-OUT                PIC XX/XX/XXXX.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  AMOUNT-OUT              PIC $$$$,$$9.99BCR.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  ACCOUNT-NUM-OUT         PIC X(4).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  INVOICE-NUM-OUT         PIC X(8).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  BATCH-NUM-OUT           PIC XX.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  VOUCHER-NUM-OUT         PIC X(6).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  STORE-NUM-OUT           PIC X(3).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  SUPPLIER-NUM-OUT        PIC X(5).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  SUPPLIER-NAME-OUT       PIC X(23).

      *****************************************************************
      *ACCUMULATED TOTALS
      *****************************************************************
       01  ACCUM-DOLLARS-LINE-SETUP.
           05  FILLER                  PIC X.
           05                          PIC X(5)        VALUE SPACE.
           05                          PIC X(35)       VALUE
           'DOLLAR TOTAL FOR THIS ACCT ONLY:'.
           05  ACCT-TOTAL-DOL-OUT      PIC $$$$,$$$,$$9.99BCR.

       01  ACCUM-RECORDS-LINE-SETUP.
           05  FILLER                  PIC X.
           05                          PIC X(5)        VALUE SPACE.
           05                          PIC X(35)       VALUE
           'RECORD TOTAL FOR THIS ACCT ONLY:'.
           05  ACCT-TOTAL-REC-OUT      PIC ZZZ9.


      *****************************************************************
      *FINAL TOTAL PAGES
      *****************************************************************

       01  TOTAL-RECORDS-LINE-SETUP.
           05  FILLER                  PIC X.
           05                          PIC X(5)        VALUE SPACE.
           05                          PIC X(35)       VALUE
           'GRAND RECORD COUNT:'.
           05  TOTAL-RECORDS-OUT       PIC ZZZ9.

       01  TOTAL-DOLLARS-LINE-SETUP.
           05  FILLER                  PIC X.
           05                          PIC X(5)        VALUE SPACE.
           05                          PIC X(35)       VALUE
           'NET GRAND DOLLAR TOTAL:'.
           05  TOTAL-DOLLARS-OUT       PIC $$$$,$$$,$$9.99BCR.




       PROCEDURE DIVISION.

       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-INVENTORY-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.

           OPEN INPUT  INVENTORY-INPUT-FILE
           OPEN OUTPUT INVENTORY-OUTPUT-FILE

      ******************************************************************
           MOVE FUNCTION CURRENT-DATE TO HOLD-DATE-WS

           MOVE HOLD-MO-WS TO HEADER-MO-OUT
           MOVE HOLD-DY-WS TO HEADER-DY-OUT
           MOVE HOLD-YR-WS TO HEADER-YR-OUT
           MOVE HOLD-HR-WS TO HEADER-HR-OUT
           MOVE HOLD-MN-WS TO HEADER-MN-OUT
           MOVE HOLD-SC-WS TO HEADER-SC-OUT
      ******************************************************************

           PERFORM 250-READ-RECORD UNTIL RECORD-TYPE = '2'
           MOVE ACCOUNT-NUMBER TO ACCOUNT-HOLD-WS

           PERFORM 500-HEADER.

       250-READ-RECORD.
           READ INVENTORY-INPUT-FILE INTO INVENT-INPUT-RECORD
           AT END MOVE 'YES' TO EOF-INVENTORY-WS
           END-READ.

       300-PROCESS.

           IF RECORD-TYPE = '2'
               THEN
                   IF ACCOUNT-NUMBER IS GREATER THAN ACCOUNT-HOLD-WS
                       THEN PERFORM 600-TOTAL
                   END-IF

                   IF ACCOUNT-NUMBER IS EQUAL TO ACCOUNT-HOLD-WS
                       THEN PERFORM 350-PRINT
                   END-IF
           END-IF

           PERFORM 250-READ-RECORD.


       350-PRINT.

           MOVE  RECORD-TYPE       TO  RECORD-TYPE-OUT
           MOVE  DATE-IN           TO  DATE-OUT
           MOVE  AMOUNT            TO  AMOUNT-OUT
           MOVE  ACCOUNT-NUMBER    TO  ACCOUNT-NUM-OUT
           MOVE  INVOICE-NUMBER    TO  INVOICE-NUM-OUT
           MOVE  BATCH-NUMBER      TO  BATCH-NUM-OUT
           MOVE  VOUCHER-NUMBER    TO  VOUCHER-NUM-OUT
           MOVE  STORE-NUMBER      TO  STORE-NUM-OUT
           MOVE  SUPPLIER-NUMBER   TO  SUPPLIER-NUM-OUT
           MOVE  SUPPLIER-NAME     TO  SUPPLIER-NAME-OUT

           ADD AMOUNT  TO TOTAL-DOLLARS-WS
           ADD 1       TO TOTAL-RECORDS-WS

           ADD AMOUNT  TO ACCUM-DOLS-WS
           ADD 1       TO ACCUM-REC-WS

           MOVE INVENT-OUTPUT-RECORD TO PRINT-LINE
           WRITE PRINT-LINE AFTER 1

           ADD 1 TO PAGE-LINE-COUNT
           IF PAGE-LINE-COUNT >= 18
               THEN PERFORM 500-HEADER
           END-IF.

       500-HEADER.

           MOVE  REPORT-HEADER-LINE-SETUP    TO  PRINT-LINE
           WRITE PRINT-LINE AFTER PAGE

           MOVE  COLUMN-HEADER-LINE1-SETUP   TO  PRINT-LINE
           WRITE PRINT-LINE AFTER 2 LINES

           MOVE  COLUMN-HEADER-LINE2-SETUP   TO  PRINT-LINE
           WRITE PRINT-LINE AFTER 1 LINE

           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER 3 LINES

           MOVE 0 TO PAGE-LINE-COUNT.


       600-TOTAL.

           MOVE ACCUM-DOLS-WS  TO  ACCT-TOTAL-DOL-OUT
           MOVE ACCUM-REC-WS   TO  ACCT-TOTAL-REC-OUT
           MOVE ACCOUNT-NUMBER TO  ACCOUNT-HOLD-WS

           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER 1
           WRITE PRINT-LINE FROM ACCUM-DOLLARS-LINE-SETUP AFTER 1
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER 1 LINE
           WRITE PRINT-LINE FROM ACCUM-RECORDS-LINE-SETUP AFTER 1
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER 1


           MOVE ZEROS TO ACCUM-DOLS-WS
           MOVE ZEROS TO ACCUM-REC-WS



           IF EOF-INVENTORY-WS = 'NO'
               THEN PERFORM 500-HEADER
           END-IF.

       900-CLOSE.

           PERFORM 600-TOTAL

           MOVE  REPORT-HEADER-LINE-SETUP TO PRINT-LINE
           WRITE PRINT-LINE AFTER PAGE

           MOVE  TOTAL-RECORDS-WS  TO  TOTAL-RECORDS-OUT
           MOVE  TOTAL-DOLLARS-WS  TO  TOTAL-DOLLARS-OUT

           MOVE  TOTAL-RECORDS-LINE-SETUP  TO  PRINT-LINE
           WRITE PRINT-LINE AFTER 3 LINES

           MOVE  TOTAL-DOLLARS-LINE-SETUP  TO  PRINT-LINE
           WRITE PRINT-LINE AFTER 1 LINE

           CLOSE INVENTORY-INPUT-FILE  INVENTORY-OUTPUT-FILE.
