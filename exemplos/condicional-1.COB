       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDICIONAL0001.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 VALOR PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "VALOR: "
           ACCEPT VALOR

           IF VALOR NOT EQUAL 10 THEN
               IF VALOR > 10 THEN
                   DISPLAY 'VALOR EH MAIOR QUE 10!'
               ELSE
                   DISPLAY 'VALOR EH MENOR QUE 10!'
               END-IF
           ELSE
               DISPLAY 'VALOR EH IGUAL A 10!'
           END-IF.

           STOP RUN.
       END PROGRAM CONDICIONAL0001.
