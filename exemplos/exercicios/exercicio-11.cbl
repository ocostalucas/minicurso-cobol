      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 N1 PIC 9(4).
           77 RESULT PIC 9(4).
           77 RESTO PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "DIGITE UM NÚMERO:"
           ACCEPT N1
           DIVIDE N1 BY 2 GIVING RESULT REMAINDER RESTO
           DISPLAY "RESTO DA DIVISAO:" RESTO
           IF RESTO = 1 THEN
               DISPLAY "ESSE NUMERO EH IMPAR"
           ELSE
               DISPLAY "ESSE NUMERO EH PAR"
           END-IF

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
