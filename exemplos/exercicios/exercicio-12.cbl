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
           77 N1 PIC S9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE UM NUMERO"
            ACCEPT N1
            IF N1 = 0 THEN
                DISPLAY "ESSE NUMERO EH IGUAL A 0"
                ELSE
                    IF N1 > 0 THEN
                        DISPLAY "ESSE NUMERO EH POSITIVO"
                    ELSE
                        DISPLAY "ESSE NUMERO EH NEGATIVO"
                    END-IF
            END-IF

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
