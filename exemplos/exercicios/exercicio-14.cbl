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
           77 LADO1 PIC 9(4).
           77 LADO2 PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE O LADO 1:"
            ACCEPT LADO1
            DISPLAY "DIGITE O LADO 2:"
            ACCEPT LADO2

            IF LADO1 = LADO2 THEN
                DISPLAY "ESTE É UM QUADRADO PERFEITO"
                ELSE
                    DISPLAY "ESTE É UM RETANGULO"
            END-IF

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
