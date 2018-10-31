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
           77 WRK-HOMENS PIC 9(4).
           77 WRK-MULHERES PIC 9(4).
           77 TOTAL PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A QTD DE HOMENS"
            ACCEPT WRK-HOMENS
            DISPLAY "DIGITE A QTD DE MULHERES"
            ACCEPT WRK-MULHERES
      *     DISPLAY "HOMENS:" WRK-HOMENS "MULHERES:" WRK-MULHERES
            ADD WRK-HOMENS, WRK-MULHERES GIVING TOTAL
            IF WRK-HOMENS = WRK-MULHERES THEN
                DISPLAY "IGUAIS"
            END-IF
            IF WRK-HOMENS > WRK-MULHERES THEN
                DISPLAY "HOMENS"
            END-IF
            IF WRK-MULHERES > WRK-HOMENS THEN
                DISPLAY "MULHERES"
            END-IF
            DISPLAY "TOTAL DE PESSOAS:" TOTAL
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
