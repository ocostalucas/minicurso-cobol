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
           77 QTD_CAVALO PIC 9(4).
           77 QTD_FERRADURAS PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A QUANTIDADE DE CAVALOS"
            ACCEPT QTD_CAVALO
            COMPUTE QTD_FERRADURAS = QTD_CAVALO*4
            DISPLAY "QUANTIDADE DE FERRADURAS NECESSARIAS: "
      -     QTD_FERRADURAS
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
