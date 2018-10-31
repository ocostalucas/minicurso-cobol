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
           77 QTD-CRIANCAS PIC 9(3).
           77 QTD-LITROS PIC 9(3).
           77 VALOR-TOTAL PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A QUANTIDADE DE CRIANCAS POR TURMA"
            ACCEPT QTD-CRIANCAS
            COMPUTE QTD-LITROS = (QTD-CRIANCAS*3)/5
            COMPUTE VALOR-TOTAL = (QTD-LITROS*3)
            DISPLAY "QTD DE LITROS NECESSARIOS: "QTD-LITROS
            DISPLAY "CUSTO FINAL: "VALOR-TOTAL
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
