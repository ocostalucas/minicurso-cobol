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
           77 DISTANCIA PIC 9(4).
           77 QTD-LITRO PIC 9(4).
           77 CUSTO PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A DISTANCIA ENTRE AS CIDADES:"
            ACCEPT DISTANCIA
            COMPUTE QTD-LITRO = DISTANCIA/10
            COMPUTE CUSTO = QTD-LITRO * 3
            DISPLAY "DISTANCIA PLANEJADA:" DISTANCIA
            DISPLAY "QUANTIDADE DE GASOLINA NECESSARIA: "QTD-LITRO
            DISPLAY "CUSTO TOTAL: "CUSTO
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
