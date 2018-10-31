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
           77 CUSTO-FABRICA PIC 9(5)V99.
           77 PRECO-CONSUMIDOR PIC 9(5)V99.
           77 IMPOSTO PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE O CUSTO DE FABRICA"
            ACCEPT CUSTO-FABRICA
            COMPUTE IMPOSTO = (CUSTO-FABRICA*0.73)
            COMPUTE PRECO-CONSUMIDOR = CUSTO-FABRICA+IMPOSTO
            DISPLAY "PRECO FINAL: "PRECO-CONSUMIDOR
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
