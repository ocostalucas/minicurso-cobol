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
           77 salario pic 9(4)v99.
           77 qwatts pic 9(4)v99.
           77 valor pic 9(4)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Digite seu salario:"
            accept salario
            display "Digite a qtd de qWatts"
            accept qwatts
            compute valor = (salario*0.01)*qwatts
            display "valor a ser pago:" valor
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
