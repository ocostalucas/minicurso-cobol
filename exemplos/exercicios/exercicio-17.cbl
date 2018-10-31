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
           01 n1 pic 9(2).
           01 n2 pic 9(2).
           01 soma pic 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "digite o primeiro numero:"
            accept n1
            display "digite o segundo numero:"
            accept n2
            if n1 = 0 or n2 = 0 then
                display "os dois numeros devem ser diferentes de 0"
            else
                compute soma = n1+n2
                display "soma dos numeros" soma
            end-if
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
