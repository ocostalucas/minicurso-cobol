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
           77 HORAS PIC 9(4).
           77 MINUTOS PIC 9(4).
           77 RESULTADO PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE AS HORAS:".
            ACCEPT HORAS.
            DISPLAY "DIGITE OS MINUTOS:".
            ACCEPT MINUTOS.

            COMPUTE RESULTADO = (HORAS*60)+MINUTOS.

            DISPLAY "TOTAL DE MINUTOS:"RESULTADO.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
