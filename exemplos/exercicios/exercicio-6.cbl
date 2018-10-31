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
           77 ANOS PIC 9(3).
           77 MES PIC 9(3).
           77 DIA PIC 9(3).
           77 TOTAL PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE SUA IDADE:"
            ACCEPT ANOS
            DISPLAY "DIGITE OS MESES:"
            ACCEPT MES
            DISPLAY "DIGITE OS DIAS:"
            ACCEPT DIA
            COMPUTE TOTAL = (ANOS*365)+(MES*30)+DIA
            DISPLAY "SUA IDADE EM DIAS É: "TOTAL

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
