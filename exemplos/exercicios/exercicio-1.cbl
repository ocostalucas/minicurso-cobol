       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 AREA_QUADRADO PIC 9(3).
           77 RESULTADO PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A AREA DO QUADRADO:"
            ACCEPT AREA_QUADRADO
            COMPUTE RESULTADO = AREA_QUADRADO*AREA_QUADRADO
            DISPLAY "RESULTADO " RESULTADO
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.