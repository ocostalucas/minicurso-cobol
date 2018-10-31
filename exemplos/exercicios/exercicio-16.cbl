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
           77 VM PIC 9(4).
           77 VC PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A VELOCIDADE MAXIMA PERMITIDA:"
            ACCEPT VM
            DISPLAY "DIGITE A VELOCIDADE DO CARRO:"
            ACCEPT VC
            IF VC <= VM THEN
                DISPLAY "VELOCIDADE DENTRO DO PERMITIDO"
            ELSE
                DISPLAY "MOTORISTA ULTRAPASSOU A VELOCIDADE MÁXIMA"
            END-IF
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
