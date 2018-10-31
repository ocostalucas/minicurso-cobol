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
           77 N1 PIC 9(2).
           77 N2 PIC 9(2).
           77 MEDIA PIC 9(2)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A PRIMEIRA MEDIA DO ALUNO(A):"
            ACCEPT N1
            DISPLAY "DIGITE A SEGUNDA NOTA DO ALUNO(A)"
            ACCEPT N2
            COMPUTE MEDIA = (N1+N2)/2
            DISPLAY "MEDIA FINAL DO ALUNO: "MEDIA
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
