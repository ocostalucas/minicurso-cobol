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
           77 N3 PIC 9(2).
           77 MEDIA_POND PIC 9(2)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE A PRIMERIA NOTA DO ALUNO"
            ACCEPT N1
            DISPLAY "DIGITE A SEGUNDA NOTA DO ALUNO"
            ACCEPT N2
            DISPLAY "DIGITE A TERCEIRA NOTA DO ALUNO"
            ACCEPT N3
            COMPUTE MEDIA_POND = ((N1)+(N2*2)+(N3*3))/6
            DISPLAY "RESULTADO: "MEDIA_POND
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
