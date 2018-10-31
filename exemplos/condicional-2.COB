       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDICIONAL0002.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 NOTA PIC 9(2).


       PROCEDURE DIVISION.
       MENU.
           DISPLAY "NOTA: "
           ACCEPT NOTA

      *O comando EVALUATE fornece uma notação abreviada para uma série
      * de declarações IF.
           EVALUATE NOTA
           WHEN < 5
               DISPLAY "REPROVADO"
           WHEN >= 5 AND < 7
               DISPLAY "RECUPERACAO"
           WHEN >= 7 AND <= 10
               DISPLAY "APROVADO"
           WHEN OTHER
               DISPLAY "NOTA INVALIDA"
           STOP RUN.

       END PROGRAM CONDICIONAL0002.
