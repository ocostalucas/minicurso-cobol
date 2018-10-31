       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 NUM1 PIC 999.
           77 NUM2 PIC 999.
           77 OPCAO PIC 99.
           77 RESULT PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTRE COM 2 VALORES".
           ACCEPT NUM1.
           ACCEPT NUM2.

           DISPLAY "====== OPCOES ======".
           DISPLAY "1 - ADICAO".
           DISPLAY "2 - SUBTRACAO".
           DISPLAY "3 - MULTIPLICACAO".
           DISPLAY "4 - DIVISAO".
           DISPLAY "====================".
           ACCEPT OPCAO.


           EVALUATE OPCAO
            WHEN  1 PERFORM SOMA
            WHEN  2 PERFORM SUB
            WHEN  3 PERFORM MULT
            WHEN  4 PERFORM DIV
            WHEN OTHER
               DISPLAY "OPCAO INVALIDA!"
           END-EVALUATE.


           SOMA.
               ADD NUM1 NUM2 GIVING RESULT.
               DISPLAY "SOMA: " RESULT.
               STOP RUN.

           SUB.
               SUBTRACT NUM2 FROM NUM1 GIVING RESULT.
               DISPLAY 'SUBTRACAO: ' RESULT.
               STOP RUN.

           MULT.
               MULTIPLY NUM1 BY NUM2 GIVING RESULT.
               DISPLAY 'MULTIPLICACAO: ' RESULT.
               STOP RUN.

           DIV.
               DIVIDE NUM1 BY NUM2 GIVING RESULT.
               DISPLAY 'DIVISAO: ' RESULT.
               STOP RUN.

       END PROGRAM CALCULADORA.
