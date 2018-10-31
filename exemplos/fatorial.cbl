       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATORIAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 FACT PIC 9(4).
           77 N PIC 9(2).
           77 I PIC 9(1).
           77 IX PIC 9(2).
           77 FACTX PIC 9(3).

       PROCEDURE DIVISION.
         MOVE 5 TO N
         MOVE 0 TO I
         MOVE 1 TO FACT
         DISPLAY "FATORIAL: "
         PERFORM UNTIL I GREATER THAN N
           MOVE I TO IX
           MOVE FACT TO FACTX
           DISPLAY IX "!= " FACTX
           ADD 1 TO I
           MULTIPLY I BY FACT
             ON SIZE ERROR DISPLAY "VALOR MUITO GRANDE"
           END-MULTIPLY
         END-PERFORM.
         STOP RUN.
