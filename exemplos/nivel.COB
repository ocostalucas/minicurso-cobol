       IDENTIFICATION DIVISION.
       PROGRAM-ID. NIVEL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA.
           10 DIA PIC 9(2) VALUE 27.
           10 MES PIC 9(2) VALUE 10.
           10 ANO PIC 9(4) VALUE 2018.
           66 DIA-MES RENAMES DIA THRU MES.

      *Nível 66 é utilizado para renomear outras variáveis do programa

      *Nível 77 é utilizado para variável que não possui relacionamento
      *definido com nenhuma outra variável.

       77 COR PIC A(4) VALUE 'AZUL'.

      *Nível 78 é utilizado para declarar constante
       78 PI VALUE 3.14159.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY WS-DATA
            DISPLAY DIA
            DISPLAY DIA-MES

            STOP RUN.
       END PROGRAM NIVEL.
