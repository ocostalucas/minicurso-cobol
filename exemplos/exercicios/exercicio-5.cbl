      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      * lucro; precoUnidade; precoVenda; aluguel: 500
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 PRECO_UNIDADE PIC 9(4)v99.
           77 PRECO_VENDA PIC 9(4)v99.
           77 QTD_VENDIDA PIC 9(4)v99.
           77 LUCRO PIC 9(4)v99.
           77 RESULTADO PIC 9(4)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE O PRECO DA UNIDADE DO PAO DE MEL:"
            ACCEPT PRECO_UNIDADE
            DISPLAY "DIGITE O PRECO DE VENDA DO PAO DE MEL:"
            ACCEPT PRECO_VENDA
            DISPLAY "DIGITE A QUANTIDADE VENDIDA"
            ACCEPT QTD_VENDIDA
            COMPUTE LUCRO = (PRECO_VENDA - PRECO_UNIDADE) * QTD_VENDIDA
            COMPUTE RESULTADO = LUCRO - 500
            DISPLAY "LUCRO DE VENDA: " RESULTADO
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
