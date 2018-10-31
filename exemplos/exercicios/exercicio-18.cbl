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
           01 SEXO PIC A(1).
           01 PESO PIC 9(3).
           01 ALTURA PIC 9(1)V99.
           01 RESULTADO PIC 9(3)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE O SEXO:"
            ACCEPT SEXO
            DISPLAY "DIGITE O PESO:"
            ACCEPT PESO
            DISPLAY "DIGITE A ALTURA:"
            ACCEPT ALTURA

            IF SEXO = 'M' OR SEXO = 'm' THEN
                DISPLAY "SEXO MASACULINO"
                COMPUTE RESULTADO = (PESO/(ALTURA**2))
                DISPLAY RESULTADO
                IF RESULTADO > 22 THEN
                    DISPLAY "VOCÊ ESTÁ ACIMA DO PESO IDEAL"
                ELSE
                    IF RESULTADO = 22 THEN
                        DISPLAY "VOCẼ ESTÁ NO PESO IDEAL"
                    ELSE
                        DISPLAY "VOCÊ ESTÁ ABAIXO DO PESO IDEAL"
                END-IF
            ELSE
                IF SEXO = 'F' OR SEXO = 'f' THEN
                    DISPLAY "SEXO FEMININO"
                    COMPUTE RESULTADO = (PESO/(ALTURA**2))
                    IF RESULTADO > 20.8 THEN
                        DISPLAY "VOCÊ ESTÁ ACIMA DO PESO IDEAL"
                    ELSE
                        IF RESULTADO = 20.8 THEN
                            DISPLAY "VOCÊ ESTÁ NO PESO IDEAL"
                        ELSE
                            DISPLAY "VOCÊ ESTÁ ABAIXO DO PESO IDEAL"
                    END-IF
                ELSE
                    DISPLAY "DIGITE 'F' OU 'M' PARA O SEXO! - OU - INFOR"
      -"MAÇÕES EQUIVOCADAS"
                 END-IF
            END-IF
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
