******************************************************************
      * FECHA       : 23/03/2023                                       *
      * PROGRAMADOR : DIANA STEFFANNY SALGUERO ROSALES                 *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : OVERFLO1                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : VIENDO COMPORTAMIENTOD DE OVERFLOW               *
      * ARCHIVOS    : NO APLICA PARA ESTE CASO                         *
      * PROGRAMA(S) : NO APLICA PARA ESTE CASO                         *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERFLO1.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.
      * FILE-CONTROL.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 CAMPOS-STRING.
          02 CAMPO1      PIC X(18) VALUE "WHERE DOES THIS GO".
          02 CAMPO2      PIC X(30) VALUE
                "THIS IS THE DESTINATION STRING".
          02 CAMPO3      PIC X(15) VALUE "HERE IS ANOTHER".

       01 PUNTERO.
          02 PUNTERO1    PIC 99.
          02 PUNTERO2    PIC 99.

       PROCEDURE DIVISION.

       01-INICIO SECTION.
           MOVE 4 TO PUNTERO2
           STRING CAMPO1 DELIMITED BY "THIS"
                  CAMPO3 DELIMITED BY SPACE
                  "TOM" DELIMITED BY SIZE
              INTO CAMPO2 WITH POINTER PUNTERO2
           ON OVERFLOW
              DISPLAY "STRING ERROR"
           NOT ON OVERFLOW
               DISPLAY CAMPO2
           END-STRING
           STOP RUN.
       01-FIN-E.
           EXIT.