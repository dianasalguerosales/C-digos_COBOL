      ******************************************************************
      * FECHA       : 23/03/2023                                       *
      * PROGRAMADOR : DIANA STEFFANNY SALGUERO ROSALES                 *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDUC3007                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : EL PROGRAMA LEE LA ENTRADA DEL SYSIN Y EVALUA    *
      *             : SI LOS DIGITOS INGRESADOS SON DPI O              *
      * ARCHIVOS    : NO APLICA PARA ESTE CASO                         *
      * PROGRAMA(S) : NO APLICA PARA ESTE CASO                         *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3007.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.
      * FILE-CONTROL.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WKS-ITERACIONES.
          02 WKS-I-GENERAL              PIC 9(02) VALUE ZEROES.
          02 WKS-I-CORRECTOS            PIC 9(02) VALUE ZEROES.
          02 WKS-I-INCORRECTOS          PIC 9(02) VALUE ZEROES.
          02 WKS-DIGITO                 PIC 9(02) VALUE ZEROES.

       01 WKS-VARIABLES.
          88 WKS-FIN-SYSIN                        VALUE "FIN".
       02 WKS-LONGITUD                  PIC 9(02) VALUE ZEROES.
          02 WKS-SUMA                   PIC 9(03) VALUE ZEROES.
          02 WKS-OPERACIONES OCCURS 10 TIMES.
             03 WKS-NUMERO-MULTI        PIC 9(02).
             03 WKS-SUMA-TOTAL          PIC 9(03).
             03 WKS-RESTA               PIC 9(02).
             03 WKS-DIVISION            PIC 9(02).
             03 WKS-RESIDUO             PIC 9(02).

      *ARRAY PARA NUMEROS A MULTIPLICAR
       01 WKS-MULTI.
          02 FILLER                     PIC 9(01) VALUE 9.
          02 FILLER                     PIC 9(01) VALUE 8.
          02 FILLER                     PIC 9(01) VALUE 7.
          02 FILLER                     PIC 9(01) VALUE 6.
          02 FILLER                     PIC 9(01) VALUE 5.
          02 FILLER                     PIC 9(01) VALUE 4.
          02 FILLER                     PIC 9(01) VALUE 3.
          02 FILLER                     PIC 9(01) VALUE 2.
       01 WKS-TABLA-MULTI REDEFINES WKS-MULTI.
          02 WKS-ARRAY-MULTI OCCURS 8 TIMES.
             03 WKS-NUM                 PIC 9(01).

      *ARRAY PARA ALMACENAR SYSIN-DPI
       01 WKS-DPI.
          02 WKS-DPI-CORREL             PIC 9(08) VALUE ZEROES.
          02 WKS-DPI-VERIFICADOR        PIC 9(01) VALUE ZEROES.
          02 WKS-DPI-DEPARTAMENTO       PIC 9(02) VALUE ZEROES.
          02 WKS-DPI-MUNICIPIO          PIC 9(02) VALUE ZEROES.
       01 WKS-ARRAY-DPI.
          02 WKS-DPI-ARRAY OCCURS 10 TIMES.
             03 WKS-DPI-CORREL-A        PIC 9(08).
             03 WKS-DPI-VERIFICADOR-A   PIC 9(01).
             03 WKS-DPI-DEPARTAMENTO-A  PIC 9(02).
             03 WKS-DPI-MUNICIPIO-A     PIC 9(02).

       01 WKS-DIGITO-ARRAY.
          02 WKS-DIGITO-DPI OCCURS 08 TIMES
                                        PIC 9(01).

       PROCEDURE DIVISION.

       00-INICIO SECTION.

           INITIALIZE WKS-OPERACIONES(1)
           INITIALIZE WKS-ARRAY-DPI
           INITIALIZE WKS-DIGITO-ARRAY
           ACCEPT WKS-DPI FROM SYSIN

           PERFORM UNTIL WKS-FIN-SYSIN
                   ADD 1 TO WKS-I-GENERAL
                   COMPUTE WKS-LONGITUD = FUNCTION LENGTH
                      (WKS-DPI)
                   IF WKS-DPI-CORREL NUMERIC THEN
                      IF WKS-LONGITUD = 13 THEN
                         MOVE WKS-DPI TO WKS-ARRAY-DPI
                         PERFORM 01-INICIO-RECORRIDO-DPI THRU
                            01-RECORRIDO-DPI-E

                         MOVE WKS-SUMA TO
                            WKS-SUMA-TOTAL(WKS-I-GENERAL)

                         PERFORM 02-INICIO-OPERACIONES THRU
                            02-OPERACIONES-E
                      ELSE
                         DISPLAY "LA LONGITUD ES INCORRECTA"
                      END-IF
                   ELSE
                      DISPLAY "NO NUMERICO"
                   END-IF

                   ACCEPT WKS-DPI FROM SYSIN
                   IF WKS-DPI = "FIN"
                      SET WKS-FIN-SYSIN TO TRUE
                   END-IF
           END-PERFORM

           DISPLAY "CONTEO GENERAL DE DPI: " WKS-I-GENERAL
           DISPLAY "CONTEO DE DPI INCORRECTOS: " WKS-I-INCORRECTOS
           DISPLAY "CONTEO DE DPI CORRECTOS: " WKS-I-CORRECTOS
           STOP RUN.
       00-FIN-E.
           EXIT.

       01-INICIO-RECORRIDO-DPI SECTION.
           MOVE 0 TO WKS-DIGITO
           MOVE 0 TO WKS-SUMA
           PERFORM UNTIL WKS-DIGITO > 7
                   ADD 1 TO WKS-DIGITO
                   MOVE WKS-DPI-CORREL
                      TO WKS-DIGITO-ARRAY

                   IF (WKS-DIGITO-DPI(WKS-DIGITO) = 0) THEN
                      MOVE 0 TO WKS-NUMERO-MULTI(WKS-DIGITO)
                   ELSE
                      COMPUTE WKS-NUMERO-MULTI(WKS-DIGITO) =
                         WKS-DIGITO-DPI(WKS-DIGITO) *
                         WKS-NUM(WKS-DIGITO)
                   END-IF

                   ADD WKS-NUMERO-MULTI(WKS-DIGITO) TO
                      WKS-SUMA
           END-PERFORM.
       01-RECORRIDO-DPI-E.
           EXIT.

       02-INICIO-OPERACIONES SECTION.

           DIVIDE WKS-SUMA-TOTAL(WKS-I-GENERAL) BY 11
              GIVING WKS-DIVISION(WKS-I-GENERAL) REMAINDER
              WKS-RESIDUO(WKS-I-GENERAL)

           SUBTRACT WKS-RESIDUO(WKS-I-GENERAL) FROM 11
              GIVING WKS-RESTA(WKS-I-GENERAL)

           EVALUATE TRUE
           WHEN(WKS-RESTA(WKS-I-GENERAL) NOT =
              WKS-DPI-VERIFICADOR)
                DISPLAY "DPI DENEGADO: " WKS-DPI
                ADD 1 TO WKS-I-INCORRECTOS
           WHEN(WKS-RESTA(WKS-I-GENERAL) =
              WKS-DPI-VERIFICADOR)
                DISPLAY "DPI ACEPTADO: " WKS-DPI
                ADD 1 TO WKS-I-CORRECTOS
           WHEN OTHER
                DISPLAY "SE HA ENCONTRADO UN ERROR"
                        WKS-RESTA(WKS-I-GENERAL)
           END-EVALUATE.
       02-OPERACIONES-E.
           EXIT.