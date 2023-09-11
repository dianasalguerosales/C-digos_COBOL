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

       01 WKS-REGISTRO1.
          02 WKS-REG-DAT1.
             03 WKS-VALIDAR           PIC 9(01) VALUE ZEROES.
             03 WKS-PRIMERO           PIC 9(01) VALUE ZEROES.
             03 WKS-SEGUNDO           PIC 9(01) VALUE ZEROES.
             03 WKS-TERCERO           PIC 9(01) VALUE ZEROES.
             03 WKS-CUARTO            PIC 9(01) VALUE ZEROES.
             03 WKS-QUINTO            PIC 9(01) VALUE ZEROES.
             03 WKS-SEXTO             PIC 9(01) VALUE ZEROES.
             03 WKS-SEPTIMO           PIC 9(01) VALUE ZEROES.
             03 WKS-OCTAVO            PIC 9(01) VALUE ZEROES.
             03 WKS-NOVENO            PIC 9(01) VALUE ZEROES.
             03 WKS-DEPARTAMENTO      PIC 9(02) VALUE ZEROES.
             03 WKS-MUNICIPIO         PIC 9(02) VALUE ZEROES.

          02 WKS-REGISTRO_ARRAY1 REDEFINES WKS-REG-DAT1.
             03 WKS-DATOS1            PIC 9(14).

       01    REDEFINES WKS-REGISTRO1.
          88 WKS-FIN1                           VALUE "FIN".
       02 FILLER                      PIC X(14).

       01 WKS-VARIABLES.
          02 WKS-VALIDACION           PIC 9(01) VALUE ZEROES.
          02 WKS-LONGITUD             PIC 9(02) VALUE ZEROES.
          02 WKS-LONG-NIT             PIC 9(02) VALUE ZEROES.
          02 WKS-DPI1                 PIC 9(13) VALUE ZEROES.
          02 WKS-NIT                  PIC 9(05) VALUE ZEROES.
          02 WKS-NIT-NOVENO           PIC 9(01) VALUE ZEROES.
          02 WKS-NIT-NUMERO           PIC 9(09) VALUE ZEROES.
          02 WKS-NIT-ENTERO           PIC X(09) VALUE ZEROES.
          02 WKS-NIT-ENTERO-STRG      PIC X(09) VALUE ZEROES.
          02 WKS-AUX                  PIC 9(14) VALUE ZEROES.
          02 WKS-RECORRIDO_NIT        PIC 9(09) VALUE ZEROES.
          02 WKS-DPI-GUARDADO OCCURS 08 TIMES.
             03 WKS-NUMERO-DPI        PIC 9(01).
          02 WKS-NIT-GUARDADO OCCURS 08 TIMES.
             03 WKS-NUMERO-NIT        PIC 9(01).
          02 WKS-NIT-NUMERO-GUARDADO  PIC 9(01)
                OCCURS 08 TIMES.
          02 WKS-RECORRIDO_DPI        PIC 9(08) VALUE ZEROES.
          02 WKS-RECORRIDO_NIT        PIC 9(08) VALUE ZEROES.
          02 WKS-REGISTRO1-GUARDADO OCCURS 10.
             03 WKS-VALIDA            PIC 9(01).
             03 WKS-OCHO-DIG          PIC 9(08).
             03 WKS-RESTO             PIC 9(05).

       01 WKS-ITERACIONES.
      *   02 WKS-I-GENERAL                       PIC 9(02) VALUE ZEROES.
          02 WKS-I-REGISTRO1          PIC 9(02) VALUE ZEROES.
          02 WKS-I                    PIC 9(02) VALUE ZEROES.
          02 WKS-I-NIT                PIC 9(02) VALUE ZEROES.

       01 WKS-OPERACIONES.
          02 WKS-SUMA-TOTAL           PIC 9(03) VALUE ZEROES.
          02 WKS-RESTA                PIC 9(03) VALUE ZEROES.
          02 WKS-DIVISION             PIC 9(02) VALUE ZEROES.
          02 WKS-RESIDUO              PIC 9(02) VALUE ZEROES.
          02 WKS-MULTI-GUARDADO OCCURS 08 TIMES.
             03 WKS-NUMERO-MULTI      PIC 9(02).

       01 WKS-MULTI.
          02                          PIC 9(01) VALUE 9.
          02                          PIC 9(01) VALUE 8.
          02                          PIC 9(01) VALUE 7.
          02                          PIC 9(01) VALUE 6.
          02                          PIC 9(01) VALUE 5.
          02                          PIC 9(01) VALUE 4.
          02                          PIC 9(01) VALUE 3.
          02                          PIC 9(01) VALUE 2.
       01 WKS-TABLA-MULTI REDEFINES WKS-MULTI.
          02 WKS-ARRAY-MILTI OCCURS 8 TIMES.
             03 WKS-NUM               PIC 9(01).

       PROCEDURE DIVISION.

       00-INICIO-BEGIN SECTION.
           ACCEPT WKS-REGISTRO1 FROM SYSIN
           MOVE 0 TO WKS-I-REGISTRO1

           PERFORM UNTIL WKS-FIN1
      *DESCOMPONIENDO
                   DISPLAY "------------------------"
                   ADD 1 TO WKS-I-REGISTRO1
                   DISPLAY WKS-I-REGISTRO1 " REGISTRO"
                   MOVE WKS-REGISTRO_ARRAY1 TO
                      WKS-REGISTRO1-GUARDADO(WKS-I-REGISTRO1)
                   DISPLAY WKS-OCHO-DIG(WKS-I-REGISTRO1) " OCHO"
                   MOVE WKS-REGISTRO1-GUARDADO(WKS-I-REGISTRO1) TO
                      WKS-DPI1
                   MOVE WKS-DPI1(2:9) TO WKS-NIT-ENTERO
                   DISPLAY WKS-NIT-ENTERO " nit entero"
                   MOVE WKS-OCHO-DIG(WKS-I-REGISTRO1) TO
                      WKS-RECORRIDO_DPI
                   MOVE WKS-VALIDA(WKS-I-REGISTRO1) TO WKS-VALIDACION
                   DISPLAY WKS-VALIDACION " nUMERO DE VALIDACION"
                   COMPUTE WKS-LONGITUD = FUNCTION LENGTH(WKS-DPI1)

                   IF WKS-OCHO-DIG(WKS-I-REGISTRO1) NUMERIC THEN
                      DISPLAY WKS-OCHO-DIG(WKS-I-REGISTRO1)
                              " registros es"
                      IF WKS-LONGITUD = 13 THEN
                         DISPLAY WKS-LONGITUD " longitud es"
      *AQUI CODIGO DE DPI
                         PERFORM 01-INICIO-OPERANDO THRU 01-OPERANDO-E
                      ELSE
                         DISPLAY "LA LONGITUD ES INCORRECTA"
                      END-IF
                   ELSE
                      DISPLAY "no es numero"
      *  AQUI NIT
                      PERFORM 01-INICIO-NIT THRU 01-NIT-E
                   END-IF

                   DISPLAY "------------------------"

      *ACEPTANDO VALORES
                   ACCEPT WKS-REGISTRO1 FROM SYSIN
                   IF WKS-REGISTRO1 = "FIN"
                      SET WKS-FIN1 TO TRUE
                   END-IF

           END-PERFORM
           STOP RUN.
       00-BEGIN-E.            EXIT.

       01-INICIO-OPERANDO SECTION.

      *operando valides
           EVALUATE TRUE
           WHEN(WKS-VALIDACION = 1)

                DISPLAY WKS-VALIDACION " numero valida"
                PERFORM UNTIL WKS-I > 7
                        ADD 1 TO WKS-I
                        MOVE WKS-RECORRIDO_DPI(WKS-I:1)
                           TO WKS-DPI-GUARDADO(WKS-I)
                        DISPLAY WKS-NUMERO-DPI(WKS-I)
                                " DIGITO DPI"
                        COMPUTE WKS-NUMERO-MULTI(WKS-I) =
                           WKS-NUMERO-DPI(WKS-I) *
                           WKS-NUM(WKS-I)
                        DISPLAY WKS-NUM(WKS-I) " NUMERO SECUENCIA"
                        DISPLAY WKS-NUMERO-MULTI(WKS-I)
                                " MULTIPLICACION"

                        ADD WKS-NUMERO-MULTI(WKS-I)
                           TO WKS-SUMA-TOTAL
                        DISPLAY WKS-SUMA-TOTAL
                                " ES SUMA"
                        DIVIDE WKS-SUMA-TOTAL BY 11 GIVING
                           WKS-DIVISION REMAINDER WKS-RESIDUO
                        SUBTRACT 11 FROM WKS-RESIDUO GIVING
                           WKS-RESTA
                        DISPLAY WKS-RESTA " La resta es"
                        EVALUATE TRUE
                        WHEN(WKS-RESTA = 10)
                             DISPLAY "DPI DENEGADO"
                        WHEN(WKS-RESTA < 10)
                             DISPLAY "DPI ACEPTADO"
                        WHEN(WKS-RESTA > 10)
                             DISPLAY "ALERTA!!!"
                        WHEN OTHER
                             DISPLAY "SE HA ENCONTRADO UN ERROR"
                        END-EVALUATE
                END-PERFORM
           WHEN(WKS-VALIDACION = 2)
                PERFORM 01-INICIO-NIT THRU 01-NIT-E
           WHEN OTHER
                DISPLAY " SE ENCONTRO UN ERROR"
           END-EVALUATE.
      * fin de operando valides

       01-OPERANDO-E.                       EXIT.

       01-INICIO-NIT SECTION.
      *INICIO NIT
           INSPECT WKS-NIT-ENTERO REPLACING ALL "-" BY
              ZEROES
           DISPLAY WKS-NIT-ENTERO " nit entero"
           INSPECT WKS-NIT-ENTERO TALLYING WKS-I-NIT FOR
              ALL SPACES
           DISPLAY WKS-I-NIT " nUMERO DE ESPACIOS"
           STRING SPACES DELIMITED BY SIZE
                  WKS-NIT-ENTERO DELIMITED BY SIZE
              INTO WKS-NIT-ENTERO-STRG
           DISPLAY WKS-NIT-ENTERO-STRG " nUMERO espacion co"
           COMPUTE WKS-LONG-NIT = FUNCTION
              LENGTH(WKS-NIT-ENTERO-STRG)
           MOVE WKS-NIT-ENTERO-STRG TO WKS-NIT-NUMERO
           DISPLAY WKS-NIT-NUMERO " niN BORAR ESTO Y MOVE"
           DISPLAY WKS-LONG-NIT " LARGO ES"

           IF WKS-LONG-NIT = 9 THEN
              DISPLAY " NIT ES DE LONGITUD 9"
      *OPERANDO IGUAL DPI
              DISPLAY WKS-VALIDACION " numero valida"
              PERFORM UNTIL WKS-I > 7

                      ADD 1 TO WKS-I
                      MOVE WKS-RECORRIDO_DPI(WKS-I:1)
                         TO WKS-NIT-GUARDADO(WKS-I)

                      DISPLAY WKS-NUMERO-NIT(WKS-I)
                              " DIGITO NIT"

                      COMPUTE WKS-NUMERO-MULTI(WKS-I) =
                         WKS-NUMERO-NIT(WKS-I) *
                         WKS-NUM(WKS-I)
                      DISPLAY WKS-NUM(WKS-I) " NUMERO SECUENCIA"
                      DISPLAY WKS-NUMERO-MULTI(WKS-I)
                              " MULTIPLICACION"

                      ADD WKS-NUMERO-MULTI(WKS-I)
                         TO WKS-SUMA-TOTAL
                      DISPLAY WKS-SUMA-TOTAL
                              " ES SUMA"
                      DIVIDE WKS-SUMA-TOTAL BY 11 GIVING
                         WKS-DIVISION REMAINDER WKS-RESIDUO
                      SUBTRACT 11 FROM WKS-RESIDUO GIVING
                         WKS-RESTA
                      DISPLAY WKS-RESTA " La resta es"

                      MOVE WKS-NIT-ENTERO-STRG(9:1) TO WKS-NIT-NOVENO
                      EVALUATE TRUE
                      WHEN(WKS-RESTA = 10)
                           DISPLAY "DPI DENEGADO"
                           IF WKS-NIT-NOVENO = "K" THEN
                              DISPLAY "NIT ACEPTADO"
                           END-IF
                      WHEN(WKS-RESTA < 10)
                           DISPLAY "DPI ACEPTADO"
                      WHEN(WKS-RESTA > 10)
                           IF WKS-NIT-NOVENO = "K" THEN
                              DISPLAY "NIT ACEPTADO"
                           END-IF
                      WHEN OTHER
                           DISPLAY "SE HA ENCONTRADO UN ERROR"
                      END-EVALUATE
              END-PERFORM
      *FIN OPERANDO
           ELSE
              DISPLAY "POR FAVOR VERIFICAR NUMERO DE NIT"
           END-IF.
      *FIN NIT
       01-NIT-E.                       EXIT.