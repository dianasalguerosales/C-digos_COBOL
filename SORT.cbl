       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3008.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN TO SORTWK1.

       DATA DIVISION.
       FILE SECTION.

       SD SORT-FILE.
       01 SOR-DATOS-ESTUDIANTE-SORT.
          02 SOR-CODIGO-ESTUDIANTE      PIC 9(05).
          02 FILLER                     PIC X(01).
          02 SOR-NOMBRE-ESTUDIANTE      PIC X(50).
          02 FILLER                     PIC X(01).
          02 SOR-EDAD-ESTUDIANTE        PIC 9(02).
          02 FILLER                     PIC X(01).
          02 SOR-FECHA-NACIMIENTO-E
                                        PIC 9(08).
          02 FILLER                     PIC X(01).
          02 SOR-GENERO-ESTUDIANTE      PIC X(01).
             88 SD-FINAL                          VALUE HIGH-VALUES.

       WORKING-STORAGE SECTION.
       01 WKS-CONTADOR-F                PIC 9(02) VALUE ZEROES.
       01 WKS-CONTADOR-M                PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-TOTAL                PIC 9(02) VALUE ZEROES.
       01 WKS-TOTAL                     PIC 9(02) VALUE ZEROES.
       01 WKS-FECHA-ENTERA              PIC X(20) VALUE ZEROES.
       01 WKS-FECHA-ACTUAL              PIC 9(08) VALUE ZEROES.
       01 WKS-DIAS-CUENTA               PIC 9(05) VALUE ZEROES.
       01 WKS-EDAD                      PIC 9(02) VALUE ZEROES.

       01 WKS-DATOS-ESTUDIANTE-ENTRADA.
          02 WKS-CODIGO-ESTUDIANT-E     PIC 9(05) VALUE ZEROES.
          02 FILLER                     PIC X(01).
          02 WKS-NOMBRE-ESTUDIANT-E     PIC X(50) VALUE ZEROES.
          02 FILLER                     PIC X(01).
          02 WKS-EDAD-ESTUDIANT-E       PIC 9(02) VALUE ZEROES.
          02 FILLER                     PIC X(01).
          02 WKS-FECHA-NAC-ESTUDIANT-E  PIC 9(08) VALUE ZEROES.
          02 FILLER                     PIC X(01).
          02 WKS-GENERO-ESTIDIANT-E     PIC X(01) VALUE ZEROES.
          02 FILLER                     PIC X(03).
             88 WKS-FINAL                         VALUE "FIN".

       01 WKS-DATOS-ESTUDIANTE-SALIDA.
          05 WKS-CODIGO-ESTUDIANT-S     PIC 9(05) VALUE ZEROES.
          05 WKS-NOMBRE-ESTUDIANT-S     PIC X(50) VALUE ZEROES.
          05 WKS-EDAD-ESTUDIANTE-S      PIC 9(02) VALUE ZEROES.
          05 WKS-FECHA-NAC-ESTUDIANT-S  PIC 9(08) VALUE ZEROES.
          05 WKS-GENERO-ESTIDIANT-S     PIC X(01) VALUE ZEROES.

       PROCEDURE DIVISION.

       0000-INICIO-SORT SECTION.

           SORT SORT-FILE ON ASCENDING KEY SOR-CODIGO-ESTUDIANTE
                          ON ASCENDING KEY SOR-EDAD-ESTUDIANTE
              INPUT PROCEDURE IS
              0001-INICIO-PROCESA-DATOS
              THRU 0001-PROCESA-DATOS-E
              OUTPUT PROCEDURE IS
              0002-INICIO-ESCRITUTA-DATOS
              THRU 0002-ESCRITUTA-DATOS-E
           STOP RUN.
       0000-SORT-E.            EXIT.

       0001-INICIO-PROCESA-DATOS SECTION.

           DISPLAY "CCCCCNNNNN50EEFFFFFFFFG"
           MOVE 0 TO WKS-CONTADOR-F
           MOVE 0 TO WKS-CONTADOR-M
           MOVE 0 TO WKS-CONT-TOTAL
           ACCEPT WKS-DATOS-ESTUDIANTE-ENTRADA FROM SYSIN
           PERFORM UNTIL WKS-FINAL
                   MOVE WKS-DATOS-ESTUDIANTE-ENTRADA TO
                      SOR-DATOS-ESTUDIANTE-SORT
                   RELEASE SOR-DATOS-ESTUDIANTE-SORT
                   ADD 1 TO WKS-CONT-TOTAL
                   EVALUATE TRUE
                   WHEN(WKS-GENERO-ESTIDIANT-E = "F")
                        ADD 1 TO WKS-CONTADOR-F
                   WHEN(WKS-GENERO-ESTIDIANT-E = "M")
                        ADD 1 TO WKS-CONTADOR-M
                   WHEN OTHER
                        DISPLAY "EL GENERO DEL ESTUDIANTE INGRESADO ES "
                                "INVALIDO, GENERO LEIDO FUE: "
                                WKS-GENERO-ESTIDIANT-E
                   END-EVALUATE

                   ACCEPT WKS-DATOS-ESTUDIANTE-ENTRADA FROM SYSIN
                   IF WKS-DATOS-ESTUDIANTE-ENTRADA = "FIN"
                      SET WKS-FINAL TO TRUE
                   END-IF
                   PERFORM 0003-INICIO-EDAD THRU 0003-EDAD-E
                   MOVE WKS-EDAD TO
                      SOR-EDAD-ESTUDIANTE
           END-PERFORM.

           ADD WKS-CONTADOR-F TO WKS-CONTADOR-M GIVING WKS-TOTAL
           DISPLAY "CONTADORES:"
                   " "
           DISPLAY "EL TOTAL DE HOMBRES Y MUJERES ES: " WKS-TOTAL
           DISPLAY "EL TOTAL DE MUJERES ES: " WKS-CONTADOR-F
           DISPLAY "EL TOTAL DE HOMBRES ES: " WKS-CONTADOR-M
           DISPLAY "EL TOTAL DE ESTUDIANTES ES: " WKS-CONT-TOTAL
           DISPLAY "---------------------------------------------".

       0001-PROCESA-DATOS-E.            EXIT.

       0002-INICIO-ESCRITUTA-DATOS SECTION.
           RETURN SORT-FILE
           AT END
              SET SD-FINAL TO TRUE
           END-RETURN

           PERFORM UNTIL SD-FINAL
                   MOVE SOR-DATOS-ESTUDIANTE-SORT TO
                      WKS-DATOS-ESTUDIANTE-SALIDA
                   MOVE SOR-CODIGO-ESTUDIANTE TO
                      WKS-CODIGO-ESTUDIANT-S
                   MOVE SOR-NOMBRE-ESTUDIANTE TO
                      WKS-NOMBRE-ESTUDIANT-S
                   MOVE SOR-EDAD-ESTUDIANTE TO
                      WKS-EDAD-ESTUDIANTE-S
                   MOVE SOR-FECHA-NACIMIENTO-E TO
                      WKS-FECHA-NAC-ESTUDIANT-S
                   MOVE SOR-GENERO-ESTUDIANTE TO
                      WKS-GENERO-ESTIDIANT-S

      *             DISPLAY SOR-DATOS-ESTUDIANTE-SORT "->POSIBLE
                   DISPLAY WKS-DATOS-ESTUDIANTE-SALIDA
                   RETURN SORT-FILE
                   AT END
                      SET SD-FINAL TO TRUE
                   END-RETURN
           END-PERFORM.
       0002-ESCRITUTA-DATOS-E.            EXIT.

       0003-INICIO-EDAD SECTION.
           MOVE FUNCTION CURRENT-DATE TO WKS-FECHA-ENTERA
           MOVE WKS-FECHA-ENTERA(1:8) TO WKS-FECHA-ACTUAL
           COMPUTE WKS-DIAS-CUENTA = FUNCTION
              INTEGER-OF-DATE(WKS-FECHA-ACTUAL) -
              FUNCTION
              INTEGER-OF-DATE(SOR-FECHA-NACIMIENTO-E)

           COMPUTE WKS-EDAD = WKS-DIAS-CUENTA / 365.

       0003-EDAD-E.  EXIT.