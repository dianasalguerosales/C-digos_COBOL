       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3010.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-A ASSIGN TO FILE-A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS FILEA-LLAVE
                  FILE STATUS IS FS-FILE-A FSE-FILE-A.
           SELECT FILE-B ASSIGN TO FILE-B
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS FILEB-LLAVE
                  FILE STATUS IS FS-FILE-B FSE-FILE-B.
           SELECT FILE-C ASSIGN TO FILE-C
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS FILEC-LLAVE
                  FILE STATUS IS FS-FILE-C FSE-FILE-C.

       DATA DIVISION.
       FILE SECTION.
       FD FILE-A.
           COPY FILE-A.

       FD FILE-B.
           COPY FILE-B.

       FD FILE-C.
           COPY FILE-C.

       WORKING-STORAGE SECTION.
       01 WKS-FIN               PIC X(1).
          88 WKS-FIN-FILE-A               VALUE HIGH-VALUES.
          88 WKS-FIN-FILE-B               VALUE HIGH-VALUES.
          88 WKS-FIN-FILE-C               VALUE HIGH-VALUES.

       01 WKS-CONT-GENERAL      PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-IGUAL        PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-OTROS        PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-NO-EXISTE-A  PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-NO-EXISTE-B  PIC 9(02) VALUE ZEROES.
       01 WKS-CONT-NO-EXISTE-C  PIC 9(02) VALUE ZEROES.

       01 PROGRAMA              PIC X(08) VALUE SPACES.
       01 ARCHIVO               PIC X(08) VALUE SPACES.
       01 ACCION                PIC X(10) VALUE SPACES.
       01 LLAVE                 PIC X(32) VALUE SPACES.

       01 FILE-STATUS.
          02 FS-FILE-A          PIC 9(2)  VALUE ZEROES.
          02 FSE-FILE-A.
             03 FSE-RETURN      PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FUNCTION    PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FEEDBACK    PIC S9(4) COMP-5
                                          VALUE ZEROES.
          02 FS-FILE-B          PIC 9(2)  VALUE ZEROES.
          02 FSE-FILE-B.
             03 FSE-RETURN      PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FUNCTION    PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FEEDBACK    PIC S9(4) COMP-5
                                          VALUE ZEROES.
          02 FS-FILE-C          PIC 9(2)  VALUE ZEROES.
          02 FSE-FILE-C.
             03 FSE-RETURN      PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FUNCTION    PIC S9(4) COMP-5
                                          VALUE ZEROES.
             03 FSE-FEEDBACK    PIC S9(4) COMP-5
                                          VALUE ZEROES.

       PROCEDURE DIVISION.

       0000-INICIO SECTION.
           PERFORM 0001-INICIO-VALIDACION THRU 0001-VALIDACION-E
           PERFORM 0004-INICIO-PROCESO THRU 0004-PROCESO-E
           PERFORM 0003-INICIO-CIERRE THRU 0003-CIERRE-E
           STOP RUN.
       0000-INICIO-E.
           EXIT.

       0001-INICIO-VALIDACION SECTION.
           OPEN INPUT FILE-A
           OPEN INPUT FILE-B
           OPEN INPUT FILE-C
      *VALIDACION FILE-A
           EVALUATE TRUE
           WHEN(FS-FILE-A = 97)
                MOVE ZEROES TO FS-FILE-A
           WHEN(FS-FILE-A = 00)
                DISPLAY "FILE STATUS : " FS-FILE-A
                CONTINUE
           WHEN OTHER
                MOVE 'FILE-A' TO ARCHIVO
                MOVE 'OPEN' TO ACCION
                MOVE 'LLAVE' TO LLAVE
                PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                STOP RUN
           END-EVALUATE
      *VALIDACION FILE-B
           EVALUATE TRUE
           WHEN(FS-FILE-B = 97)
                MOVE ZEROES TO FS-FILE-B
           WHEN(FS-FILE-B = 00)
                DISPLAY "FILE STATUS : " FS-FILE-B
                CONTINUE
           WHEN OTHER
                MOVE 'FILE-B' TO ARCHIVO
                MOVE 'OPEN' TO ACCION
                MOVE 'LLAVE' TO LLAVE
                PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                STOP RUN
           END-EVALUATE
      *VALIDACION FILE-C
           EVALUATE TRUE
           WHEN(FS-FILE-C = 97)
                MOVE ZEROES TO FS-FILE-B
           WHEN(FS-FILE-C = 00)
                DISPLAY "FILE STATUS : " FS-FILE-C
                CONTINUE
           WHEN OTHER
                MOVE 'FILE-C' TO ARCHIVO
                MOVE 'OPEN' TO ACCION
                MOVE 'LLAVE' TO LLAVE
                PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                STOP RUN
           END-EVALUATE.
       0001-VALIDACION-E.
           EXIT.

       0002-INICIO-RUTINA SECTION.
           MOVE 'EDUK3010' TO PROGRAMA
           CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
              FS-FILE-A, FSE-FILE-A,
              FS-FILE-B, FSE-FILE-B,
              FS-FILE-C, FSE-FILE-C.
       0002-RUTINA-E.
           EXIT.

       0003-INICIO-CIERRE SECTION.
           CLOSE FILE-A, FILE-B, FILE-C.
       0003-CIERRE-E.
           EXIT.

       0004-INICIO-PROCESO SECTION.
           PERFORM 0041-INICIO-LECTURA1 THRU 0041-LECTURA1-E
           PERFORM UNTIL(WKS-FIN-FILE-A) OR (WKS-FIN-FILE-B)
              OR (WKS-FIN-FILE-C)
                   ADD 1 TO WKS-CONT-GENERAL

                   EVALUATE TRUE
                   WHEN(FILEA-LLAVE > FILEB-LLAVE OR
                      FILEB-LLAVE > FILEA-LLAVE)
                        READ FILE-C
                        AT END
                           SET WKS-FIN-FILE-C TO TRUE
                        END-READ
                        IF FS-FILE-C NOT = 00
                           MOVE 'FILE-C' TO ARCHIVO
                           MOVE 'READ' TO ACCION
                           MOVE 'FILEC-LLAVE' TO LLAVE
                           PERFORM 0002-INICIO-RUTINA THRU
                              0002-RUTINA-E
                        END-IF
                        IF FILEB-LLAVE > FILEA-LLAVE THEN
                           ADD 1 TO WKS-CONT-NO-EXISTE-B
                           DISPLAY "LA CUENTA: "
                                   FILEA-LLAVE
                                   " NO EXISTE"
                                   " EN EL ARCHIVO FILE-B"
                           READ FILE-A
                           AT END
                              SET WKS-FIN-FILE-A TO TRUE
                           END-READ
                           IF FS-FILE-A NOT = 00
                              MOVE 'FILE-A' TO ARCHIVO
                              MOVE 'READ' TO ACCION
                              MOVE 'FILEA-LLAVE' TO LLAVE
                              PERFORM 0002-INICIO-RUTINA THRU
                                 0002-RUTINA-E
                           END-IF
                        ELSE
                           ADD 1 TO WKS-CONT-NO-EXISTE-A
                           DISPLAY "LA CUENTA: "
                                   FILEB-LLAVE
                                   " NO EXISTE"
                                   " EN EL ARCHIVO FILE-A"
                           READ FILE-B
                           AT END
                              SET WKS-FIN-FILE-B TO TRUE
                           END-READ
                           IF FS-FILE-B NOT = 00
                              MOVE 'FILE-B' TO ARCHIVO
                              MOVE 'READ' TO ACCION
                              MOVE 'FILEB-LLAVE' TO LLAVE
                              PERFORM 0002-INICIO-RUTINA THRU
                                 0002-RUTINA-E
                           END-IF
                        END-IF

                   WHEN(FILEA-LLAVE > FILEC-LLAVE OR
                      FILEC-LLAVE > FILEA-LLAVE)
                        READ FILE-B
                        AT END
                           SET WKS-FIN-FILE-B TO TRUE
                        END-READ
                        IF FS-FILE-B NOT = 00
                           MOVE 'FILE-B' TO ARCHIVO
                           MOVE 'READ' TO ACCION
                           MOVE 'FILEB-LLAVE' TO LLAVE
                           PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                        END-IF
                        IF FILEC-LLAVE > FILEA-LLAVE THEN
                           ADD 1 TO WKS-CONT-NO-EXISTE-C
                           DISPLAY "LA CUENTA: "
                                   FILEA-LLAVE
                                   " NO EXISTE"
                                   " EN EL ARCHIVO FILECIB"
                           READ FILE-A
                           AT END
                              SET WKS-FIN-FILE-A TO TRUE
                           END-READ
                           IF FS-FILE-A NOT = 00
                              MOVE 'FILE-A' TO ARCHIVO
                              MOVE 'READ' TO ACCION
                              MOVE 'FILEA-LLAVE' TO LLAVE
                              PERFORM 0002-INICIO-RUTINA THRU
                                 0002-RUTINA-E
                           END-IF
                        ELSE
                           ADD 1 TO WKS-CONT-NO-EXISTE-A
                           DISPLAY "LA CUENTA: "
                                   FILEC-LLAVE
                                   " NO EXISTE"
                                   " EN EL ARCHIVO FILE-A"
                           READ FILE-C
                           AT END
                              SET WKS-FIN-FILE-C TO TRUE
                           END-READ
                           IF FS-FILE-C NOT = 00
                              MOVE 'FILE-C' TO ARCHIVO
                              MOVE 'READ' TO ACCION
                              MOVE 'FILEC-LLAVE' TO LLAVE
                              PERFORM 0002-INICIO-RUTINA THRU
                                 0002-RUTINA-E
                           END-IF
                        END-IF

                   WHEN(FILEA-LLAVE = FILEB-LLAVE AND
                      FILEA-LLAVE = FILEC-LLAVE)
                        ADD 1 TO WKS-CONT-IGUAL
                        READ FILE-B
                        AT END
                           SET WKS-FIN-FILE-B TO TRUE
                        END-READ

                        READ FILE-C
                        AT END
                           SET WKS-FIN-FILE-C TO TRUE
                        END-READ

                        READ FILE-A
                        AT END
                           SET WKS-FIN-FILE-A TO TRUE
                        END-READ

                   WHEN OTHER
                        ADD 1 TO WKS-CONT-OTROS
                        DISPLAY "SE ENCONTRO UNA ANOMALIA"
                                " CONVENIENTE REVIZAR: "
                        DISPLAY "FILE STATUS - FIL STATUS EXT - LLAVE"
                                FS-FILE-A,
                                FSE-FILE-A,
                                FILEA-LLAVE,
                                FS-FILE-B,
                                FSE-FILE-B,
                                FILEB-LLAVE,
                                FS-FILE-C,
                                FSE-FILE-C,
                                FILEC-LLAVE
                        STOP RUN
                   END-EVALUATE

           END-PERFORM
           DISPLAY "EL TOTAL DE REGISTROS LEIDOS ES: "
                   WKS-CONT-GENERAL
           DISPLAY "EL TOTAL DE REGISTROS NO EXISTENTES EN FILE-A ES: "
                   WKS-CONT-NO-EXISTE-A
           DISPLAY "EL TOTAL DE REGISTROS NO EXISTENTES EN FILE-B ES: "
                   WKS-CONT-NO-EXISTE-B
           DISPLAY "EL TOTAL DE REGISTROS NO EXISTENTES EN FILECIB ES: "
                   WKS-CONT-NO-EXISTE-C
           IF WKS-CONT-IGUAL < WKS-CONT-GENERAL THEN
              DISPLAY " ATENCION!!! SOLAMENTE: "
                      WKS-CONT-IGUAL
                      " FUERON ENCONTRADOS IGUALES"
           ELSE
              DISPLAY "SUS ARCHIVOS CUENTAN CON INTEGRIDAD"
           END-IF
           DISPLAY "SE ENCONTRARON REGISTROS NO EVALUADOS, TOTAL: "
                   WKS-CONT-OTROS.
       0004-PROCESO-E.
           EXIT.

       0041-INICIO-LECTURA1 SECTION.
           READ FILE-A
           AT END
              SET WKS-FIN-FILE-A TO TRUE
           END-READ
           IF FS-FILE-A NOT = 00
              MOVE 'FILE-A' TO ARCHIVO
              MOVE 'READ' TO ACCION
              MOVE 'FILEA-LLAVE' TO LLAVE
              PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
           END-IF

           PERFORM 3 TIMES
                   READ FILE-B
                   AT END
                      SET WKS-FIN-FILE-B TO TRUE
                   END-READ
                   IF FS-FILE-B NOT = 00
                      MOVE 'FILE-B' TO ARCHIVO
                      MOVE 'READ' TO ACCION
                      MOVE 'FILEB-LLAVE' TO LLAVE
                      PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                   END-IF
           END-PERFORM

           READ FILE-C
           AT END
              SET WKS-FIN-FILE-C TO TRUE
           END-READ
           IF FS-FILE-C NOT = 00
              MOVE 'FILE-C' TO ARCHIVO
              MOVE 'READ' TO ACCION
              MOVE 'FILEC-LLAVE' TO LLAVE
              PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
           END-IF.
       0041-LECTURA1-E.
           EXIT.