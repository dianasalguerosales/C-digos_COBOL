       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3011.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-IN ASSIGN TO AS-FILEIN
                  FILE STATUS IS FS-FILEIN FSE-FILEIN.
           SELECT FTPMOM ASSIGN TO FTPMOM.
      *            FILE STATUS IS FS-FTPMOM FSE-FTPMOM.

       DATA DIVISION.
       FILE SECTION.
       FD FILE-IN.
           COPY FILE-IN.

       FD FTPMOM
           RECORDING MODE IS F.
       01 AREA-SALIDA                 PIC X(263).

       WORKING-STORAGE SECTION.
       01 WKS-CONTADOR                PIC 9(02)        VALUE ZEROES.
       01 WKS-HEADER1.
          05 WKS-TRANSACCION          PIC X(11)        VALUE
                "TRANSACCION".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-DIA                  PIC X(03)        VALUE "DIA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-MES                  PIC X(03)        VALUE "MES".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CLASE-CUENTA         PIC X(12)        VALUE
                "CLASE-CUENTA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-NO-CUENTA            PIC X(10)        VALUE
                "NO. CUENTA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-DOCUMENTO            PIC X(09)        VALUE "DOCUMENTO"
           .
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-AGENCIA              PIC X(07)        VALUE "ACENCIA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR                PIC X(05)        VALUE "VALOR".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-RESERVA1             PIC X(09)        VALUE "RESERVA 1"
           .
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR-RESERV-1       PIC X(15)        VALUE
                "VALOR RESERVA 1".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-RESERVA2             PIC X(09)        VALUE "RESERVA 2"
           .
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR-RESERV-2       PIC X(15)        VALUE
                "VALOR RESERVA 2".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-LEGAJO               PIC X(06)        VALUE "LEGAJO".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TERMINAL-FINANC      PIC X(19)        VALUE
                "TERMINAL FINANCIERA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-NUM-SECUENCIA        PIC X(13)        VALUE
                "NO. SECUENCIA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TIPO-MOVIMIENTO      PIC X(18)        VALUE
                "TIPO DE MOVIMIENTO".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-SIT-REGISTRO         PIC X(13)        VALUE
                "SIT. REGISTRO".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-GRAB-VERIF           PIC X(12)        VALUE
                "GRAB. VERIF."
           .
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-ORIGEN-TRANSAC       PIC X(18)        VALUE
                "ORIGEN TRANSACCION".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CAJERO               PIC X(06)        VALUE "CAJERO".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CODIGO-MONEDA        PIC X(16)        VALUE
                "CODIGO DE MONEDA".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CODIGO-MOTIVO        PIC X(06)        VALUE "MOTIVO".
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-FILLER               PIC X(06)        VALUE SPACES.

       01 WKS-REG-FTPMOM.
          05 WKS-CODIGO-TRANSACCION   PIC 9(02)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-DIA-FTP              PIC 9(02)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-MES-FTP              PIC 9(02)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CLASE-CUENTA-FTP     PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-NUMERO-CUENTA        PIC 9(10)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-DOCUMENTO-FTP.
             10 WKS-DOCUMENTO-SIGNO   PIC X(01)        VALUE ZEROES.
             10 WKS-DOCUMENTO-RESTO   PIC 99999999     VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-AGENCIA-ORIGEN       PIC 9(03)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR-FTP.
             10 WKS-VALOR-SIGNO       PIC X(01)        VALUE ZEROES.
             10 WKS-VALOR-RESTO       PIC 999999999.99 VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TIPO-RESERVA1        PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR-RESERVA1.
             10 WKS-VALRES1-SIGNO     PIC X(01)        VALUE ZEROES.
             10 WKS-VALRES1-RESTO     PIC 999999999.99 VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TIPO-RESERVA2        PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-VALOR-RESERVA2.
             10 WKS-VALRES2-SIGNO     PIC X(01)        VALUE ZEROES.
             10 WKS-VALRES2-RESTO     PIC 999999999.99 VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-LEGAJO-FTP           PIC 9(09)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TERMINAL-FINANCIERA  PIC 9(04)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-NUM-SECUENCIA-FTP    PIC 9(04)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-TIPO-MOVIMIENTO-FTP  PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-SIT-REGISTRO-FTP     PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-GRABA-VERIF          PIC X(08)        VALUE SPACES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-ORIGEN-TRANSACCION   PIC 9(02)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CAJERO-FTP           PIC 9(04)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CODIGO-MONEDA-FTP    PIC 9(01)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-CODIGO-MOTIVO-FTP    PIC 9(05)        VALUE ZEROES.
          05 FILLER                   PIC X(01)        VALUE "|".
          05 WKS-FILLER-FTP           PIC X(06)        VALUE SPACES.

       01 WKS-FIN                     PIC X(01)        VALUE SPACES.
          88 WKS-FIN-FILEIN                            VALUE HIGH-VALUES
           .
          88 WKS-FIN-FTPMOM                            VALUE HIGH-VALUES
           .

       01 WKS-CONT-LEIDOS             PIC 9(02)        VALUE ZEROES.
       01 WKS-CONT-ESCRITOS           PIC 9(02)        VALUE ZEROES.

       01 PROGRAMA                    PIC X(08)        VALUE SPACES.
       01 ARCHIVO                     PIC X(08)        VALUE SPACES.
       01 ACCION                      PIC X(10)        VALUE SPACES.
       01 LLAVE                       PIC X(32)        VALUE SPACES.

       01 FILE-STATUS.
          02 FS-FILEIN                PIC 9(2)         VALUE ZEROES.
          02 FSE-FILEIN.
             03 FSE-RETURN            PIC S9(4) COMP-5
                                                       VALUE ZEROES.
             03 FSE-FUNCTION          PIC S9(4) COMP-5
                                                       VALUE ZEROES.
             03 FSE-FEEDBACK          PIC S9(4) COMP-5
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
           OPEN INPUT FILE-IN
           OPEN OUTPUT FTPMOM

           EVALUATE TRUE
           WHEN(FS-FILEIN = 97)
                MOVE ZEROES TO FS-MOMDCO
           WHEN(FS-FILEIN = 00)
                DISPLAY "FILE STATUS : " FS-FILEIN 
                CONTINUE
           WHEN OTHER
                MOVE 'FILE-IN' TO ARCHIVO
                MOVE 'OPEN' TO ACCION
                MOVE 'LLAVE' TO LLAVE
                PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
                STOP RUN
           END-EVALUATE.
       0001-VALIDACION-E.
           EXIT.

       0002-INICIO-RUTINA SECTION.
           MOVE 'EDUK3011' TO PROGRAMA
           CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
              FS-FILEIN, FS-FILEIN.
       0002-RUTINA-E.
           EXIT.

       0003-INICIO-CIERRE SECTION.
           CLOSE FILE-IN, FTPMOM.
       0003-CIERRE-E.
           EXIT.

       0004-INICIO-PROCESO SECTION.
           READ FILE-IN
           AT END
              SET WKS-FIN-FILEIN TO TRUE
           END-READ
           MOVE 'FILE-IN' TO ARCHIVO
           MOVE 'READ' TO ACCION
           MOVE 'LLAVE' TO LLAVE
           PERFORM 0002-INICIO-RUTINA THRU 0002-RUTINA-E
           WRITE AREA-SALIDA FROM WKS-HEADER1
           PERFORM UNTIL WKS-FIN-FILEIN 

                   ADD 1 TO WKS-CONTADOR
                   MOVE FILE-CODIGO-TRANSACCION TO
                      WKS-CODIGO-TRANSACCION
                   MOVE FILE-DIA TO WKS-DIA-FTP
                   MOVE FILE-MES TO WKS-MES-FTP
                   MOVE FILE-CLASE-CUENTA TO WKS-CLASE-CUENTA-FTP
                   MOVE FILE-NUMERO-CUENTA TO WKS-NUMERO-CUENTA
                   IF FILE-DOCUMENTO < 0
                      MOVE "-" TO WKS-DOCUMENTO-SIGNO
                   ELSE
                      MOVE "+" TO WKS-DOCUMENTO-SIGNO
                   END-IF
                   MOVE FILE-DOCUMENTO TO WKS-DOCUMENTO-RESTO
                   MOVE FILE-AGENCIA-ORIGEN TO WKS-AGENCIA-ORIGEN
                   IF FILE-VALOR < 0
                      MOVE "-" TO WKS-VALOR-SIGNO
                   ELSE
                      MOVE "+" TO WKS-VALOR-SIGNO
                   END-IF
                   MOVE FILE-VALOR TO WKS-VALOR-RESTO
                   MOVE FILE-TIPO-RESERVA1 TO WKS-TIPO-RESERVA1
                   IF FILE-VALOR-RESERVA1 < 0
                      MOVE "-" TO WKS-VALRES1-SIGNO
                   ELSE
                      MOVE "+" TO WKS-VALRES1-SIGNO
                   END-IF
                   MOVE FILE-VALOR-RESERVA1 TO WKS-VALRES1-RESTO
                   MOVE FILE-TIPO-RESERVA2 TO WKS-TIPO-RESERVA2
                   IF FILE-VALOR-RESERVA2 < 0
                      MOVE "-" TO WKS-VALRES2-SIGNO
                   ELSE
                      MOVE "+" TO WKS-VALRES2-SIGNO
                   END-IF
                   MOVE FILE-VALOR-RESERVA2 TO WKS-VALRES2-RESTO
                   MOVE FILE-LEGAJO TO WKS-LEGAJO-FTP
                   MOVE FILE-TERMINAL-FINANCIERA TO
                      WKS-TERMINAL-FINANCIERA
                   MOVE FILE-NUM-SECUENCIA TO WKS-NUM-SECUENCIA-FTP
                   MOVE FILE-TIPO-MOVIMIENTO TO WKS-TIPO-MOVIMIENTO-FTP
                   MOVE FILE-SIT-REGISTRO TO WKS-SIT-REGISTRO-FTP
                   MOVE FILE-GRABA-VERIF TO WKS-GRABA-VERIF
                   MOVE FILE-ORIGEN-TRANSACCION TO
                      WKS-ORIGEN-TRANSACCION
                   MOVE FILE-CAJERO TO WKS-CAJERO-FTP
                   MOVE FILE-CODIGO-MONEDA TO WKS-CODIGO-MONEDA-FTP
                   MOVE FILE-CODIGO-MOTIVO TO WKS-CODIGO-MOTIVO-FTP
                   MOVE FILE-FILLER TO WKS-FILLER-FTP

                   WRITE AREA-SALIDA FROM WKS-REG-FTPMOM

                   READ MOMDCO
                   AT END
                      SET WKS-FIN-FILEIN TO TRUE
                   END-READ

           END-PERFORM

           DISPLAY "EL TOTAL DE REGISTRO LEIDOS ES: " WKS-CONTADOR.
       0004-PROCESO-E.
           EXIT.