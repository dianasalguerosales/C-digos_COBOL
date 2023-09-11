       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3005.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           UPSI-0 IS SW-0 ON STATUS IS ENGLISH
                          OFF STATUS IS SPANISH.

       INPUT-OUTPUT SECTION.
      * FILE-CONTROL.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WKS-REGISTRO.
          02 WKS-FECHA-INGRESADA.
             04 WKS-ANIO-INGRESADO         PIC 9(04) VALUE ZEROES.
             04 WKS-DIA-INGRESADO          PIC 9(02) VALUE ZEROES.
             04 WKS-MES-INGRESADO          PIC 9(02) VALUE ZEROES.

          02 WKS-FECHA-INGRESADA-NUM REDEFINES WKS-FECHA-INGRESADA.
              03 WKS-FECHA                  PIC 9(08).

          02 FILLER                         PIC X(12).

       01 REDEFINES WKS-REGISTRO.
          02 FILLER                         PIC X(03).
             88 WKS-FIN-FECHAS                       VALUE "FIN".
          02 FILLER                         PIC X(17).

       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-CUENTA-FECHAS               PIC 9(02) VALUE ZEROES.
          02 WKS-CUENTA-PROCESOS             PIC 9(02) VALUE ZEROES.
             88 WKS-LLEGO-AL-LIMITE                    VALUE 11.

          02 WKS-FECHA-GUARDADA              PIC 9(08) OCCURS 10.

          02 WKS-RESTA-DIAS                  PIC 9(05) VALUE ZEROES.
          02 WKS-DIA                         PIC 9(02) VALUE ZEROES.
          02 WKS-MES                         PIC 9(02) VALUE ZEROES.
          02 WKS-FECHA-ITE                   PIC 9(08) VALUE ZEROES.
          02 WKS-ANHO-SALIDA                 PIC 9(04) VALUE ZEROES.
          02 WKS-DIA-STR                     PIC X(09) VALUE SPACES.

       01 WKS-MESDIA.
          02                             PIC X(12) VALUE '31ENERO     '.
          02                             PIC X(12) VALUE '28FEBRERO   '.
          02                             PIC X(12) VALUE '31MARZO     '.
          02                             PIC X(12) VALUE '30ABRIL     '.
          02                             PIC X(12) VALUE '31MAYO      '.
          02                             PIC X(12) VALUE '30JUNIO     '.
          02                             PIC X(12) VALUE '31JULIO     '.
          02                             PIC X(12) VALUE '31AGOSTO    '.
          02                             PIC X(12) VALUE '30SEPTIEMBRE'.
          02                             PIC X(12) VALUE '31OCTUBRE   '.
          02                             PIC X(12) VALUE '31NOVIEMBRE '.
          02                             PIC X(12) VALUE '30DICIEMBRE '.
       01 WKS-TABLA-MESDIA REDEFINES WKS-MESDIA.
          02 WKS-ARRAY-MESDIA OCCURS 12  TIMES.
             03 WKS-DIA-NUM              PIC 9(02).
             03 WKS-MES-LET              PIC X(10).

       01 WKS-MESDIA-ING.
          02                             PIC X(12) VALUE '31JANUARY   '.
          02                             PIC X(12) VALUE '28FEBRUARY  '.
          02                             PIC X(12) VALUE '31MARCH     '.
          02                             PIC X(12) VALUE '30APRIL     '.
          02                             PIC X(12) VALUE '31MAY       '.
          02                             PIC X(12) VALUE '30JUNE      '.
          02                             PIC X(12) VALUE '31JULY      '.
          02                             PIC X(12) VALUE '31AUGUST    '.
          02                             PIC X(12) VALUE '30SEPTEMBER '.
          02                             PIC X(12) VALUE '31OCTOBER   '.
          02                             PIC X(12) VALUE '31NOVEMBER  '.
          02                             PIC X(12) VALUE '30DECEMBER  '.
       01 WKS-TABLA-MESDIA-ING REDEFINES WKS-MESDIA-ING.
          02 WKS-ARRAY-MESDIA-ING OCCURS 12 TIMES.
             03 WKS-DIA-NUM-ING             PIC 9(02).
             03 WKS-MES-LET-ING             PIC X(10).

       PROCEDURE DIVISION.

       00-INICIO-BEGIN SECTION.
      * accept necesario para pasar primer dato a PERFORM UNTIL

           ACCEPT WKS-REGISTRO FROM SYSIN
           MOVE 0 TO WKS-CUENTA-FECHAS

           PERFORM UNTIL WKS-FIN-FECHAS
               ADD 1 TO WKS-CUENTA-FECHAS
               MOVE WKS-FECHA-INGRESADA-NUM TO
                   WKS-FECHA-GUARDADA(WKS-CUENTA-FECHAS)
               ACCEPT WKS-REGISTRO FROM SYSIN
                   IF WKS-REGISTRO = "FIN"
                       SET WKS-FIN-FECHAS TO TRUE
                   END-IF
           END-PERFORM

           MOVE 1 TO WKS-CUENTA-PROCESOS

           PERFORM UNTIL WKS-CUENTA-PROCESOS GREATER WKS-CUENTA-FECHAS
                   OR WKS-LLEGO-AL-LIMITE

                   MOVE WKS-FECHA-GUARDADA(WKS-CUENTA-PROCESOS) TO
                        WKS-FECHA-ITE
                   COMPUTE WKS-RESTA-DIAS =
                      FUNCTION INTEGER-OF-DATE(WKS-FECHA-ITE) -
                      FUNCTION INTEGER-OF-DATE(19900101)

                   COMPUTE WKS-DIA = FUNCTION REM (WKS-RESTA-DIAS 7)
                   MOVE WKS-DIA TO WKS-DIA-STR
                   EVALUATE TRUE
                       WHEN (WKS-DIA = 0)
                           MOVE "LUNES"     TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "MONDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 1)
                           MOVE "MARTES"    TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "TUESDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 2)
                           MOVE "MIERCOLES" TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "WEDNESDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 3)
                           MOVE "JUEVES"    TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "THURSDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 4)
                           MOVE "VIERNES"   TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "FRIDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 5)
                           MOVE "SABADO"    TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "SATURDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN (WKS-DIA = 6)
                           MOVE "DOMINGO"   TO WKS-DIA-STR
                           IF ENGLISH THEN
                                   MOVE "SUNDAY" TO WKS-DIA-STR
                           END-IF
                       WHEN OTHER
                           DISPLAY "ERROR EN CALCULO DIA"
                   END-EVALUATE

                   MOVE WKS-FECHA-ITE(5:2) TO WKS-MES
                   MOVE WKS-FECHA-ITE(1:4) TO WKS-ANHO-SALIDA

                   ADD 1 TO WKS-CUENTA-PROCESOS

                   IF ENGLISH THEN
                       DISPLAY WKS-RESTA-DIAS ' days ago was 1 of Janua'
                       'ry of 1990, and today is ' WKS-DIA-STR ' day '
                       'of ' WKS-MES-LET-ING( WKS-MES) ' of '
                       WKS-ANHO-SALIDA ' and the last day of this '
                       'month will be ' WKS-DIA-NUM(WKS-MES)
                       DISPLAY " "
                   ELSE
                       DISPLAY 'Hace ' WKS-RESTA-DIAS ' días fue 1 de '
                       'enero de 1990, y hoy es ' WKS-DIA-STR 'to. dia '
                       WKS-MES-LET(WKS-MES) ' de ' WKS-ANHO-SALIDA
                       ' y el ultimo dia del mes será '
                       WKS-DIA-NUM(WKS-MES)
                       DISPLAY " "
                   END-IF
           END-PERFORM
           STOP RUN.
       00-BEGIN-E. EXIT.