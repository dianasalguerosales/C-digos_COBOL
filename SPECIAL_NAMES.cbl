       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3006.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           CLASS VOCAL IS 'A' 'E' 'I' 'O' 'U',
           CLASS LETRA iS 'A' THRU 'I' 'J' THRU 'R' 'S' THRU 'Z'
           CLASS PAR IS '0' '2' '4' '6' '8',
           CLASS NUMERO IS '0' '1' '2' '3' '4' '5' '6' '7' '8' '9'.

       INPUT-OUTPUT SECTION.
      * FILE-CONTROL.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WKS-CAMPO                         PIC X(08) VALUE SPACES.
           88 WKS-FIN-CAMPOS                          VALUE "FIN".

       PROCEDURE DIVISION.

       00-INICIO-BEGIN SECTION.

           ACCEPT WKS-CAMPO FROM SYSIN

           PERFORM UNTIL WKS-FIN-CAMPOS

               ACCEPT WKS-CAMPO FROM SYSIN
      *             IF WKS-CAMPO = "FIN"
      *                 SET WKS-FIN-CAMPOS TO TRUE
      *             END-IF

               IF WKS-CAMPO NUMERIC THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO NUMERICO'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO NUMERICO'
               END-IF

               IF WKS-CAMPO NUMERO THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO NUMERO'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO NUMERO'
               END-IF

               IF WKS-CAMPO ALPHABETIC THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO ALFABETICO'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO ALFABETICO'
               END-IF

               IF WKS-CAMPO PAR THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO PAR'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO PAR'
               END-IF

               IF WKS-CAMPO ALPHABETIC THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO ALFABETICO'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO ALFABETICO'
               END-IF

               IF WKS-CAMPO LETRA THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO LETRA'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO LETRA'
               END-IF

               IF WKS-CAMPO VOCAL THEN
                   DISPLAY WKS-CAMPO ' SI LO ACEPTO COMO VOCAL'
               ELSE
                   DISPLAY WKS-CAMPO ' NO LO ACEPTO COMO VOCAL'
               END-IF

           END-PERFORM

           STOP RUN.
       00-BEGIN-E. EXIT.