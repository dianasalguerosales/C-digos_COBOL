      *ESTA DIVISION EL PROGRAMA LO INTERPRETA COMO COMENTARIO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUC3002.
       AUTHOR. DIANA SALGUERO.

      *ESTA DIVISION  ES EL HEADER
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

      *ESTA DIVISION ES PARA DECLARAR VARIABLES
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WKS-REGISTRO.
          02 WKS-CAMPO1      PIC X(01) VALUE ZEROES.
          02 FILLER          PIC X(01) VALUE ZEROES.
          02 WKS-CAMPO2      PIC X(01) VALUE ZEROES.
          02 FILLER          PIC X(01) VALUE ZEROES.
          02 WKS-CAMPO3      PIC X(01) VALUE ZEROES.
          02 FILLER          PIC X(01) VALUE ZEROES.
          02 WKS-CAMPO4      PIC X(01) VALUE ZEROES.
          02 FILLER          PIC X(01) VALUE ZEROES.
          02 WKS-CAMPO5      PIC X(01) VALUE ZEROES.
          02 FILLER          PIC X(01) VALUE ZEROES.

       01 WKS-REGISTRO2.
          02 WKS-CAMPO           PIC X(01) OCCURS 5.
             88 WKS-LETRA2                 VALUE "A" THRU "I"
                                                 "J" THRU "S"
                                                 "T" THRU "Z".
             88 WKS-VOCAL2                 VALUE "A" "E" "I" "0" "U".
             88 WKS-NUMERO2                VALUE "0" THRU "9".
             88 WKS-PAR2                   VALUE "0" "2" "4" "6" "8".
             88 WKS-IMPAR2                 VALUE "1" "3" "5" "7" "9".

       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-EVALUADOR       PIC X(01).
             88 WKS-LETRA1                 VALUE "A" THRU "I"
                                                 "J" THRU "S"
                                                 "T" THRU "Z".
             88 WKS-VOCAL1                 VALUE "A" "E" "I" "0" "U".
             88 WKS-NUMERO1                VALUE "0" THRU "9".
             88 WKS-PAR1                   VALUE "0" "2" "4" "6" "8".
             88 WKS-IMPAR1                 VALUE "1" "3" "5" "7" "9".
       01 WKS-X               PIC 9(01)    VALUE ZEROES.

      *AQUI INICIA EL PROCESO DEL PROGRAMA PARA EVALUAR DATOS
       PROCEDURE DIVISION.
      *SECCION PRINCIPAL
       000-INICIO-MAIN SECTION.
           ACCEPT WKS-REGISTRO FROM SYSIN
           MOVE 1 TO WKS-X
           MOVE WKS-CAMPO1 TO WKS-EVALUADOR
           PERFORM 010-INICIO-EVALUACION1   
           MOVE 2 TO WKS-X
           MOVE WKS-CAMPO2 TO WKS-EVALUADOR
           PERFORM 010-INICIO-EVALUACION1 
           MOVE 3 TO WKS-X
           MOVE WKS-CAMPO3 TO WKS-EVALUADOR
           PERFORM 010-INICIO-EVALUACION1 
           MOVE 4 TO WKS-X
           MOVE WKS-CAMPO4 TO WKS-EVALUADOR
           PERFORM 010-INICIO-EVALUACION1 
           MOVE 5 TO WKS-X
           MOVE WKS-CAMPO5 TO WKS-EVALUADOR
           PERFORM 010-INICIO-EVALUACION1 

           PERFORM 030-INICIO-MENSAJE

           ACCEPT WKS-REGISTRO2 FROM SYSIN
           MOVE 0 TO WKS-X
           PERFORM 020-INICIO-EVALUACION2 UNTIL WKS-X = 5
           STOP RUN.
       000-MAIN-E. EXIT.

       010-INICIO-EVALUACION1.
           IF WKS-LETRA1
              DISPLAY "CAMPO " WKS-X ": " WKS-EVALUADOR " ES ALFABETICO"
              IF WKS-VOCAL1
                 DISPLAY "CAMPO " WKS-X ": " WKS-EVALUADOR " ES VOCAL"
              ELSE
                 DISPLAY "CAMPO " WKS-X ": "  WKS-EVALUADOR
                         " ES CONSONANTE"
              END-IF
           ELSE
              IF WKS-NUMERO1
                 DISPLAY "CAMPO " WKS-X ": " WKS-EVALUADOR
                         " ES NUMERICO"
                 IF WKS-PAR1
                    DISPLAY "CAMPO " WKS-X ": " WKS-EVALUADOR " ES PAR"
                 ELSE
                    DISPLAY "CAMPO " WKS-X ": "  WKS-EVALUADOR
                    " ES IMPAR"
                 END-IF
              END-IF
           END-IF.
       010-EVALUACION1-E. EXIT.

       030-INICIO-MENSAJE.
           DISPLAY " ".
           DISPLAY "SECCION DE LA SEGUNDA FORMA".
           DISPLAY " ".
       010-MENSAJE-E. EXIT.

       020-INICIO-EVALUACION2.
           ADD 1 TO WKS-X.
           IF WKS-LETRA2(WKS-X)
              DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                      " ES ALFABETICO"
              IF WKS-VOCAL2(WKS-X)
                 DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                         " ES VOCAL"
              ELSE
                 DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                         " ES CONSONANTE"
              END-IF
           ELSE
              IF WKS-NUMERO2(WKS-X)
                 DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                         " ES NUMERICO"
                 IF WKS-PAR2(WKS-X)
                    DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                            " ES PAR"
                 ELSE
                    DISPLAY "CAMPO " WKS-X ": " WKS-CAMPO(WKS-X)
                            " ES IMPAR"
                 END-IF
              END-IF
           END-IF.
       010-EVALUACION2-E. EXIT.