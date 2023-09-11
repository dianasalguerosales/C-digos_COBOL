      ******************************************************************
      * FECHA       : 17/03/2023                                       *
      * PROGRAMADOR : DIANA STEFFANNY SALGUERO ROSALES                 *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EJINTRIS                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PRUEBA DE FUNCIONES INTRINSECAS - GRUPO 2        *
      *             :                                                  *
      * ARCHIVOS    : NO APLICA PARA ESTE CASO                         *
      *                                                                *
      * PROGRAMA(S) : NO APLICA PARA ESTE CASO                         *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJINTRIS.
       AUTHOR. DIANA SALGUERO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 NOMBRE                      PIC X(8)  VALUE "FERNANDO".
       01 APELLIDO                    PIC X(10) VALUE "MAZARIEGOS".
       01 DIRECCION                   PIC X(15) VALUE "2CALLE 19-6 Z11".
       01 LARGO-NOMBRE                PIC 9(2)  VALUE ZEROES.
       01 NOMBRE-COMPLETO             PIC X(19) VALUE ZEROES.
       01 FECHA-ACTUAL                PIC 9 VALUE  ZEROES.
       01 HORA-ACTUAL                 PIC 9(8)  VALUE ZEROES.
       01 FECHA-ENTERA                PIC 9(8)  VALUE ZEROES.
       01 DIAS-CUENTA                 PIC 9(2)  VALUE ZEROES.
       01 CALLE                       PIC X(6)  VALUE ZEROES.
       01 NUMERO                      PIC X(4)  VALUE ZEROES.
       01 OTROS-CAMPOS                PIC X(3)  VALUE SPACES.
       01 CONTADOR                    PIC 9(2)  VALUE ZEROES.
       01 ALFA                        PIC X(3)  VALUE ZEROES.
       01 ALFA-B                      PIC X(20) VALUE "-$12,345.67CR".
       01 ALFA-2                      PIC X(6)  VALUE "123X56".
       01 NUMERICO                    PIC 9(6)  VALUE ZEROES.

       PROCEDURE DIVISION.

       00-INICIO-BEGIN SECTION.
      ******************************************************************
      *       FUNCION PARA UNIR VARIAS CADENAS EN UN SOLO STRING       *
      ******************************************************************
           DISPLAY "FUNCION PARA UNIR VARIAS CADENAS EN UN SOLO STRING"

           MOVE NOMBRE TO NOMBRE-COMPLETO
           STRING " ", APELLIDO DELIMITED BY SIZE INTO
           NOMBRE-COMPLETO(9:11)

           DISPLAY "EL NOMBRE COMPLETO ES: " NOMBRE-COMPLETO
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA SEPARAR UN CADENA EN VARIOS STRINGS         *
      ******************************************************************
           DISPLAY "FUNCION PARA SEPARAR UN CADENA EN VARIOS STRINGS"

           UNSTRING DIRECCION DELIMITED BY " " INTO CALLE, NUMERO,
           OTROS-CAMPOS
           DISPLAY "LOS DATOS SON: " CALLE
           DISPLAY NUMERO
           DISPLAY OTROS-CAMPOS
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA OBTENER EL LARGO DE UN STRING               *
      ******************************************************************
           DISPLAY "FUNCION PARA OBTENER EL LARGO DE UN STRING"

           MOVE LENGTH OF NOMBRE-COMPLETO TO LARGO-NOMBRE
           DISPLAY "LARGO DE NOMBRE ES: " LARGO-NOMBRE
           DISPLAY "NOMBRE ES: " NOMBRE-COMPLETO
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA RECORRER UN CADENA Y CAMBIAR CARACTERES     *
      ******************************************************************
           DISPLAY "FUNCION PARA RECORRER UN CADENA/CAMBIAR CARACTERES"

           INSPECT NOMBRE-COMPLETO TALLYING CONTADOR FOR ALL "A"
           INSPECT NOMBRE-COMPLETO REPLACING ALL "A" BY "E"
           DISPLAY "CAMBIO DE LETRAS ES: " NOMBRE-COMPLETO
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA OBTENER LA FECHA DEL SISTEMA                *
      ******************************************************************
           DISPLAY "FUNCION PARA OBTENER LA FECHA DEL SISTEMA"

           MOVE FUNCTION CURRENT-DATE TO FECHA-ACTUAL
           DISPLAY "LA FECHA ES: " FECHA-ACTUAL
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA OBTEBER FECHA COMO ENTERO PARA RESTAR       *
      ******************************************************************
           DISPLAY "FUNCION PARA OBTEBER FECHA COMO ENTERO PARA RESTAR"

           COMPUTE DIAS-CUENTA = FUNCTION INTEGER-OF-DATE (20230317)
                               - FUNCTION INTEGER-OF-DATE (20230313)
           DISPLAY "LA FECHA ENTERA ES: " DIAS-CUENTA
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA VALIDAR UN CAMPO NUMERICO                   *
      ******************************************************************
           DISPLAY "FUNCION PARA VALIDAR UN CAMPO NUMERICO"

           MOVE ALFA-2 TO NUMERICO
           IF NUMERICO NUMERIC THEN
              MOVE ZEROES TO NUMERICO
              COMPUTE NUMERICO = FUNCTION NUMVAL(ALFA-2)
              DISPLAY "OPERACION VALIDA: " NUMERICO
           ELSE
              DISPLAY "OPERACION INVALIDA" NUMERICO
           END-IF
           DISPLAY "--------------------------------------------------"
      ******************************************************************
      *       FUNCION PARA OBTEBER FECHA COMO ENTERO PARA RESTAR       *
      ******************************************************************
      *    DISPLAY "FUNCION PARA OBTEBER FECHA COMO ENTERO PARA RESTAR"
      *
      *    COMPUTE DIAS-CUENTA = FUNCTION INTEGER-OF-DATE (20230317) -
      *                          FUNCTION INTEGER-OF-DATE (20230313)
      *    DISPLAY "LA FECHA ENTERA ES: " DIAS-CUENTA
      *   DISPLAY "--------------------------------------------------"
           ACCEPT FECHA-ACTUAL FROM DAY-OF-WEEK
           DISPLAY "EL DIA DE LA SEMANA: " FECHA-ACTUAL
      ******************************************************************
      *       FUNCION PARA CONVERTIR UN STRING EN ENTERO Y OPERAR      *
      ******************************************************************
      *    DISPLAY "FUNCION PARA CONVERTIR UN STRING EN ENTERO Y OPERAR"
      *
      *    FUNCTION NUMVAL-C (ALFA-B)
      *    DISPLAY "EL TOTAL ES: " ALFA-B
      *    DISPLAY "--------------------------------------------------"
           STOP RUN.
       00-BEGIN-E. EXIT.