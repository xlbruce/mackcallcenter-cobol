       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADMINOCOR AS "PGM2".
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 DADOS-ENTRADA.
           05 OPCAO PIC 9.
               88 OPCAO-VALID VALUES 1 2 3 4.
               
       01 WS-DATE.
           05 YYYY PIC X(4).
           05 MM PIC X(2).
           05 DD PIC X(2).
           
       01 WS-DD-MM-YYYY.
           05 DD PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 MM PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 YYYY PIC X(4).
               
       01 BORDA.
           05 CANTO-SE PIC X.
           05 CANTO-IE PIC X.
           05 CANTO-SD PIC X.
           05 CANTO-ID PIC X.
           05 LINHA PIC X(75).
           05 COLUNA PIC X.
           05 SEPARADOR PIC X(69).
           
               
       SCREEN SECTION.
       
       01 TELA1.
           05 TELA1-BORDA.
               10 BLANK SCREEN.
               10 LINE 1 COLUMN 1 USING CANTO-SE.
               10 LINE 1 COLUMN 2 USING LINHA.
               10 LINE 1 COLUMN 77 USING CANTO-SD.
               10 LINE 2 COLUMN 1 USING COLUNA.
               10 LINE 2 COLUMN 77 USING COLUNA.
               10 LINE 3 COLUMN 1 USING COLUNA.
               10 LINE 3 COLUMN 77 USING COLUNA.
               10 LINE 4 COLUMN 1 USING COLUNA.
               10 LINE 4 COLUMN 77 USING COLUNA.
               10 LINE 5 COLUMN 1 USING COLUNA.
               10 LINE 5 COLUMN 77 USING COLUNA.
               10 LINE 6 COLUMN 1 USING COLUNA.
               10 LINE 6 COLUMN 77 USING COLUNA.
               10 LINE 7 COLUMN 1 USING COLUNA.
               10 LINE 7 COLUMN 77 USING COLUNA.
               10 LINE 8 COLUMN 1 USING COLUNA.
               10 LINE 8 COLUMN 77 USING COLUNA.
               10 LINE 9 COLUMN 1 USING COLUNA.
               10 LINE 9 COLUMN 77 USING COLUNA.
               10 LINE 10 COLUMN 1 USING COLUNA.
               10 LINE 10 COLUMN 77 USING COLUNA.
               10 LINE 11 COLUMN 1 USING COLUNA.
               10 LINE 11 COLUMN 77 USING COLUNA.
               10 LINE 12 COLUMN 1 USING COLUNA.
               10 LINE 12 COLUMN 77 USING COLUNA.
               10 LINE 13 COLUMN 1 USING COLUNA.
               10 LINE 13 COLUMN 77 USING COLUNA.
               10 LINE 14 COLUMN 1 USING COLUNA.
               10 LINE 14 COLUMN 77 USING COLUNA.
               10 LINE 15 COLUMN 1 USING COLUNA.
               10 LINE 15 COLUMN 77 USING COLUNA.
               10 LINE 16 COLUMN 1 USING COLUNA.
               10 LINE 16 COLUMN 77 USING COLUNA.
               10 LINE 17 COLUMN 1 USING COLUNA.
               10 LINE 17 COLUMN 77 USING COLUNA.
               10 LINE 18 COLUMN 1 USING COLUNA.
               10 LINE 18 COLUMN 77 USING COLUNA.
               10 LINE 19 COLUMN 1 USING COLUNA.
               10 LINE 19 COLUMN 77 USING COLUNA.
               10 LINE 20 COLUMN 1 USING COLUNA.
               10 LINE 20 COLUMN 77 USING COLUNA.
               10 LINE 21 COLUMN 1 USING COLUNA.
               10 LINE 21 COLUMN 77 USING COLUNA.
               10 LINE 22 COLUMN 1 USING COLUNA.
               10 LINE 22 COLUMN 77 USING COLUNA.
               10 LINE 23 COLUMN 1 USING COLUNA.
               10 LINE 23 COLUMN 77 USING COLUNA.
               10 LINE 24 COLUMN 1 USING COLUNA.
               10 LINE 24 COLUMN 77 USING COLUNA.
               10 LINE 25 COLUMN 1 USING CANTO-IE.
               10 LINE 25 COLUMN 2 USING LINHA.
               10 LINE 25 COLUMN 77 USING CANTO-ID.
           
           05 TELA1-CABECALHO.
               10 LINE 3 COLUMN 5 USING SEPARADOR.
               10 LINE 5 COLUMN 5 VALUE "TV MACK". 
               10 LINE 5 COLUMN 32 VALUE "MENU DE OCORRENCIAS".
               10 LINE 5 COLUMN 64 USING WS-DD-MM-YYYY.
               10 LINE 7 COLUMN 5 USING SEPARADOR.
           
           05 TELA1-MENU.
               10 LINE 11 COLUMN 28 VALUE "1. INCLUSAO DE OCORRENCIA".
               10 LINE 13 COLUMN 28 VALUE "2. ALTERACAO DE OCORRENCIA".
               10 LINE 15 COLUMN 28 VALUE "3. EXCLUSAO DE OCORRENCIA".
               10 LINE 17 COLUMN 28 VALUE "4. RETORNA AO MENU ANTERIOR".
               10 LINE 19 COLUMN 5 USING SEPARADOR.
           
           05 TELA1-ENTRADA.
               10 LINE 21 COLUMN 05 VALUE 
               "MENSAGENS: INFORME SUA OPCAO [".
           
               10 A1 LINE 21 COLUMN 35 PIC 9 USING OPCAO.
               10 LINE 21 COLUMN 36 VALUE "]".
               10 LINE 23 COLUMN 05 USING SEPARADOR.
           
               
       PROCEDURE DIVISION.
       INICIO.
       
           MOVE FUNCTION CHAR (193) TO CANTO-IE
           MOVE FUNCTION CHAR (219) TO CANTO-SE
           MOVE FUNCTION CHAR (192) TO CANTO-SD
           MOVE FUNCTION CHAR (218) TO CANTO-ID
           MOVE FUNCTION CHAR (180) TO COLUNA
           MOVE ALL 'Í' TO SEPARADOR
           MOVE ALL 'Ä' TO LINHA.
           
           DISPLAY TELA1-BORDA
           
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           MOVE CORRESPONDING WS-DATE TO WS-DD-MM-YYYY.
           
           DISPLAY TELA1-CABECALHO
           
           DISPLAY TELA1-MENU
           
           DISPLAY TELA1-ENTRADA
           
           PERFORM WITH TEST AFTER UNTIL OPCAO-VALID
               ACCEPT A1
           END-PERFORM
           
           EVALUATE OPCAO
               WHEN 1
                   CALL "PGM21"
               WHEN 2
                   CALL "PGM22"
               WHEN 3
                   CALL "PGM23"
               WHEN 4
                   CALL "PGM0"
           END-EVALUATE
       
       STOP RUN
