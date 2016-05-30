       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTERA_OCORR AS "PGM22".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-OCORR ASSIGN TO "D:\OCORRENCIAS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS O-CPF
               FILE STATUS IS ST-OCORR.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-OCORR.
       01 REG-OCORRENCIA.
           05 O-CPF PIC X(11).
           05 O-NOME PIC X(30).
           05 O-DATA-OCORR.
               10 YYYY PIC 9999.
               10 MM PIC 99.
               10 DD PIC 99.
           05 O-HORA-OCORR.
               10 HH PIC 99.
               10 MM PIC 99.
           05 O-ATENDENTE PIC X(30).
           05 O-DESCRICAO PIC X(70).
           05 O-STAT PIC 9.
           05 O-ANDAMENTO PIC 9.
       
       
       WORKING-STORAGE SECTION.
       
       01 WS-OCORRENCIA.
           05 CPF PIC X(11).
           05 NOME PIC X(30).
           05 DATA-OCORR.
               10 YYYY PIC X(4).
               10 MM PIC X(2).
               10 DD PIC X(2).
           05 HORA-OCORR.
               10 HH PIC 99.
               10 MM PIC 99.
           05 ATENDENTE PIC X(30).
           05 DESCRICAO PIC X(70).
           05 STAT PIC 9.
           05 ANDAMENTO PIC 9.
       
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
           
       01 OCORR-DD-MM-YYYY.
           05 DD PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 MM PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 YYYY PIC X(4).
           
       01 WS-TIME.
           05 HH PIC 99.
           05 MM PIC 99.
           
       01 OCORR-HH-MM.
           05 HH PIC 9(2).
           05 FILLER PIC X VALUE ':'.
           05 MM PIC 9(2).
           
       01 LINHA.
           05 FILLER VALUE "   ".
           05 L PIC X(72) VALUE ALL "Í".
           05 FILLER VALUE "    ".
           
       77 CONFIRM PIC X.
       77 MSG PIC X(40).
       77 FOUND-CPF PIC X.
       
       77 ST-CLIENTE PIC XX VALUES SPACES.
       77 ST-OCORR PIC XX VALUE SPACES.
       
       SCREEN SECTION.

       01 TELA1.
           05 BLANK SCREEN.
           05 LINE 01 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 03 COLUMN 35 VALUE "MACKALLCENTER".
           05 LINE 05 COLUMN 04 VALUE "ALTERACAO DE OCORRENCIA".
           05 LINE 05 COLUMN 66 USING WS-DD-MM-YYYY.
           05 LINE 07 COLUMN 01 PIC X(80) FROM LINHA.
           
           05 LINE 09 COLUMN 04 VALUE "CPF DO CLIENTE.: [           ]".
           
           05 LINE 10 COLUMN 04 VALUE "NOME DO CLIENTE: [".
           05 LINE 10 COLUMN 52 VALUE "]".
           
           05 LINE 11 COLUMN 04 VALUE "DATA OCORRENCIA: [".
           05 LINE 11 COLUMN 32 VALUE "]".
           
           05 LINE 12 COLUMN 04 VALUE "HORA OCORRENCIA: [".
           05 LINE 12 COLUMN 27 VALUE "]".
               
           05 LINE 13 COLUMN 04 VALUE "ATENDENTE......: [".
           05 LINE 13 COLUMN 52 VALUE "]".
           
           05 LINE 15 COLUMN 04 VALUE "DESCRICAO DA OCORRENCIA: ".
           
           05 LINE 19 COLUMN 04 VALUE "STATUS...: [ ]".
           
           05 LINE 19 COLUMN 40 VALUE "ANDAMENTO: [ ]".
           
           05 LINE 21 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 22 COLUMN 05 VALUE "MENSAGENS: ".
           05 LINE 23 COLUMN 01 PIC X(80) FROM LINHA.
           
       01 TELA1-VALUES.
           05 LINE 10 COLUMN 22 USING NOME.
           05 LINE 11 COLUMN 22 USING OCORR-DD-MM-YYYY.
           05 LINE 12 COLUMN 22 USING OCORR-HH-MM.
           05 LINE 13 COLUMN 22 USING ATENDENTE.
           05 LINE 15 COLUMN 29 USING DESCRICAO.
           05 LINE 19 COLUMN 16 USING STAT.
           05 LINE 19 COLUMN 52 USING ANDAMENTO.
        
       01 TELA1-ALTERA.
           05 NEW-DESC LINE 15 COLUMN 29 PIC X(70) TO DESCRICAO.
           05 NEW-STAT LINE 19 COLUMN 16 PIC 9 TO STAT.
           05 NEW-ANDAMENTO LINE 19 COLUMN 52 PIC 9 TO ANDAMENTO.
           
       01 TELA1-MENSAGEM.
           05 LINE 22 COLUMN 16 USING MSG.
           05 PRESS-ENTER LINE 22 COLUMN 80 TO CONFIRM.
       
       PROCEDURE DIVISION.
       
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           MOVE CORRESPONDING WS-DATE TO WS-DD-MM-YYYY.
       
           DISPLAY TELA1.
           
                 
           ACCEPT O-CPF WITH LENGTH-CHECK FULL AT LINE 09 COLUMN 22
           
           
           OPEN I-O ARQ-OCORR
               READ ARQ-OCORR RECORD INTO WS-OCORRENCIA
                   KEY IS O-CPF
                   INVALID KEY MOVE "N" TO FOUND-CPF
                   NOT INVALID KEY MOVE "S" TO FOUND-CPF
               END-READ
           CLOSE ARQ-OCORR

           IF FOUND-CPF = "S"
               MOVE CORRESPONDING O-DATA-OCORR TO OCORR-DD-MM-YYYY
               MOVE CORRESPONDING O-HORA-OCORR TO OCORR-HH-MM
               DISPLAY TELA1-VALUES
               MOVE "TECLE ENTER PARA CONTINUAR" TO MSG
               DISPLAY TELA1-MENSAGEM
               ACCEPT PRESS-ENTER
           ELSE    
               MOVE "OCORRENCIA NAO ENCONTRADA" TO MSG
               DISPLAY TELA1-MENSAGEM
               ACCEPT PRESS-ENTER
               CALL "PGM2"
           END-IF
           
           PERFORM WITH TEST AFTER UNTIL DESCRICAO NOT EQUALS SPACES
               ACCEPT NEW-DESC
               MOVE DESCRICAO TO O-DESCRICAO
           END-PERFORM
           
           PERFORM WITH TEST AFTER UNTIL STAT = 0 OR 1
               ACCEPT NEW-STAT
               MOVE STAT TO O-STAT
           END-PERFORM
           
           PERFORM WITH TEST AFTER UNTIL ANDAMENTO = 0 OR 1 OR 2 OR 3
               ACCEPT NEW-ANDAMENTO
               MOVE ANDAMENTO TO O-ANDAMENTO
           END-PERFORM
           
            OPEN I-O ARQ-OCORR
               REWRITE REG-OCORRENCIA
           CLOSE ARQ-OCORR
           
           MOVE "OCORRENCIA SALVA" TO MSG.
           DISPLAY TELA1-MENSAGEM
           ACCEPT PRESS-ENTER
           
           CALL "PGM2"

       STOP RUN.
       EXIT PROGRAM.
