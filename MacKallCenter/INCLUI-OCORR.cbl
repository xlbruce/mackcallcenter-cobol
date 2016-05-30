       PROGRAM-ID INCLUI_OCORR AS "PGM21".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO "D:\CLIENTES.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CPF
               FILE STATUS IS ST-CLIENTE.

           SELECT ARQ-OCORR ASSIGN TO "D:\OCORRENCIAS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS O-CPF
               FILE STATUS IS ST-OCORR.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTE.
       01 REG-CLIENTE.
          05 CPF PIC 9(11).
          05 NOME PIC X(30).
          05 ENDERECO PIC X(45).
          05 COMPLEMENTO PIC X(10).
          05 BAIRRO PIC X(20).
          05 CIDADE PIC X(20).
          05 ESTADO PIC X(2).
          05 CEP PIC X(9).
          05 DDD PIC 9(2).
          05 TELEFONE PIC X(9).
          05 DATA-INCLUSAO PIC X(10).
          05 TIPO-PACOTE PIC X(1).
          05 QTDE-PONTOS-RESIDENCIA PIC 9(1).
          05 VENCIMENTO-FATURA PIC 9(2).
          05 DATA-INCLUSAO-CLIENTE PIC X(10).
          05 DATA-ULTIMA-ALTERACAO PIC X(10).
       
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
       01 WS-CLIENTE.
          05 C-CPF PIC 9(11).
          05 C-NOME PIC X(30).
          05 C-ENDERECO PIC X(45).
          05 C-COMPLEMENTO PIC X(10).
          05 C-BAIRRO PIC X(20).
          05 C-CIDADE PIC X(20).
          05 C-ESTADO PIC X(2).
          05 C-CEP PIC X(9).
          05 C-DDD PIC 9(2).
          05 C-TELEFONE PIC X(9).
          05 C-DATA-INCLUSAO PIC X(10).
          05 C-TIPO-PACOTE PIC X(1).
          05 C-QTDE-PONTOS-RESIDENCIA PIC 9(1).
          05 C-VENCIMENTO-FATURA PIC 9(2).
          05 C-DATA-INCLUSAO-CLIENTE PIC X(10).
          05 C-DATA-ULTIMA-ALTERACAO PIC X(10).
       
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
           
       01 WS-TIME.
           05 HH PIC 99.
           05 MM PIC 99.
           
       01 WS-HH-MM.
           05 HH PIC 9(2).
           05 FILLER PIC X VALUE ':'.
           05 MM PIC 9(2).
           
       01 LINHA.
           05 FILLER VALUE "   ".
           05 L PIC X(72) VALUE ALL "Í".
           05 FILLER VALUE "    ".
       
       77 FOUND-CPF PIC X.
       77 CONFIRM PIC X.
       77 MSG PIC X(40).
       
       77 ST-CLIENTE PIC XX VALUES SPACES.
       77 ST-OCORR PIC XX VALUE SPACES.
       
       SCREEN SECTION.
       01 TELA1.
           05 BLANK SCREEN.
           05 LINE 01 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 03 COLUMN 35 VALUE "MACKALLCENTER".
           05 LINE 05 COLUMN 04 VALUE "INCLUSAO DE OCORRENCIA".
           05 LINE 05 COLUMN 66 USING WS-DD-MM-YYYY.
           05 LINE 07 COLUMN 01 PIC X(80) FROM LINHA.
           
           05 LINE 09 COLUMN 04 VALUE "CPF DO CLIENTE.: [           ]".
           
           05 LINE 10 COLUMN 04 VALUE "NOME DO CLIENTE: [".
           05 LINE 10 COLUMN 52 VALUE "]".
           
           05 LINE 11 COLUMN 04 VALUE "ENDEREÇO.......: [".
           05 LINE 11 COLUMN 52 VALUE "]".
           
           05 LINE 12 COLUMN 04 VALUE "COMPLEMENTO....: [          ]".
           
           05 LINE 13 COLUMN 04 VALUE "BAIRRO.........: [".
           05 LINE 13 COLUMN 42 VALUE "]".
           
           05 LINE 14 COLUMN 04 VALUE "CIDADE.........: [".
           05 LINE 14 COLUMN 42 VALUE "]".
           
           05 LINE 15 COLUMN 04 VALUE "ESTADO.........: [  ]".
           
           05 LINE 16 COLUMN 04 VALUE "CEP............: [     -   ]".
           
           05 LINE 17 COLUMN 04 VALUE "TELEFONE.......: [  ]-[    -  ]".
           
           05 LINE 18 COLUMN 04 VALUE "TIPO PACOTE....: [ ]".
           
           05 LINE 19 COLUMN 04 VALUE "QTDE PONTOS....: [ ]".
           
           05 LINE 20 COLUMN 04 VALUE "DIA VENCIMENTO.: [  ]".
           
           05 LINE 21 COLUMN 04 VALUE "DATA INCLUSAO..: [ / /   ]".
           
           05 LINE 22 COLUMN 04 VALUE "DATA ULT ALTERACAO [  /  /    ]".
           
           05 LINE 24 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 25 COLUMN 05 VALUE "MENSAGENS: ".
           05 LINE 26 COLUMN 01 PIC X(80) FROM LINHA.
           
       01 TELA1-VALUES.
           05 LINE 10 COLUMN 22 USING C-NOME.
           05 LINE 11 COLUMN 22 USING C-ENDERECO.
           05 LINE 12 COLUMN 22 USING C-COMPLEMENTO.
           05 LINE 13 COLUMN 22 USING C-BAIRRO.
           05 LINE 14 COLUMN 22 USING C-CIDADE.
           05 LINE 15 COLUMN 22 USING C-ESTADO.
           05 LINE 16 COLUMN 22 USING C-CEP.
           05 LINE 17 COLUMN 22 USING C-TELEFONE.                       
           05 LINE 18 COLUMN 22 USING C-TIPO-PACOTE.
           05 LINE 19 COLUMN 22 USING C-QTDE-PONTOS-RESIDENCIA.
           05 LINE 20 COLUMN 22 USING C-VENCIMENTO-FATURA.
           05 LINE 21 COLUMN 22 USING C-DATA-INCLUSAO-CLIENTE.
           05 LINE 22 COLUMN 22 USING C-DATA-ULTIMA-ALTERACAO.
           
       01 TELA1-MENSAGEM.
           05 LINE 25 COLUMN 16 USING MSG.
           05 PRESS-ENTER LINE 25 COLUMN 80 TO CONFIRM.
       
       01 TELA2.
           05 BLANK SCREEN.
           05 LINE 01 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 03 COLUMN 35 VALUE "TVMACK".
           05 LINE 05 COLUMN 04 VALUE "INCLUSAO DE OCORRENCIA".
           05 LINE 05 COLUMN 66 USING WS-DD-MM-YYYY.
           05 LINE 07 COLUMN 01 PIC X(80) FROM LINHA.
           
           05 LINE 09 COLUMN 04 VALUE "CPF DO CLIENTE.: [           ]".
           05 LINE 09 COLUMN 22 PIC X(11) USING CPF.
           
           05 LINE 10 COLUMN 04 VALUE "NOME DO CLIENTE: [".
           05 LINE 10 COLUMN 52 VALUE "]".
           05 LINE 10 COLUMN 22 USING NOME.
           
           05 LINE 11 COLUMN 04 VALUE "DATA OCORRENCIA: [".
           05 LINE 11 COLUMN 22 USING WS-DD-MM-YYYY.
           05 LINE 11 COLUMN 32 VALUE "]".
           
           05 LINE 12 COLUMN 04 VALUE "HORA OCORRENCIA: [".
           05 LINE 12 COLUMN 22 USING WS-HH-MM.
           05 LINE 12 COLUMN 27 VALUE "]".
               
           05 LINE 13 COLUMN 04 VALUE "ATENDENTE......: [".
           05 LINE 13 COLUMN 52 VALUE "]".
           05 S-ATENDENTE LINE 13 COLUMN 22 PIC X(30) TO O-ATENDENTE.
           
           05 LINE 15 COLUMN 04 VALUE "DESCRICAO DA OCORRENCIA: ".
           05 S-DESCRICAO LINE 16 COLUMN 04 PIC X(144) TO O-DESCRICAO.
           
           05 LINE 19 COLUMN 04 VALUE "STATUS...: [ ]".
           05 S-STAT LINE 19 COLUMN 16 PIC X TO O-STAT.
           
           05 LINE 19 COLUMN 40 VALUE "ANDAMENTO: [ ]".
           05 S-ANDAMENTO LINE 19 COLUMN 52 PIC X TO O-ANDAMENTO.
           
           05 LINE 21 COLUMN 01 PIC X(80) FROM LINHA.
           05 LINE 22 COLUMN 05 VALUE "MENSAGENS: ".
           05 LINE 23 COLUMN 01 PIC X(80) FROM LINHA.
           
       01 TELA2-MENSAGEM.
           05 LINE 22 COLUMN 16 USING MSG.
           05 PRESS-ENTER-2 LINE 17 COLUMN 80 TO CONFIRM.
           
       PROCEDURE DIVISION.
       INICIO.
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           MOVE CORRESPONDING WS-DATE TO WS-DD-MM-YYYY.
           
           DISPLAY TELA1.
           
           ACCEPT CPF WITH LENGTH-CHECK FULL AT LINE 09 COLUMN 22.
           
           OPEN I-O ARQ-CLIENTE
               READ ARQ-CLIENTE RECORD INTO WS-CLIENTE
                   KEY IS CPF
                   INVALID KEY MOVE "N" TO FOUND-CPF
                   NOT INVALID KEY MOVE "S" TO FOUND-CPF
               END-READ
           CLOSE ARQ-CLIENTE
           
           IF FOUND-CPF = "S"
               DISPLAY TELA1-VALUES
               MOVE "TECLE ENTER PARA INSERIR OCORRENCIA" TO MSG
               DISPLAY TELA1-MENSAGEM
               ACCEPT PRESS-ENTER
           ELSE
               MOVE "CLIENTE NAO ENCONTRADO." TO MSG
               DISPLAY TELA1-MENSAGEM
               ACCEPT PRESS-ENTER
               CALL "PGM2"
           END-IF
           
           ACCEPT WS-TIME FROM TIME
           MOVE CORRESPONDING WS-TIME TO WS-HH-MM.
           
           DISPLAY TELA2.
           
           MOVE CPF TO O-CPF
           MOVE NOME TO O-NOME
           MOVE WS-DATE TO O-DATA-OCORR
           MOVE WS-TIME TO O-HORA-OCORR
           
           PERFORM WITH TEST AFTER UNTIL O-ATENDENTE NOT EQUAL SPACES
               ACCEPT S-ATENDENTE
           END-PERFORM
           
           PERFORM WITH TEST AFTER UNTIL O-DESCRICAO NOT EQUAL SPACES
               ACCEPT S-DESCRICAO
           END-PERFORM
           
           PERFORM WITH TEST AFTER UNTIL O-STAT = 0 OR 1
               ACCEPT S-STAT
           END-PERFORM
           
           PERFORM WITH TEST AFTER UNTIL O-ANDAMENTO = 0 OR 1 OR 2 OR 3
               ACCEPT S-ANDAMENTO
           END-PERFORM
           
           OPEN I-O ARQ-OCORR
               WRITE REG-OCORRENCIA
           CLOSE ARQ-OCORR
           
           MOVE "OCORRENCIA SALVA" TO MSG.
           DISPLAY TELA2-MENSAGEM
           ACCEPT PRESS-ENTER-2
           
           CALL "PGM2"
           

       STOP RUN.
       EXIT PROGRAM.