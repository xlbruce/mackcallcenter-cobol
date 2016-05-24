      *INSERIR CLIENTE
       program-id. PGM11 as "PGM11".

       environment division.
       configuration section.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTES ASSIGN TO "C:\TEMP\CLIENTES.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY CPF 
               FILE STATUS IS ST-CLIENTE.

       data division.
       FILE SECTION.
       FD ARQ-CLIENTES.
       01 REG-CLIENTE.
          05 CPF PIC X(11).
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
       
       working-storage section.
       01 SYS-DATE.
           05 ANO PIC 9999.
           05 MES PIC 99.
           05 DIA PIC 99.
       77 ST-CLIENTE PIC XX VALUE SPACES.
       77 RESP PIC X VALUE SPACE.
       01 LINHA.
           05 FILLER VALUE "   ".
           05 L PIC X(72) VALUE ALL "Í".
           05 FILLER VALUE "    ".
           
       01 W-DATE.
           05 DIA PIC 99.
           05 MES PIC 99.
           05 ANO PIC 9999.
       01 CLIENTE-AUX.
           05 CEP-CLI-AUX.
               10 CEP1 PIC 9(5).
               10 CEP2 PIC 999.
           05 TELEFONE-CLI-AUX.
               10 TEL1 PIC 99.
               10 TEL2 PIC 9(8).
           05 TIPO.
               10 TIPO1 PIC X.
               10 TIPO2 PIC X(10).
       
       SCREEN SECTION.
       01  TELA.
           05 BLANK SCREEN.
           05 LINE 01 COLUMN 01 PIC X(80)
           FROM LINHA.
           05 LINE 03 COLUMN 35 VALUE "TVMACK".
           05 LINE 05 COLUMN 04 VALUE "INCLUSAO DE CLIENTE".
           05 LINE 05 COLUMN 66 PIC 99/99/9999
           FROM W-DATE.
           05 LINE 07 COLUMN 01 PIC X(80)
           FROM LINHA.
           
           05 LINE 09 COLUMN 04
           VALUE "CPF DO CLIENTE.: [           ]".
           05 S-CPF LINE 09 COLUMN 22 PIC X(11) TO CPF.
           
           05 LINE 10 COLUMN 04
           VALUE "NOME DO CLIENTE: [".
           05 LINE 10 COLUMN 34 VALUE "]".
           05 S-NOME LINE 10 COLUMN 22 PIC X(30) TO NOME.
           
           05 LINE 11 COLUMN 04
           VALUE "ENDERECO.......: [".
           05 LINE 11 COLUMN 49 VALUE "]".
           05 S-ENDERECO LINE 11 COLUMN 22 PIC X(45) TO ENDERECO.
           
           05 LINE 12 COLUMN 04
           VALUE "COMPLEMENTO....: [          ]".
           05 S-COMPLEMENTO LINE 12 COLUMN 22 PIC X(10) TO COMPLEMENTO.
           
           05 LINE 13 COLUMN 04
           VALUE "BAIRRO.........: [".
           05 LINE 13 COLUMN 42 VALUE "]".
           05 S-BAIRRO LINE 13 COLUMN 22 PIC X(20) TO BAIRRO.
           
           05 LINE 14 COLUMN 04
           VALUE "CIDADE.........: [".
           05 LINE 14 COLUMN 42 VALUE "]".
           05 S-CIDADE LINE 14 COLUMN 22 PIC X(20) TO CIDADE.
           
           05 LINE 15 COLUMN 04
           VALUE "ESTADO.........: [  ]".
           05 S-ESTADO LINE 15 COLUMN 22 PIC XX TO ESTADO.
           
           05 LINE 16 COLUMN 04
           VALUE "CEP............: [     -   ]".
           05 S-CEP LINE 16 COLUMN 22  PIC X(8) TO CEP.
           
           05 LINE 17 COLUMN 04
           VALUE "TELEFONE.......: [  ]-[    -    ]".
           05 S-TELEFONE LINE 17 COLUMN 22 PIC X(10) TO TELEFONE.
           
           05 LINE 18 COLUMN 04
           VALUE "TIPO PACOTE....: [ ]".
           05 S-TIPO LINE 18 COLUMN 22 PIC X TO TIPO-PACOTE.
           
           05 LINE 19 COLUMN 04
           VALUE "QTDE PONTOS....: [ ]".
           05 S-PONTOS LINE 19 COLUMN 22 PIC X TO 
           QTDE-PONTOS-RESIDENCIA.
           
           05 LINE 20 COLUMN 04
           VALUE "DIA VENCIMENTO.: [  ]".
           05 S-VENCIMENTO LINE 20 COLUMN 22 PIC XX TO 
           VENCIMENTO-FATURA.
           
           05 LINE 21 COLUMN 04
           VALUE "DATA INCLUSAO..: [ / /   ]".
           05 S-DT-INCLUSAO.
               10 S-DT-DIA LINE 21 COLUMN 22 PIC 99 .
               10 S-DT-MES LINE 21 COLUMN 24 PIC 99.
               10 S-DT-ANO LINE 21 COLUMN 26 PIC 9999.
           
           05 LINE 22 COLUMN 04
           VALUE "DATA ULT ALTERACAO [  /  /    ]".
           05 S-UL-INCLUSAO.
               10 S-UL-DIA LINE 21 COLUMN 22 PIC 99 .
               10 S-UL-MES LINE 21 COLUMN 24 PIC 99.
               10 S-UL-ANO LINE 21 COLUMN 26 PIC 9999.
           05 LINE 24 COLUMN 01 PIC X(80)
           FROM LINHA.
           05 LINE 25 COLUMN 05
           VALUE "MENSAGENS: ".
            05 s-Sucesso line 25 column 15 value " ".
           05 LINE 26 COLUMN 01 PIC X(80)
           FROM LINHA.
           
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY TELA.    
           accept S-CPF
           ACCEPT S-NOME
           ACCEPT S-ENDERECO
           ACCEPT S-COMPLEMENTO
           ACCEPT S-BAIRRO
           ACCEPT S-CIDADE
           ACCEPT S-ESTADO
           ACCEPT S-CEP
           ACCEPT S-TELEFONE
           ACCEPT S-TIPO
           ACCEPT S-PONTOS
           ACCEPT S-VENCIMENTO
            OPEN I-O ARQ-CLIENTES
               READ ARQ-CLIENTES
           
           
           
               IF ST-CLIENTE = "23"
                   WRITE REG-CLIENTE
                   DISPLAY S-SUCESSO
               ELSE 
                   
               END-IF
               
           
           
           
           STOP RUN.
      *TODO - FINISH SCREEN
       EXIT program 
      