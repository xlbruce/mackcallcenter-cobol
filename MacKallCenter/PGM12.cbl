      *ALTERA CLIENTE
       program-id. PGM12 as "PGM12".

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
       77 ST-CLIENTE PIC XX VALUE SPACES.
       77 RESP PIC X VALUE SPACE.
       01 W-DATE.
           05 DIA PIC 99.
           05 MES PIC 99.
           05 ANO PIC 9999.
       01 LINHA.
           05 FILLER VALUE "   ".
           05 L PIC X(72) VALUE ALL "Í".
           05 FILLER VALUE "    ".
           
       SCREEN SECTION.
       01  TELA.
           05 BLANK SCREEN.
           05 LINE 01 COLUMN 01 PIC X(80)
           FROM LINHA.
           05 LINE 03 COLUMN 35 VALUE "TVMACK".
           05 LINE 05 COLUMN 04 VALUE "ALTERACAO DE CLIENTE".
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
           
           05 LINE 11 COLUMN 04
           VALUE "ENDERECO.......: [".
           05 LINE 11 COLUMN 49 VALUE "]".
           
           05 LINE 12 COLUMN 04
           VALUE "COMPLEMENTO....: [          ]".
           
           05 LINE 13 COLUMN 04
           VALUE "BAIRRO.........: [".
           05 LINE 13 COLUMN 42 VALUE "]".
           
           05 LINE 14 COLUMN 04
           VALUE "CIDADE.........: [".
           05 LINE 14 COLUMN 42 VALUE "]".
           
           05 LINE 15 COLUMN 04
           VALUE "ESTADO.........: [  ]".
           
           05 LINE 16 COLUMN 04
           VALUE "CEP............: [     -   ]".
           
           05 LINE 17 COLUMN 04
           VALUE "TELEFONE.......: [  ]-[    -    ]".
           
           05 LINE 18 COLUMN 04
           VALUE "TIPO PACOTE....: [ ]-[          ]".
           
           05 LINE 19 COLUMN 04
           VALUE "QTDE PONTOS....: [ ]".
           
           05 LINE 20 COLUMN 04
           VALUE "DIA VENCIMENTO.: [  ]".
           
           05 LINE 21 COLUMN 04
           VALUE "DATA INCLUSAO..: [  /  /    ]".
           
           05 LINE 22 COLUMN 04
           VALUE "DATA ULT ALTERACAO [  /  /    ]".
           
           05 LINE 24 COLUMN 01 PIC X(80)
           FROM LINHA.
           05 LINE 25 COLUMN 05
           VALUE "MENSAGENS: ".
           05 LINE 26 COLUMN 01 PIC X(80)
           FROM LINHA.
       
       PROCEDURE DIVISION.   
       INICIO.
           DISPLAY TELA.
      *TODO ler o arquivo de clientes, e preencher a tela 
       EXIT program