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
       77 ST-CLIENTE PIC XX VALUE SPACES.
       77 RESP PIC X VALUE SPACE.
       01 W-DATE.
           05 DIA PIC 99.
           05 MES PIC 99.
           05 ANO PIC 9999.
       01 CLIENTE-AUX.
           05 CEP.
               10 CEP1 PIC 9(5).
               10 CEP2 PIC 999.
           05 TELEFONE.
               10 TEL1 PIC 99.
               10 TEL2 PIC 9(8).
           05 TIPO.
               10 TIPO1 PIC X.
               10 TIPO2 PIC X(10).
       
       SCREEN SECTION.
       01  TELA.
           05  LINE 01  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  LINE 03  COLUMN 01 
               VALUE  "                                TVMACK".
           05  LINE 05  COLUMN 01 
               VALUE  "    INCLUSAO DE CLIENTE".
           05  LINE 07  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 07  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  LINE 09  COLUMN 01 
               VALUE  "    CPF DO CLIENTE.: [           ]".
           05  LINE 10  COLUMN 01 
               VALUE  "    NOME DO CLIENTE: [".
           05  LINE 10  COLUMN 41 
               VALUE  "            ]".
           05  LINE 11  COLUMN 01 
               VALUE  "    ENDERECO.......: [".
           05  LINE 11  COLUMN 41 
               VALUE  "                           ]".
           05  LINE 12  COLUMN 01 
               VALUE  "    COMPLEMENTO....: [          ]".
           05  LINE 13  COLUMN 01 
               VALUE  "    BAIRRO.........: [".
           05  LINE 13  COLUMN 41 
               VALUE  "  ]".
           05  LINE 14  COLUMN 01 
               VALUE  "    CIDADE.........: [".
           05  LINE 14  COLUMN 41 
               VALUE  "  ]".
           05  LINE 15  COLUMN 01 
               VALUE  "    ESTADO.........: [  ]".
           05  LINE 16  COLUMN 01 
               VALUE  "    CEP............: [     -   ]".
           05  LINE 17  COLUMN 01 
               VALUE  "    TELEFONE.......: [  ]-[    -     ]".
           05  LINE 18  COLUMN 01 
               VALUE  "    TIPO PACOTE....: [ ]-[           ]".
           05  LINE 19  COLUMN 01 
               VALUE  "    QTDE PONTOS....: [ ]".
           05  LINE 20  COLUMN 01 
               VALUE  "    DIA VENCIMENTO.: [  ]".
           05  LINE 21  COLUMN 01 
               VALUE  "    DATA INCLUSAO..: [          ]".
           05  LINE 22  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 22  COLUMN 41 
               VALUE  "ออออออออออออออออออออออออออออออออออออ".
           05  LINE 23  COLUMN 01 
               VALUE  "    MENSAGENS:".
           05  LINE 24  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41 
               VALUE  "ออออออออออออออออออออออออออออออออออออ".
           05  S-DATA
               LINE 05  COLUMN 66  PIC 99/99/9999
               FROM   W-DATE.
           05  S-CPF
               LINE 09  COLUMN 23  PIC X(11)
               TO     CPF.
           05  S-NOME
               LINE 10  COLUMN 23  PIC X(30)
               TO     NOME.
           05  S-ENDERECO
               LINE 11  COLUMN 23  PIC X(45)
               TO     ENDERECO.
           05  S-COMPLEMENTO
               LINE 12  COLUMN 23  PIC X(10)
               TO     COMPLEMENTO.
           05  S-BAIRRO
               LINE 13  COLUMN 23  PIC X(20)
               TO     BAIRRO.
           05  S-CIDADE
               LINE 14  COLUMN 23  PIC X(20)
               TO     CIDADE.
           05  S-ESTADO
               LINE 15  COLUMN 23  PIC X(02)
               TO     ESTADO.
           05  S-CEP1
               LINE 16  COLUMN 23  PIC X(05)
               TO     CEP1.
           05  S-CEP02
               LINE 16  COLUMN 29  PIC X(03)
               TO     CEP2.
           05  S-DDD
               LINE 17  COLUMN 23  PIC 9(02)
               TO     DDD.
           05  S-TEL1
               LINE 17  COLUMN 28  PIC X(04)
               TO     TEL1.
           05  S-TEL2
               LINE 17  COLUMN 34  PIC X(04)
               TO     TEL2.
           05  S-TIPO1
               LINE 18  COLUMN 23  PIC X(01)
               TO     TIPO1.
           05  S-TIPO2
               LINE 18  COLUMN 27  PIC X(11)
               TO     TIPO2.
           05  S-PONTOS
               LINE 19  COLUMN 23  PIC 9
               TO     QTDE-PONTOS-RESIDENCIA.
           05  S-VENCIMENTO
               LINE 20  COLUMN 23  PIC 9(02)
               TO     VENCIMENTO-FATURA.
           05  S-DT-INCLUSAO
               LINE 21  COLUMN 23  PIC XX/XX/XXXX
               TO     DATA-INCLUSAO-CLIENTE.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY TELA.
       
       EXIT program 
      