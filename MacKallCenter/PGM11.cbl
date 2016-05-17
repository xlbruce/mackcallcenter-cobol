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
       
       LINKAGE SECTION.
       01 CPF-P PIC X(11).
       01 NOME-P PIC X(30).
       01 ENDERECO-P PIC X(45).
       01 COMPLEMENTO-P PIC X(10).
       01 BAIRRO-P PIC X(20).
       01 CIDADE-P PIC X(20).
       01 ESTADO-P PIC X(2).
       01 CEP-P PIC X(9).
       01 DDD-P PIC 9(2).
       01 TELEFONE-P PIC X(9).
       01 DATA-INCLUSAO-P PIC X(10).
       01 TIPO-PACOTE-P PIC X(1).
       01 QTDE-PONTOS-RESIDENCIA-P PIC 9(1).
       01 VENCIMENTO-FATURA-P PIC 9(2).
       01 DATA-INCLUSAO-CLIENTE-P PIC X(10).
       01 DATA-ULTIMA-ALTERACAO-P PIC X(10).
       01 RETORNO PIC 9.
       procedure division 
           USING 
           CPF-P,NOME-P,ENDERECO-P,COMPLEMENTO-P,BAIRRO-P,CIDADE-P,
           ESTADO-P,CEP-P,DDD-P,TELEFONE-P,DATA-INCLUSAO-P,TIPO-PACOTE-P
           QTDE-PONTOS-RESIDENCIA-P,VENCIMENTO-FATURA-P,                
           DATA-INCLUSAO-CLIENTE-P,DATA-ULTIMA-ALTERACAO-P,RETORNO.     
       INICIO.
           
           OPEN I-O ARQ-CLIENTES
           
           READ ARQ-CLIENTES
               MOVE CPF-P TO CPF
               
               IF ST-CLIENTE = "23"
                   MOVE NOME-P TO NOME
                   MOVE ENDERECO-P TO ENDERECO
                   MOVE COMPLEMENTO-P TO COMPLEMENTO
                   MOVE BAIRRO-P TO BAIRRO
                   MOVE CIDADE-P TO CIDADE
                   MOVE ESTADO-P TO ESTADO
                   MOVE CEP-P TO CEP
                   MOVE DDD-P TO DDD
                   MOVE TELEFONE-P TO TELEFONE
                   MOVE DATA-INCLUSAO-P TO DATA-INCLUSAO
                   MOVE TIPO-PACOTE-P TO TIPO-PACOTE
                   MOVE QTDE-PONTOS-RESIDENCIA-P TO 
                   QTDE-PONTOS-RESIDENCIA
                   MOVE VENCIMENTO-FATURA-P TO VENCIMENTO-FATURA
                   MOVE DATA-INCLUSAO-CLIENTE-P TO DATA-INCLUSAO-CLIENTE
                   MOVE DATA-ULTIMA-ALTERACAO-P TO DATA-ULTIMA-ALTERACAO
                   MOVE 1 TO RETORNO
                   WRITE REG-CLIENTE
               ELSE 
                   MOVE 0 TO RETORNO
               END-IF
               
           CLOSE ARQ-CLIENTES
       EXIT program 
      