       program-id. Menu_Principal as "Menu_Principal".

       environment division.
       configuration section.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TMP-DATA.
           05 ANO-TMP PIC 9(4).
           05 MES-TMP PIC 99.
           05 DIA-TMP PIC 99.
       01 W-DATA.
           05 DIA PIC XX.
           05 FILLER VALUE "/".
           05 MES PIC XX.
           05 FILLER VALUE "/".
           05 ANO PIC XXXX.
       77 OPC PIC 9 VALUE ZERO.
           88 OPC-OK VALUES 1 THRU 4.
       77 W-BRANCO PIC X(39).
       SCREEN SECTION.
       01  TELA.
           05  LINE 01  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  LINE 03  COLUMN 01 
               VALUE  "    TVMACK           MENU PRINCIPAL".
           05  LINE 05  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 05  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  LINE 09  COLUMN 01 
               VALUE  "                        1. CLIENTES".
           05  LINE 10  COLUMN 01 
               VALUE  "                        2. OCORRENCIAS".
           05  LINE 11  COLUMN 01 
               VALUE  "                        3. CONSULTAS".
           05  LINE 12  COLUMN 01 
               VALUE  "                        4. ENCERRA".
           05  LINE 16  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 16  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  LINE 18  COLUMN 01 
               VALUE  "    MENSAGENS: INFORME SUA OPCAO [ ]".
           05  LINE 20  COLUMN 01 
               VALUE  "    ออออออออออออออออออออออออออออออออออออ".
           05  LINE 20  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออ".
           05  S-DATA
               LINE 03  COLUMN 66  PIC XX(10)
               FROM  W-DATA.
           05  S-OPC
               LINE 18  COLUMN 35  PIC 9
               TO     OPC.
           
       01 MENSAGENS.
           05 MSG-BRANCO 
           LINE 18 COLUMN 38 PIC X(39) FROM W-BRANCO.
           05 MSG-OPC-INVALIDA
           LINE 18 COLUMN 38 VALUE "OPCAO INVALIDA".
       
             
       PROCEDURE DIVISION.
       INICIO.
           PERFORM ROT-DATA.
           DISPLAY TELA
           
           PERFORM WITH TEST AFTER UNTIL OPC EQUAL 4
           ACCEPT S-OPC
           IF NOT OPC-OK THEN
           DISPLAY MSG-OPC-INVALIDA
           STOP " "
           DISPLAY MSG-BRANCO
           ELSE
           EVALUATE OPC
           WHEN 1 CALL "PGM1"
           WHEN 2 CALL "PGM2"
           WHEN 3 CALL "PGM3"
           END-EVALUATE
           END-IF
           END-PERFORM

           
           
           STOP RUN.
           
       ROT-DATA.
           ACCEPT TMP-DATA FROM DATE YYYYMMDD.
           MOVE DIA-TMP TO DIA.
           MOVE MES-TMP TO MES.
           MOVE ANO-TMP TO ANO.