identification division.
       program-id. GERAL_CLIENTES as "PGM311".

       environment division.
       configuration section.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       input-output section.
       file-control.
           select arq-cli assign to "D:\CLIENTES.DAT"
               file status is st-cli.
               
           select rel-cli assign to "D:\CLIENTES.PRN".
       data division.
           File section.
           fd arq-cli.
       01 reg-cli.
           05 cpf-c pic 9(11).
           05 nome-c pic x(30).
	   05 telefone-c.
		10 parentes-1 pic x value "(".
		10 parentes-2 pic x value ")".
		10 prefixo pic 99.
		10 telefone pic 9(8).
	   05 endereço-c pic x(45).
	   05 complemento-c pic x(10).
	   05 bairro-c pic x(20).
	   05 cidade-c pic x(20).
	   05 estado-c pic x(2).
	   05 cep-c pic 9(8). 
           05 data-o.
               10 dia-o pic 99/.
               10 mes-o pic 99/.
               10 ano-o pic 9999.
               
           05 tipo-pacotes pic x(10).
	   05 qtd-pontos-c pic 9.
	   05 data-v.
               10 dia-v pic 99/.
               10 mes-v pic 99/.
               10 ano-v pic 9999.
               
       fd rel-cli.
           01 reg-rel pic x(80).
       working-storage section.
       77 st-cli pic xx value spaces.
       77 wlin pic 99 value 55.
       01 DATA-SIS.
           05 ano pic 9999.
           05 mes pic 99.
           05 dia pic 99.
       01 CAB1.
           05 filler pic x(6) value "TvMack".
           05 filler pic x(21) value spaces.
           05 filler pic x(27) value "Relacao Geral de Clientes".
           05 filler pic x(20) value spaces.
	   05 filler pic x(3)B(2) value "PAG".
	   05 npag pic 9.

       01 CAB2.
           05 filler pic x(26) value spaces.
           05 filler pic x(17) value "Data de Emissao: ".
           05 data-cab2.
               10 dia-cab2 pic 99/.
               10 mes-cab2 pic 99/.
               10 ano-cab2 pic 9999.
           05 filler pic x(27) value spaces.
       01 lin1.
           05 cpf pic x(11).
           05 filler pic x(7) value spaces.
           05 nome pic x(30).
           05 filler pic x(6) value spaces.
	       05 telefone-fixo pic x(12).
       01 lin2.
	   05 endereço pic x(45)B(5).
	   05 complemento pic x(10)B(20).
       01 lin3.
           05 bairro pic x(20)B(10).
	   05 cidade pic x(10)B(2).
 	   05 estado pic x(2)B(2).
	   05 cep pic x(9).
       01 lin4.
           05 data-ocor pic x(11)B(19).
	   05 tipo-pac pic x(10)B(13).
	   05 qtd-pontos pic 9B(2).
	   05 data-vencimento pic x(11).
       procedure division.

       
       
       mestre.
           perform inicio
           perform proc until st-cli="10"
           perform fim.
               stop run.

	inicio.

       perform rot-data
           open output rel-cli
               input arq-cli
           perform ler.
       proc.
           move cpf-c to cpf
           move nome-c to nome
           move telefone-c to telefone-fixo
           move endereço-c to endereço
           move complemento-c to complemento
           move bairro-c to bairro
           move cidade-c to cidade
           move estado-c to estado
           move cep-c to cep
           
           move data-o to data-ocor
           move tipo-pacotes to tipo-pac
           move qtd-pontos-c to qtd-pontos
           move data-v to data-vencimento
           
           if wlin = 55
               perform cabec
           else 
           write reg-rel from lin1 after 1
           write reg-rel from lin2 after 1
           write reg-rel from lin3 after 1
           write reg-rel from lin4 after 1
           add 4 to wlin
           
           perform ler.
           
       cabec.
           add 1 to npag
           write reg-rel from cab1 after page
           write reg-rel from cab2 after 2
           write reg-rel from lin1 after 1
           write reg-rel from lin2 after 1
           write reg-rel from lin3 after 1
           write reg-rel from lin4 after 1
           move 8 to wlin.
       fim.
           close rel-cli
           arq-cli.
       ler.
           read arq-cli.
       rot-data.
           accept data-sis from date
           move dia to dia-cab2
           move mes to mes-cab2
           move ano to ano-cab2