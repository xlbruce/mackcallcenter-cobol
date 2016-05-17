       IDENTIFICATION DIVISION.
       program-id. Validador_CPF as "Validador_CPF".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 digitos.
           05 d1 pic 9 values zero.
           05 d2 pic 9 values zero.
          
       
       01 soma pic 9(3).
       01 peso pic 9(2).
       01 resto pic 9(2).
       77 i pic 99 values zero.
       77 lixo pic 9(3).
       
       linkage section.
       01 retorno pic 9.
       01 cpf.
           05 dig pic 9 occurs 11.
           
       procedure division using retorno,cpf.
       
       INICIO.

           INITIALIZE soma
           move 10 to peso
           
           perform varying i from 1 by 1 until i>9
               compute soma = soma + (peso * dig(i))
               compute peso = peso - 1
           end-perform

           divide soma by 11 giving lixo remainder resto
           
           compute d1 = 11 - resto
           
           if d1 > 9 then
               move 0 to d1
           end-if

           move 0 to soma
           move 11 to peso
           
           perform varying i from 1 by 1 until i>10
               compute soma = soma + (peso * dig(i))
               compute peso = peso - 1
           end-perform
           
           divide soma by 11 giving lixo remainder resto
           
           compute d2 = 11 - resto
           
           if d2 > 9 then
               move 0 to d2
           end-if
           
           if d1 = dig(10) and d2 = dig(11) then
               move 1 to retorno
           else
               move 0 to retorno
           end-if
           
           exit program.