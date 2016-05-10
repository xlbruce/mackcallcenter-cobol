       program-id. Menu_Principal as "Menu_Principal".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 valido pic 9.
       01 cpf pic 9(11) value 12312312312.
       procedure division.

       inicio.
       
           call "VALIDA-CPF" using valido, cpf
           
           display valido at 1010
           stop run.