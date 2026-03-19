       select vbm-ttrs assign to
           "vbm-ttrs.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is ttrs-chave
           alternate record key is ttrs-chave-1
           with duplicates
           alternate record key is ttrs-chave-ext-1 = ttrs-cpf
           with duplicates
           alternate record key is ttrs-chave-ext-2 = ttrs-nome
           with duplicates
           file status is fs-vbm-ttrs.