       select vbm-tuto assign to
           "vbm-tuto.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is tuto-chave-tutor
           alternate record key is tuto-chave-tutor-1
           with duplicates
           alternate record key is tuto-chave-ext-1 = tuto-cpf-tutor
           with duplicates
           alternate record key is tuto-chave-ext-2 = tuto-nome-tutor
           with duplicates
           file status is fs-vbm-tuto.