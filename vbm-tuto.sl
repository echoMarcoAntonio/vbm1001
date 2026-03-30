       select vbm-tuto assign to
           "vbm-tuto.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is tuto-chave-tutor
           alternate record key is tuto-chave-tutor-ext-1 =
                                   tuto-nm-tutor
           with duplicates
           file status is fs-vbm-tuto.