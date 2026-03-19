       identification                division.
       program-id.                   vbm-1001.
       author.                       stanicki.
       date-written.                 mar-2026.
      ******************************************************************
      *                      cadastro de tutores                       *
      ******************************************************************
       environment                          division.
       special-names.
           decimal-point is comma.
       input-output                         section.
       file-control.
           copy "vbm-ttrs.sl".
       data                                 division.
       file                                 section.
           copy "vbm-ttrs.fd".
      ******************************************************************
      *                        working storage                         *
      ******************************************************************
       working-storage                      section.
       copy "Acugui.def".
       copy "Acucobol.def".
      *
       77 s01-keystatus                     pic 9(04)
                                            special-names crt status.
          88 f2-key                    value 0002.
          88 esc-key                   value 0027.
          88 novo-key                       value 1001.
          88 editar-key                     value 1002.
          88 excluir-key                    value 1003.
          88 primeiro-key                   value 1011.
          88 anterior-key                   value 1012.
          88 proximo-key                    value 1013.
          88 ultimo-key                     value 1014.
          88 salvar-key                     value 0221.
          88 cancelar-key                   value 1005.
      *
       77 fs-vbm-ttrs pic xx.
          88 valid-vbm-ttrs                 value "00" thru "09".
      *
       01 screen-t01.
          03 s01-cd-tutor                   pic 9(07).
          03 s01-nm-tutor                   pic x(30).
          03 s01-fone-mask                  pic x(19).
          03 s01-fone-tutor.
             05 s01-ddd                     pic 9(02).
             05 s01-prefixo                 pic 9(05).
             05 s01-sufixo                  pic 9(04).
          03 s01-endereco-tutor.
             05 s01-rua-tutor               pic x(30).
             05 s01-numero-tutor            pic 9(12).
             05 s01-bairro-tutor            pic x(30).
             05 s01-cidade-tutor            pic x(30).
             05 s01-cep-tutor               pic 9(08).
             05 s01-uf-tutor                pic x(02).
          03 s01-qtd-pets-tutor             pic 9(02).
      *
       01 s01-menu-opcao.
          03 s01-opcao                      pic 9(02).
      *
       01 wss-controle                     pic 9(01).
          88 wss-inclusao               value 1.
          88 wss-alteracao              value 2.
      *
      ******************************************************************
      *                         screen section                         *
      ******************************************************************
       screen                               section.
       copy "vbm-tela-ttrs.scr".

      ******************************************************************
      *                      procedure division                        *
      ******************************************************************
       procedure                            division.
           perform abrir-arquivos.
           perform inicio-programa.
           perform fechar-arquivos.
           perform fim-programa.
      *
       abrir-arquivos.
           open i-o vbm-ttrs.
           if not valid-vbm-ttrs
               display message box "erro ao abrir o arquivo vbm-ttrs!"
                                   "status: " fs-vbm-ttrs
               go to fim-programa
           end-if.
      *
       fechar-arquivos.
           close vbm-ttrs.
      *
       inicio-programa.
           display standard graphical window
               lines 30
               size 102
               title "cadastro de tutores".
           display tela-menu.
           accept s01-opcao.
            perform with test after until esc-key
                accept tela-menu on exception
                    perform modificar-componentes
                end-accept
            end-perform.
      *
       fim-programa.
           exit program.
           stop run.
      *
       modificar-componentes.
           modify t01-cd-tutor       value s01-cd-tutor.
           modify t01-fone-tutor     value s01-fone-tutor.
           modify t01-qtd-pets-tutor value s01-qtd-pets-tutor.
      *    modify t01-endereco-tutor value s01-endereco-tutor.
           modify t01-cidade-tutor   value s01-cidade-tutor.
           modify t01-uf-tutor       value s01-uf-tutor.
           modify t01-cep-tutor      value s01-cep-tutor.
           modify t01-rua-tutor      value s01-rua-tutor.
           modify t01-numero-tutor   value s01-numero-tutor.
      *
       limpa-tela.
           initialize screen-t01.
           perform modificar-componentes.
      * move all "-" to wss-linha-topo