       identification                       division.
       program-id.                          vbm-1001.
       author.                              stanicki.
       date-written.                        mar-2026.
      ******************************************************************
      *                      cadastro de tutores                       *
      ******************************************************************
       environment                          division.
       special-names.
           decimal-point is comma.
       input-output                         section.
       file-control.
           copy "vbm-tuto.sl".
       data                                 division.
       file                                 section.
           copy "vbm-tuto.fd".
      ******************************************************************
      *                        working storage                         *
      ******************************************************************
       working-storage                      section.
       copy "Acugui.def".
       copy "Acucobol.def".
      *
       77 s01-keystatus                     pic 9(04)
                                            special-names crt status.
          88 f2-key                         value 0002.
          88 esc-key                        value 0027.
          88 novo-key                       value 1001.
          88 editar-key                     value 1002.
          88 excluir-key                    value 1003.
          88 primeiro-key                   value 1011.
          88 anterior-key                   value 1012.
          88 proximo-key                    value 1013.
          88 ultimo-key                     value 1014.
          88 salvar-key                     value 1015.
          88 cancelar-key                   value 1016.
      *
       77 fs-vbm-tuto pic xx.
          88 valid-vbm-tuto                 value "00" thru "09".
      *
       01 screen-t01.
          03 s01-cd-tutor                   pic 9(07).
          03 s01-cpf-tutor                  pic 9(11).
          03 s01-nome-tutor                 pic x(30).
          03 s01-fone-tutor                 pic x(17) value spaces.
          03 s01-uf-tutor                   pic x(02).
          03 s01-cep-tutor                  pic 9(08).
          03 s01-cidade-tutor               pic x(30).
          03 s01-bairro-tutor               pic x(30).
          03 s01-rua-tutor                  pic x(30).
          03 s01-numero-tutor               pic 9(12).
          03 s01-qtd-pets-tutor             pic 9(02).
      *
       01 s01-menu-opcao.
          03 s01-opcao                      pic 9(02).
      *
       01 wss-controle                      pic 9(01).
          88 wss-inclusao                   value 1.
          88 wss-alteracao                  value 2.
          88 wss-pesquisa                   value 3.
      *
      ******************************************************************
      *                         screen section                         *
      ******************************************************************
       screen                               section.
       copy "tutores-tela-tuto.scr".
      *
      ******************************************************************
      *                      procedure division                        *
      ******************************************************************
       procedure                            division.
           perform abrir-arquivos.
           perform inicio-programa.
           perform fechar-arquivos.
      *
       abrir-arquivos.
           open i-o vbm-tuto.
           if not valid-vbm-tuto
              display message box "ERRO ao abrir o arquivo tutores!"
                                  " status: " fs-vbm-tuto
              exit paragraph
           end-if.
      *
       inicio-programa.
           set wss-pesquisa to true.
           display standard graphical window
              lines 30
              size 102
              title "VBM v0.0.1. Cadastro de Tutores".
           display tela-menu.
           perform with test after until esc-key
              accept tela-menu on exception
                 perform controle-componentes
              end-accept
           end-perform.
      *
       fechar-arquivos.
           close vbm-tuto.
           goback.
      *
       controle-componentes.
           evaluate true
              when f2-key
                 perform buscar-codigo
              when novo-key
                 perform novo
              when editar-key
                 perform editar
              when excluir-key
                 perform excluir
              when primeiro-key
                 perform primeiro
              when anterior-key
                 perform anterior
              when proximo-key
                 perform proximo
              when ultimo-key
                 perform ultimo
              when salvar-key
                 perform salvar
              when cancelar-key
                 perform cancelar
           end-evaluate.
      *
       modificar-componentes.
           modify t01-ef-cd-tutor           value s01-cd-tutor.
           modify t01-ef-fone-tutor         value s01-fone-tutor.
           modify t01-ef-qtd-pets-tutor     value s01-qtd-pets-tutor.
           modify t01-ef-cidade-tutor       value s01-cidade-tutor.
           modify t01-ef-uf-tutor           value s01-uf-tutor.
           modify t01-ef-cep-tutor          value s01-cep-tutor.
           modify t01-ef-rua-tutor          value s01-rua-tutor.
           modify t01-ef-numero-tutor       value s01-numero-tutor.
           modify t01-ef-qtd-pets-tutor     value s01-qtd-pets-tutor.
      *
       habilitar-componentes.
           modify t01-ef-cpf-tutor          enabled true.
           modify t01-ef-nome-tutor         enabled true.
           modify t01-ef-fone-tutor         enabled true.
           modify t01-ef-uf-tutor           enabled true.
           modify t01-ef-cep-tutor          enabled true.
           modify t01-ef-cidade-tutor       enabled true.
           modify t01-ef-bairro-tutor       enabled true.
           modify t01-ef-rua-tutor          enabled true.
           modify t01-ef-numero-tutor       enabled true.
           modify t01-ef-qtd-pets-tutor     enabled true.
      *
       desabilitar-componentes.
           modify t01-ef-cpf-tutor          enabled true.
           modify t01-ef-nome-tutor         enabled true.
           modify t01-ef-fone-tutor         enabled false.
           modify t01-ef-uf-tutor           enabled false.
           modify t01-ef-cep-tutor          enabled false.
           modify t01-ef-cidade-tutor       enabled false.
           modify t01-ef-bairro-tutor       enabled false.
           modify t01-ef-rua-tutor          enabled false.
           modify t01-ef-numero-tutor       enabled false.
           modify t01-ef-qtd-pets-tutor     enabled false.
      *
       habilitar-navegacao.
           modify t01-pb-novo               enabled true.
           modify t01-pb-editar             enabled true.
           modify t01-pb-excluir            enabled true.
           modify t01-pb-primeiro           enabled true.
           modify t01-pb-proximo            enabled true.
           modify t01-pb-anterior           enabled true.
           modify t01-pb-ultimo             enabled true.
      *
       desabilitar-navegacao.
           modify t01-pb-novo               enabled false.
           modify t01-pb-editar             enabled false.
           modify t01-pb-excluir            enabled false.
           modify t01-pb-primeiro           enabled false.
           modify t01-pb-proximo            enabled false.
           modify t01-pb-anterior           enabled false.
           modify t01-pb-ultimo             enabled false.
      *
       limpa-tela.
           initialize screen-t01.
           display tela-menu.
      *
       mover-registro-para-tela.
           initialize screen-t01.
           move tuto-cpf-tutor              to s01-cpf-tutor.
           move tuto-cd-tutor               to s01-cd-tutor.
           move tuto-nome-tutor             to s01-nome-tutor.
      *
           string "("                       delimited by size
                  tuto-ddd-tutor
                  ")"                       delimited by size
                  " "                       delimited by size
                  tuto-prefixo-tutor        delimited by size
                  "-"                       delimited by size
                  tuto-sufixo-tutor         delimited by size
                  into s01-fone-tutor
           end-string.
      *
           move tuto-uf-tutor               to s01-uf-tutor.
           move tuto-cep-tutor              to s01-cep-tutor.
           move tuto-cidade-tutor           to s01-cidade-tutor.
           move tuto-bairro-tutor           to s01-bairro-tutor.
           move tuto-rua-tutor              to s01-rua-tutor.
           move tuto-numero-tutor           to s01-numero-tutor.
           move tuto-qtd-pets-tutor         to s01-qtd-pets-tutor.
      *
       mover-tela-para-registro.
           initialize reg-tuto.
           move s01-cpf-tutor               to tuto-cpf-tutor.
           move s01-cd-tutor                to tuto-cd-tutor.
           move s01-nome-tutor              to tuto-nome-tutor.
           move s01-uf-tutor                to tuto-uf-tutor.
           move s01-cep-tutor               to tuto-cep-tutor.
           move s01-cidade-tutor            to tuto-cidade-tutor.
           move s01-bairro-tutor            to tuto-bairro-tutor.
           move s01-rua-tutor               to tuto-rua-tutor.
           move s01-numero-tutor            to tuto-numero-tutor.
           move s01-qtd-pets-tutor          to tuto-qtd-pets-tutor.
      *
       navegar-registros.
           perform limpa-tela.
           perform mover-registro-para-tela.
           perform modificar-componentes.
      *
       campo_enter.
           if s01-cd-tutor not equal zeros and
              s01-cd-tutor not equal null then
              display "Codigo ja existente"
           else
              perform habilitar-componentes
              perform desabilitar-navegacao
              modify t01-ef-cd-tutor enabled false
              perform chamarcampoenter
              set wss-inclusao to true
           end-if.
      *
       chamar-campoenter.
      *
       novo.
           if wss-inclusao
              perform limpa-tela
              perform desabilitar-navegacao
              perform habilitar-componentes
              perform mover-tela-para-registro
              read vbm-tuto with no lock
           else
           if not valid-vbm-tuto
           display message box "erro ao abrir o arquivo tutores!"
                               "status: " fs-vbm-tuto
           end-if.
           set wss-inclusao to true.
      *
       salvar.
           perform mover-tela-para-registro.
           if wss-inclusao
              write reg-tuto
              display message box "Sucesso ao incluir tutor!"
           end-if.
           if wss-alteracao
              rewrite reg-tuto
              display message box "Dados regravados com sucresso!"
           end-if.
           if not valid-vbm-tuto
              display message box "erro ao abrir o arquivo tutores!"
                                  "status: " fs-vbm-tuto
              perform desabilitar-componentes
           else
              perform limpa-tela
           end-if.
           set wss-pesquisa to true.
      *
       editar.
      *     move s01-cd-tutor to ttrs-cd-tutor
      *     read vbm-tuto with no lock
      *     if not valid-vbm-tuto or not equal zeros
      *        perform
      *    set wss-inclusao to true.
      *
       cancelar.
      *    tem certeza
      *     perform limpa-tela.
      *
       excluir.
      *
       buscar-codigo.
           move s01-cd-tutor to tuto-cd-tutor.
           read vbm-tuto with no lock.
           if not valid-vbm-tuto
              initialize screen-t01
              display message box
              "vbm-tuto not valid" x"0a"
           else
              move reg-tuto to screen-t01
              display tela-menu
           end-if.
      *
       primeiro.
      *
       ultimo.
      *
       proximo.
           move high-value to tuto-cd-tutor.
           start vbm-tuto key > tuto-cd-tutor
           read vbm-tuto previous with no lock.
           if not valid-vbm-tuto
              display message box "fim registro"
           else
              perform navegar-registros
           end-if.
      *
       anterior.
           move low-value to tuto-cd-tutor.
           start vbm-tuto key < tuto-cd-tutor.
           read vbm-tuto previous with no lock.
           if not valid-vbm-tuto
              display message box "primeiro registro"
           else
              perform navegar-registros
           end-if.
      *
