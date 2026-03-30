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
       copy "ACUGUI.DEF".
       COPY "ACUCOBOL.DEF".
       COPY "crtvars.def".
      *
       01 screen-t01.
          03 s01-cd-tutor                   pic 9(07).
          03 s01-rn-tutor                   pic x(18).
          03 s01-nm-tutor                   pic x(30).
          03 s01-contato-tutor              pic x(17).
          03 s01-tp-pessoa-tutor            pic 9(01).
             88 s01-fisica-tutor            value 1.
             88 s01-juridica-tutor          value 2.
          03 s01-qtd-pets-tutor             pic 9(02).
          03 s01-uf-tutor                   pic x(02).
          03 s01-cep-tutor                  pic x(09).
          03 s01-cidade-tutor               pic x(30).
          03 s01-bairro-tutor               pic x(30).
          03 s01-rua-tutor                  pic x(30).
          03 s01-numero-tutor               pic 9(12).
          03 s01-status                     pic x(50).
          03 w-id-field                     pic s9(06).
      *
       77 keystatus                         pic 9(04)
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
       01 wss-controle                      pic 9(01).
          88 wss-inclusao                   value 1.
          88 wss-alteracao                  value 2.
          88 wss-pesquisa                   value 3.
      *
       01 s01-fl-campos-validos             pic 9(01).
          88 s01-campos-validos             value 1 false 0.
      *
       01 s01-fl-validar-todos-campos       pic 9(01).
          88 s01-validar-todos-campos       value 1 false 0.
      *
      ******************************************************************
      *                         screen section                         *
      ******************************************************************
       screen                               section.
       copy "tutores-tela-tuto.scr".
      *
      ******************************************************************
      *                       procedure division                       *
      ******************************************************************
       procedure                            division.
           perform abrir-arquivos.
           perform inicio-programa.
           perform fechar-arquivos.
      *
       abrir-arquivos.
           open i-o vbm-tuto.
           if not valid-vbm-tuto
              display message box "erro ao abrir o arquivo vbm-tuto!"
                                  "status: " fs-vbm-tuto
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
           set s01-fisica-tutor to true.
           modify t01-rb-tp-fisica-tutor   value s01-tp-pessoa-tutor.
           modify t01-rb-tp-juridica-tutor value s01-tp-pessoa-tutor.
           perform desabilitar-componentes.
           perform desabilitar-salvar-cancelar.
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
                 perform f2-link-to
              when novo-key
                 perform novo-link-to
              when editar-key
                 perform editar-link-to
              when excluir-key
                 perform excluir-link-to
              when primeiro-key
                 perform primeiro-link-to
              when anterior-key
                 perform anterior-link-to
              when proximo-key
                 perform proximo-link-to
              when ultimo-key
                 perform ultimo-link-to
              when salvar-key
                 perform salvar-link-to
              when cancelar-key
                 perform cancelar-link-to
           end-evaluate.
      *
       modificar-componentes.
           modify t01-ef-cd-tutor          value s01-cd-tutor.
           modify t01-ef-rn-tutor          value s01-rn-tutor.
           modify t01-ef-nm-tutor          value s01-nm-tutor.
           modify t01-ef-contato-tutor     value s01-contato-tutor.
           modify t01-ef-qtd-pets-tutor    value s01-qtd-pets-tutor.
           modify t01-ef-uf-tutor          value s01-uf-tutor.
           modify t01-ef-cep-tutor         value s01-cep-tutor.
           modify t01-ef-cidade-tutor      value s01-cidade-tutor.
           modify t01-ef-bairro-tutor      value s01-bairro-tutor.
           modify t01-ef-rua-tutor         value s01-rua-tutor.
           modify t01-ef-numero-tutor      value s01-numero-tutor.
           if s01-tp-pessoa-tutor = 0
              set s01-fisica-tutor to true
           end-if.
           modify t01-rb-tp-fisica-tutor   value s01-tp-pessoa-tutor.
           modify t01-rb-tp-juridica-tutor value s01-tp-pessoa-tutor.
      *
       habilitar-componentes.
           modify t01-ef-rn-tutor           enabled true.
           modify t01-ef-nm-tutor           enabled true.
           modify t01-ef-contato-tutor      enabled true.
           modify t01-ef-qtd-pets-tutor     enabled true.
           modify t01-ef-uf-tutor           enabled true.
           modify t01-ef-cep-tutor          enabled true.
           modify t01-ef-cidade-tutor       enabled true.
           modify t01-ef-bairro-tutor       enabled true.
           modify t01-ef-rua-tutor          enabled true.
           modify t01-ef-numero-tutor       enabled true.
           modify t01-rb-tp-fisica-tutor    enabled true.
           modify t01-rb-tp-juridica-tutor  enabled true.
      *
       desabilitar-componentes.
           modify t01-ef-rn-tutor           enabled false.
           modify t01-ef-nm-tutor           enabled false.
           modify t01-ef-contato-tutor      enabled false.
           modify t01-ef-qtd-pets-tutor     enabled false.
           modify t01-ef-uf-tutor           enabled false.
           modify t01-ef-cep-tutor          enabled false.
           modify t01-ef-cidade-tutor       enabled false.
           modify t01-ef-bairro-tutor       enabled false.
           modify t01-ef-rua-tutor          enabled false.
           modify t01-ef-numero-tutor       enabled false.
           modify t01-rb-tp-fisica-tutor    enabled false.
           modify t01-rb-tp-juridica-tutor  enabled false.
      *
       habilitar-salvar-cancelar.
           modify t01-pb-salvar             enabled true.
           modify t01-pb-cancelar           enabled true.
      *
       desabilitar-salvar-cancelar.
           modify t01-pb-salvar             enabled false.
           modify t01-pb-cancelar           enabled false.
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
       mover-tela-para-registro.
           initialize reg-tuto.
           move s01-cd-tutor                to tuto-cd-tutor.
      *
           if s01-fisica-tutor
              move s01-rn-tutor             to tuto-rn-tutor
           end-if.
           if s01-juridica-tutor
              move s01-rn-tutor             to tuto-rn-tutor
           end-if.
      *
           if s01-fisica-tutor
              set tuto-fisica-tutor to true
           end-if.
           if s01-juridica-tutor
              set tuto-juridica-tutor to true
           end-if.
      *
           move s01-nm-tutor                to tuto-nm-tutor.
      *
           string
              s01-contato-tutor(2:3)        delimited by size
              s01-contato-tutor(7:5)        delimited by size
              s01-contato-tutor(13:4)       delimited by size
              into tuto-contato-tutor
           end-string.
      *
           move s01-qtd-pets-tutor          to tuto-qtd-pets-tutor.
           move s01-uf-tutor                to tuto-uf-tutor.
           move s01-cidade-tutor            to tuto-cidade-tutor.
           move s01-bairro-tutor            to tuto-bairro-tutor.
           move s01-rua-tutor               to tuto-rua-tutor.
           move s01-numero-tutor            to tuto-numero-tutor.
      *
           string
              s01-cep-tutor(1:5)            delimited by size
              s01-cep-tutor(7:3)            delimited by size
              into tuto-cep-tutor
           end-string.
      *
       mover-registro-para-tela.
           initialize screen-t01.
           move tuto-cd-tutor               to s01-cd-tutor.
      *
           if tuto-fisica-tutor
              string
                   tuto-rn-tutor(1:3)       delimited by size
                   "."
                   tuto-rn-tutor(4:3)       delimited by size
                   "."
                   tuto-rn-tutor(7:3)       delimited by size
                   "-"
                   tuto-rn-tutor(10:2)      delimited by size
                into s01-rn-tutor
             end-string
           end-if.
           if tuto-juridica-tutor
              string
                    tuto-rn-tutor(1:2)      delimited by size
                    "."
                    tuto-rn-tutor(3:3)      delimited by size
                    "."
                    tuto-rn-tutor(6:3)      delimited by size
                    "/"
                    tuto-rn-tutor(9:4)      delimited by size
                    "-"
                    tuto-rn-tutor(13:2)     delimited by size
                 into s01-rn-tutor
              end-string
           end-if.
      *
           if tuto-fisica-tutor
              set s01-fisica-tutor to true
           end-if.
           if tuto-juridica-tutor
              set s01-juridica-tutor to true
           end-if.
      *
           move tuto-nm-tutor               to s01-nm-tutor.
           string
                "("  tuto-ddd-tutor         delimited by size
                ") " tuto-fone-tutor(1:5)   delimited by size
                "-"  tuto-fone-tutor(6:4)   delimited by size
                  into s01-contato-tutor
              end-string.
      *
           move tuto-qtd-pets-tutor         to s01-qtd-pets-tutor.
           move tuto-uf-tutor               to s01-uf-tutor.
      *
              string
                    tuto-cep-tutor(1:5)     delimited by size
                    "-"
                    tuto-cep-tutor(6:3)     delimited by size
                 into s01-cep-tutor
              end-string.
      *
           move tuto-cidade-tutor           to s01-cidade-tutor.
           move tuto-bairro-tutor           to s01-bairro-tutor.
           move tuto-rua-tutor              to s01-rua-tutor.
           move tuto-numero-tutor           to s01-numero-tutor.
      *
       navegar-registros.
           initialize screen-t01.
           perform mover-registro-para-tela.
           perform modificar-componentes.
      *
       carregar-sequencia.
           initialize reg-tuto.
           move high-value                  to tuto-cd-tutor.
           start vbm-tuto key < tuto-chave-tutor.
           read vbm-tuto previous with no lock.
           if valid-vbm-tuto
              add 1 to tuto-cd-tutor        giving s01-cd-tutor
           else
              move 1 to s01-cd-tutor
           end-if.
      *
       carregar-barra-info.
           initialize s01-status
      *
           inquire t01-ef-cd-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o codigo do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-nm-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o nome do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-rn-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Preencha com o CPF / CPNJ do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-contato-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o telefone do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-qtd-pets-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe a quantidade de pets"
              to s01-status
           end-if.
      *
           inquire t01-ef-qtd-pets-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe a quantidade de pets"
              to s01-status
           end-if.
      *
           inquire t01-ef-cidade-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe a cidade do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-cep-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o CEP do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-uf-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe a UF do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-bairro-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o bairro do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-rua-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe a rua do tutor"
              to s01-status
           end-if.
      *
           inquire t01-ef-numero-tutor id in w-id-field.
           if w-id-field equal control-id
              move "Informe o numero da propriedade do tutor"
              to s01-status
           end-if.
      *
           modify t01-la-status title s01-status.
      *
       cancelar-link-to.
           initialize reg-tuto.
           perform modificar-componentes.
           perform desabilitar-salvar-cancelar.
           perform desabilitar-componentes.
           perform habilitar-navegacao.
           modify t01-ef-cd-tutor           enabled true.
           set wss-pesquisa                 to true.
      *
       salvar-link-to.
           set s01-validar-todos-campos     to true.
           initialize reg-tuto.
           perform mover-tela-para-registro
           if wss-inclusao
              write reg-tuto
              if not valid-vbm-tuto
                 if fs-vbm-tuto equal       "22"
                    display message box     "Registro ja cadastrado"
                 exit paragraph
              else
                 display message box        "erro ao gravar tutores!"
                                            "status: " fs-vbm-tuto
              end-if
              exit paragraph
           end-if.
           if wss-alteracao
              set wss-inclusao to true
              rewrite reg-tuto
              if not valid-vbm-tuto
                 display message box "erro ao alterar arquivo tutores!"
                                     "status: " fs-vbm-tuto
           end-if.
           perform validar-campos.
           set s01-validar-todos-campos     to false.
           if not s01-campos-validos
              exit paragraph
           end-if.
           perform desabilitar-salvar-cancelar.
           perform desabilitar-componentes
           perform habilitar-navegacao
           modify t01-ef-cd-tutor enabled true.
           set wss-pesquisa to true.
      *
       novo-link-to.
           perform carregar-sequencia.
           initialize screen-t01.
           perform habilitar-componentes.
           perform modificar-componentes.
           perform desabilitar-navegacao.
           modify t01-pb-novo enabled false.
           modify t01-pb-editar enabled false.
           modify t01-pb-excluir enabled false.
           perform habilitar-salvar-cancelar.
           set wss-inclusao to true.
      *
       editar-link-to.
           initialize reg-tuto.
           move s01-cd-tutor                to tuto-cd-tutor.
           read vbm-tuto with no lock.
           if not valid-vbm-tuto
              display message box
                 "Registro do tutor nao pode ser encontrado"
                x"0a" "File status:" fs-vbm-tuto
              modify t01-pb-editar          enabled false
              modify t01-pb-excluir         enabled false
              inquire t01-ef-cd-tutor       id in control-id
              move 4                        to accept-control
              exit paragraph
           end-if.
           modify t01-pb-novo               enabled false.
           modify t01-pb-editar             enabled false.
           modify t01-ef-cd-tutor           enabled false.
           perform desabilitar-navegacao.
           perform habilitar-salvar-cancelar.
           perform habilitar-componentes.
           set wss-alteracao                to true.
           inquire t01-ef-nm-tutor          id in control-id.
           move 4                           to accept-control.
      *
       excluir-link-to.
           initialize reg-tuto.
           move s01-cd-tutor to tuto-cd-tutor.
           read vbm-tuto with no lock.
           if not valid-vbm-tuto
              display message box
              "Registro do tutor nao pode ser encontrado"
             x"0a" "File status:" fs-vbm-tuto
              exit paragraph
           end-if.
              display message box
                 "Voce realmente deseja excluir o registro do tutor?"
                 type 2
                 giving return-code.
           if return-code equal mb-yes
              delete vbm-tuto
              display message box "Tutor removido"
              initialize screen-t01
              perform modificar-componentes
           else
              display message box
              "Acao interrompida"
           end-if.
      *
       primeiro-link-to.
           initialize reg-tuto.
           perform mover-tela-para-registro.
           move low-value to tuto-cd-tutor.
           start vbm-tuto key > tuto-cd-tutor.
           read vbm-tuto previous with no lock.
           if valid-vbm-tuto
              perform navegar-registros
           end-if.
           inquire T01-ef-cd-tutor          id in control-id.
           move 4                           to accept-control.

      *
       ultimo-link-to.
           initialize reg-tuto.
           perform mover-tela-para-registro.
           move high-value                  to tuto-cd-tutor.
           start vbm-tuto key < tuto-cd-tutor.
           read vbm-tuto previous with no lock.
           if valid-vbm-tuto
              perform navegar-registros
           end-if.
           inquire T01-ef-cd-tutor          id in control-id.
           move 4                           to accept-control.

      *
       anterior-link-to.
           initialize reg-tuto.
           perform mover-tela-para-registro.
           start vbm-tuto key < tuto-cd-tutor.
           read vbm-tuto previous with no lock.
           if not valid-vbm-tuto
              display
                 message box
                 "Voce chegou ao primeiro registro"
           else
              perform navegar-registros
           end-if.
           inquire T01-ef-cd-tutor          id in control-id.
           move 4                           to accept-control.
      *
       proximo-link-to.
           initialize reg-tuto.
           perform mover-tela-para-registro.
           start vbm-tuto key > tuto-cd-tutor
           read vbm-tuto next with no lock.
           if not valid-vbm-tuto
              display
                 message box
                 "Voce chegou ao ultimo registro"
           else
              perform navegar-registros
           end-if.
           inquire T01-ef-cd-tutor          id in control-id.
           move 4                           to accept-control.
      *
       f2-link-to.
           move s01-cd-tutor to tuto-cd-tutor.
           read vbm-tuto with no lock.
           if not valid-vbm-tuto
              initialize screen-t01
              display
                  message box
                 "vbm-tuto not valid" x"0a"
           else
              move reg-tuto to screen-t01
              perform modificar-componentes
           end-if.
      *
       t01-ef-cd-tutor-aft-proc.
           modify t01-la-status title s01-status.
              if s01-cd-tutor equal spaces or equal zeros
                 display message box
                 "Informe um codigo valido!"
                 set s01-campos-validos               to false
                 perform modificar-componentes
                 inquire t01-ef-cd-tutor              id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if.
           initialize reg-tuto.
           move s01-cd-tutor to tuto-cd-tutor.
           read vbm-tuto with no lock.
           if wss-pesquisa
              if valid-vbm-tuto
                 perform mover-registro-para-tela
                 perform modificar-componentes
              else
                 display
                    message box
                    "Deseja cadastrar novo tutor?"
                    type 2
                    giving return-code
                 if return-code equal mb-yes
                    initialize screen-t01
                    perform mover-registro-para-tela
                    perform modificar-componentes
                    perform habilitar-componentes
                    perform desabilitar-navegacao
                    perform habilitar-salvar-cancelar
                    modify t01-pb-novo      enabled false
                    modify t01-pb-editar    enabled false
                    modify t01-pb-excluir   enabled false
                    set wss-inclusao        to true
                 else
                    initialize screen-t01
                    perform modificar-componentes
                    perform habilitar-navegacao
                    modify t01-pb-salvar    enabled false
                    modify t01-pb-cancelar  enabled false
                    inquire t01-ef-cd-tutor id in control-id
                    move 4                  to accept-control
              end-if
           end-if.
      *
       t01-rb-tp-fisica-evt-proc.
           evaluate event-type
           when cmd-clicked
              perform t01-rb-tp-pessoa-tutor-evt-cmd-clicked
           when other
           end-evaluate.
      *
       t01-rb-tp-juridica-evt-proc.
           evaluate event-type
           when cmd-clicked
              perform t01-rb-tp-pessoa-tutor-evt-cmd-clicked
           when other
           end-evaluate.
      *
       t01-rb-tp-pessoa-tutor-evt-cmd-clicked.
           inquire t01-rb-tp-fisica-tutor   value s01-tp-pessoa-tutor.
           inquire t01-rb-tp-juridica-tutor value s01-tp-pessoa-tutor.
      *
       validar-campos.
           set s01-campos-validos                     to true.
           initialize s01-status.
           modify t01-la-status title s01-status.
           if keystatus equal 52
              exit paragraph
           end-if.
      *
           inquire t01-ef-nm-tutor id in w-id-field.
           if w-id-field equal control-id or s01-validar-todos-campos
              if s01-nm-tutor equal spaces
                 display message box
                 "Informe o nome do tutor!"
                 set s01-campos-validos               to false
                 inquire t01-ef-nm-tutor              id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-rn-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-rn-tutor equal spaces
                 display message box
                 "Informe o CPF ou CNPJ do tutor!"
                 set s01-campos-validos               to false
                 inquire t01-ef-rn-tutor              id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-contato-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-contato-tutor equal spaces
                 display message box
                 "Informe o celular do tutor!"
                 set s01-campos-validos               to false
                 inquire t01-ef-contato-tutor         id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-qtd-pets-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-qtd-pets-tutor equal spaces
                 display message box
                 "Informe a quantidade de pets do tutor!"
                 set s01-campos-validos               to false
                 inquire t01-ef-qtd-pets-tutor        id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-cidade-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-cidade-tutor equal spaces
                 display message box
                 "Informe a cidade do tutor!"
                 set s01-campos-validos               to false
                 inquire t01-ef-cidade-tutor          id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-cep-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-cep-tutor equal spaces
                 display message box
                 "Informe o CEP do tutor"
                 set s01-campos-validos               to false
                 inquire t01-ef-cep-tutor        id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-uf-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-uf-tutor equal spaces
                 display message box
                 "Informe a UF do tutor"
                 set s01-campos-validos               to false
                 inquire t01-ef-uf-tutor              id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-rua-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-rua-tutor equal spaces
                 display message box
                 "Informe a rua do tutor"
                 set s01-campos-validos               to false
                 inquire t01-ef-rua-tutor             id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-bairro-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-bairro-tutor equal spaces
                 display message box
                 "Informe o bairro do tutor"
                 set s01-campos-validos               to false
                 inquire t01-ef-bairro-tutor          id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *
           inquire t01-ef-numero-tutor id in w-id-field.
              if w-id-field equal control-id or s01-validar-todos-campos
                 if s01-numero-tutor equal spaces
                 display message box
                 "Informe o numero da moradia do tutor"
                 set s01-campos-validos               to false
                 inquire t01-ef-numero-tutor          id in control-id
                 move 4                               to accept-control
                 exit paragraph
              end-if
           end-if.
      *