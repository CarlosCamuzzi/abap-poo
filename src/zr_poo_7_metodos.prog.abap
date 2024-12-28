*&---------------------------------------------------------------------*
*& Report ZR_POO_7_METODOS
*&---------------------------------------------------------------------*
* APRENDENDO MELHOR SOBRE MÉTODOS
*
* PARÂMETRO OPCIONAL
* iv_idade TYPE i OPTIONAL.
*
*
*  ASSINATURA DO MÉTODO
*   - Assinatura do método: nome + parâmetros do método
*   - ABAP não aceita métodos com mesmo nome (mesmo que tenham parâmetos diferentes).
*   - Em casos que precisamos de ter métodos de mesmo nome, usamos OPTIONAL.
*
*&---------------------------------------------------------------------*
*
* EXPORTING, IMPORTING, RETURNING, VALOR E REFERÊNCIA
*   - Podemos exportar ou importar parâmetro por valor ou por referência.
*   - POR VALOR, no importing/exporting
*         - IMPORTING VALUE(iv_parametro)
*         - EXPORTING VALUE(iv_parametro)
*
*   - POR REFERÊNCIA basta omitir o VALUE.
*
*   - RETURNING: Sempre é por valor
*     - RETURNING VALUE(parametro).
*     - Métodos com RETURNING são chamados de método funcional
*     - A diferença é que um método funcional podemos acessar o método
*         dentro de um condicional (IF, por exemplo).*
*
*  Obs.: Os parâmetros IMPORTING e EXPORTING podemos ter vários.
*        O parâmetro RETURNING só permite um.
*
*  Obs.: Priorizar RETURNING, para evitar métodos com vários retornos com EXPORTING
*        Métodos devem ter função única.
*
*&---------------------------------------------------------------------*
*
* PREFERRED PARAMETER
*     - Ao usar o PREFERRED PARAMETER, dizemos ao método o parâmetro
*         opcional que deve ser usado quando não passamos a variável:
*     - lo_locutor3->falar( iv_mensagem = 'Quero correr!').
*     - lo_locutor3->falar( 'Quero correr!').
*
*   - Na assinatura:
*   falar IMPORTING iv_mensagem TYPE string
*                   it_mensagem type string_table OPTIONAL
*                   PREFERRED PARAMETER iv_mensagem,
*
*   - Dessa forma, quando não especificamos a variável, ele vai usar preferencialmente
*       o parâmetro de definido em PREFERRED PARAMETER
*
*&---------------------------------------------------------------------*

REPORT zr_poo_7_metodos.



" Definição da classe
CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    DATA: nome            TYPE string,
          idade           TYPE i,
          ultima_mensagem TYPE string.

*    CLASS-METHODS:
*      _gerar_locutor IMPORTING iv_nome          TYPE string
*                               iv_idade         TYPE i OPTIONAL
*                     EXPORTING eo_locutor       TYPE REF TO lcl_locutor
*                     RETURNING VALUE(instancia) TYPE REF TO lcl_locutor.

    METHODS:
      constructor IMPORTING iv_nome  TYPE string
                            iv_idade TYPE i OPTIONAL, " parâmetro opcional

      falar IMPORTING iv_mensagem TYPE string       OPTIONAL
                      it_mensagem TYPE string_table OPTIONAL
                        PREFERRED PARAMETER iv_mensagem,

      possui_idade RETURNING VALUE(return) TYPE boolean.

    " Usando com EXPORTING, dará erro quando compilar, pois
    "   não podemos chamar o método com IF, conforme foi feito
    "possui_idade EXPORTING return TYPE boolean.

ENDCLASS.
**********************************************************************

START-OF-SELECTION.

  DATA: lo_locutor  TYPE REF TO lcl_locutor,
        lo_locutor2 TYPE REF TO lcl_locutor.

  " Com idade -----------------------------------------------------------
  lo_locutor = NEW lcl_locutor( iv_nome = 'Max' iv_idade = 23  ).
  lo_locutor->falar( iv_mensagem = 'Quero café!').

  " Sem idade -----------------------------------------------------------
  CREATE OBJECT lo_locutor2
    EXPORTING
      iv_nome = 'José'.

  lo_locutor2->falar( iv_mensagem = 'Quero ir ao shopping' ).

**********************************************************************

  " Somente com RETURNING é possível chamar um método diretamente na condição
  IF lo_locutor->possui_idade( ).
    WRITE:/ 'Possui idade'.
  ELSE.
    WRITE:/ 'Não possui idade'.
  ENDIF.

  IF lo_locutor2->possui_idade( ).
    WRITE:/ 'Possui idade'.
  ELSE.
    WRITE:/ 'Não possui idade'.
  ENDIF.

**********************************************************************

  " REFERRED PARAMETER iv_mensagem,
  DATA: lo_locutor3 TYPE REF TO lcl_locutor.

  ULINE.
  lo_locutor3 = NEW lcl_locutor( iv_nome = 'Júlio' iv_idade = 50  ).
  lo_locutor3->falar( 'Quero correr!').

**********************************************************************
  " Implementação da classe (métodos)
CLASS lcl_locutor IMPLEMENTATION.
  METHOD: constructor.
    nome = iv_nome.
    idade = iv_idade.
  ENDMETHOD.

  METHOD: falar.
    WRITE:/ 'Nome: ', nome,
            'Idade: ', idade,
            'Mensagem: ', iv_mensagem.
  ENDMETHOD.

  METHOD: possui_idade.
    IF idade IS INITIAL.
      return = abap_false.
    ELSE.
      return = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
