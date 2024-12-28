*&---------------------------------------------------------------------*
*& Report ZR_POO_8_ATRIB_VAR_LOCAL
*&---------------------------------------------------------------------*
*  ATRIBUTOS X VARIÁVEIS lOCAIS
*   - Podem ocorrer casos que temos em um método uma variável local
*       de mesmo nome que o atributo da classe.
*   - No exemplo foi usado no método dizer_ultima_mensagem a variável ultima_mensagem
*
*   - Nesses casos, para o programa entender que estamos falando do atributo
*       do objeto, devemos utilizar "me->" para se referenciar ao atributo
*   - Ex.: me->ultima_mensagem.
*
* ---------------------------------------------------------------------
*
*   EXEMPLO:
*   - Sem utilizar o me->ultima_mensagem, o programa vai ler a variável local
*      WRITE:/ 'Última mensagem dita (LOCAL): ', ultima_mensagem COLOR COL_GROUP.
*
*   - Usando o me->, vai ler o atributo do objeto
*      WRITE:/ 'Última mensagem dita (OBJETO) ', me->ultima_mensagem COLOR COL_GROUP.
*
*&---------------------------------------------------------------------*

REPORT zr_poo_8_atrib_var_local.

" Definição da classe
CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    DATA: nome            TYPE string,
          idade           TYPE i,
          ultima_mensagem TYPE string.


    METHODS:
      constructor IMPORTING iv_nome  TYPE string
                            iv_idade TYPE i OPTIONAL,

      falar IMPORTING iv_mensagem        TYPE string       OPTIONAL
                      it_mensagem        TYPE string_table OPTIONAL
                      " Acrescentado para reescrever os métodos falar e dizer_ultima_mensagem
                      iv_ultima_mensagem TYPE boolean OPTIONAL
                        PREFERRED PARAMETER iv_mensagem,

      dizer_ultima_mensagem.

ENDCLASS.
**********************************************************************

START-OF-SELECTION.

  DATA: lo_locutor  TYPE REF TO lcl_locutor,
        lo_locutor2 TYPE REF TO lcl_locutor.

  " Com idade -----------------------------------------------------------
  lo_locutor = NEW lcl_locutor( iv_nome = 'Max' iv_idade = 23  ).

  " Se passar a iv_ultima_mensagem como abap_false, ele vai entrar no IF
  "    do método falar (IS NOT SUPPLIED)
  lo_locutor->falar( 'Olá, sejam bem-vindos!' ). " Parâmetro preferencial iv_mensagem
  lo_locutor->falar( 'Quero café!' ).

  lo_locutor->dizer_ultima_mensagem( ).
  ULINE.

  lo_locutor->falar( 'Quero ir para casa!').
  lo_locutor->falar( 'Quero dormir!').

  lo_locutor->dizer_ultima_mensagem( ).
  ULINE.


**********************************************************************
  " Implementação da classe (métodos)
CLASS lcl_locutor IMPLEMENTATION.
  METHOD: constructor.
    nome = iv_nome.
    idade = iv_idade.
  ENDMETHOD.

  " Reescrevendo método falar e dizer_ultima_mensagem para aproveitar
  "   o comportamento do método falar.
  " Acrescentei o parâmetro opcional iv_ultima_mensagem no método falar
  METHOD: falar.

    IF iv_ultima_mensagem IS NOT SUPPLIED.  " não está preenchido
      WRITE:/ 'Nome: ', nome COLOR COL_TOTAL, 'diz: ', iv_mensagem COLOR COL_POSITIVE.
      " Sempre vai entrar no ELSE quando não passamos no lo_locutor->falar o valor de iv_ultima_mensagem
    ELSE.
      WRITE:/ 'Nome: ', nome COLOR COL_TOTAL, 'diz novamente: ', iv_mensagem COLOR COL_KEY.
    ENDIF.

    ultima_mensagem = iv_mensagem.

  ENDMETHOD.

  METHOD: dizer_ultima_mensagem.
    " Dessa forma, passamos um parâmetro para definir se é ou não a última mensagem
    " Desse modo podemos reaproveitar o método e focamos apenas na lógica do método FALAR
    me->falar( iv_mensagem = me->ultima_mensagem
               iv_ultima_mensagem = abap_true ).  " Ou seja, estamos passando uma última mensagem

    " Obs.: Quando temos somente IMPORTING, não precisa passar EXPORTING
    "     me->falar(
*       EXPORTING
*         iv_mensagem        =
*         it_mensagem        =
*         iv_ultima_mensagem =
    "     ).
  ENDMETHOD.

  " Métodos reescritos para exemplificar o reaproveitamento do método falar
  " Métodos sem a utilização do iv_ultima_mensagem
*  METHOD: falar.
*    WRITE:/ 'Nome: ', nome COLOR COL_TOTAL,
*            "'Idade: ', idade,
*            'Mensagem: ', iv_mensagem COLOR COL_POSITIVE.
*    ultima_mensagem = iv_mensagem.  " Atribui sempre a última mensagem dita
*  ENDMETHOD.
*
*  METHOD: dizer_ultima_mensagem.
*    " Variável local de mesmo nome do atributo da classe
*    DATA: ultima_mensagem TYPE string.
*    ultima_mensagem = 'Eu sou uma variável local'.
*
*    " Sem utilizar o me->ultima_mensagem, o programa vai ler a variável local
*    WRITE:/ 'Última mensagem dita (LOCAL): ', ultima_mensagem COLOR COL_HEADING.
*
*    " Usando o me->, vai ler o atributo do objeto
*    WRITE:/ 'Última mensagem dita (OBJETO): ', me->ultima_mensagem COLOR COL_GROUP.
*  ENDMETHOD.
ENDCLASS.
