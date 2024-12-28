*&---------------------------------------------------------------------*
*& Report ZR_POO_9_DESTRUTOR
*&---------------------------------------------------------------------*
*   DESTRUTORES
*   - Primeiramente: não precisamos criar destrutores, essa explicação
*       é apenas para entermos como funciona.
*
*   - CLEAR ou FREE para limpar
*
*   - Podemos criar um método para destruir as instâncias dos objetos
*   - DESTRUCTOR é uma palavra reservada, não é possível criar ações para um método
*       esse nome.
*
*  GARBAGE COLECTOR (automático)
*   - Após a saída de um método, o garbage colector busca todas as sujeiras
*        que o programa OO possui, e verifica se a variável vai ser utilizada
*        em outra parte do programa. Caso não for mais utilizada, ele mesmo
*        destrói a variável / objeto
*
*&---------------------------------------------------------------------*
REPORT zr_poo_9_destrutor.


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
                      iv_ultima_mensagem TYPE boolean OPTIONAL
                        PREFERRED PARAMETER iv_mensagem,

      dizer_ultima_mensagem.

ENDCLASS.
**********************************************************************

START-OF-SELECTION.

  DATA: lo_locutor  TYPE REF TO lcl_locutor.


  " Com idade -----------------------------------------------------------
  lo_locutor = NEW lcl_locutor( iv_nome = 'Max' iv_idade = 23  ).

  lo_locutor->falar( 'Olá, sejam bem-vindos!' ). " Parâmetro preferencial iv_mensagem
  lo_locutor->falar( 'Quero café!' ).

  lo_locutor->dizer_ultima_mensagem( ).
  ULINE.

**********************************************************************

**********************************************************************
  " Destruindo objeto
  FREE lo_locutor.
  " DUMP, pois não temos mais a referência ao objeto
  "lo_locutor->dizer_ultima_mensagem( ).


**********************************************************************
  " Implementação da classe (métodos)
CLASS lcl_locutor IMPLEMENTATION.
  METHOD: constructor.
    nome = iv_nome.
    idade = iv_idade.
  ENDMETHOD.

  METHOD: falar.

    IF iv_ultima_mensagem IS NOT SUPPLIED.  " não está preenchido
      WRITE:/ 'Nome: ', nome COLOR COL_TOTAL, 'diz: ', iv_mensagem COLOR COL_POSITIVE.
    ELSE.
      WRITE:/ 'Nome: ', nome COLOR COL_TOTAL, 'diz novamente: ', iv_mensagem COLOR COL_KEY.
    ENDIF.
    ultima_mensagem = iv_mensagem.

  ENDMETHOD.

  METHOD: dizer_ultima_mensagem.
    me->falar( iv_mensagem = me->ultima_mensagem
               iv_ultima_mensagem = abap_true ).
  ENDMETHOD.
ENDCLASS.
