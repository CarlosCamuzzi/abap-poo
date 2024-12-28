*&---------------------------------------------------------------------*
*& Report ZR_POO_10_ENCAPSULAMENTO
*&---------------------------------------------------------------------*
*   ENCAPSULAMENTO
*
*   - Obs.: é preciso seguir a ordem de declaração das seções
*
*   - PUBLIC SECTION: Acessível para qualquer instância da classe, objeto
*       dentro do programa.
*
*   - PRIVATE SECTION: Limita que somente a instância acesse.
*     - A seção privada encapsula os atributos e métodos que não queremos que
*         seja de acesso livre.
*     - Utilizamos métodos para conseguir acessar esses atributos
*
*   - PROTECTED SECTION: Mesmo efeito da privada, porém as classes filhas também
*       podem acessar (herança).
*
*&---------------------------------------------------------------------*


REPORT zr_poo_10_encapsulamento.


CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string,

      perguntar_nome IMPORTING io_pessoa TYPE REF TO lcl_pessoa,  " Qualquer pessoa pode perguntar
      responder_nome.

  PRIVATE SECTION.
    DATA:
      nome TYPE string.

    METHODS:
      " A ideia aqui é que não falamos o nome para qualquer pessoa, por isso PRIVATE
      " Somente a instância pode acessar
      falar IMPORTING iv_frase TYPE string.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_pessoa_joao  TYPE REF TO lcl_pessoa,
        lo_pessoa_maria TYPE REF TO lcl_pessoa.

  lo_pessoa_joao = NEW lcl_pessoa( 'João' ).
  lo_pessoa_maria = NEW lcl_pessoa( 'Maria' ).

  WRITE:/ 'João pergunta para Maria'.
  lo_pessoa_joao->perguntar_nome( lo_pessoa_maria ).

  SKIP.
  WRITE:/ 'Maria pergunta para João'.
  lo_pessoa_maria->perguntar_nome( lo_pessoa_joao ).


CLASS lcl_pessoa IMPLEMENTATION.
  METHOD: constructor.
    me->nome = iv_nome.
  ENDMETHOD.

  METHOD: falar.
    WRITE:/ iv_frase.
  ENDMETHOD.

  " Estamos usando uma ação externa/pública para acessar uma interna
  METHOD: perguntar_nome. " Público
    me->falar('Qual seu nome?' ) .  " Privado
    io_pessoa->responder_nome( ).
  ENDMETHOD.

  METHOD: responder_nome.
    me->falar( |Meu nome é { me->nome }| ).
  ENDMETHOD.

ENDCLASS.
