*&---------------------------------------------------------------------*
*& Report ZR_POO_1_PRIMEIRA_CLASSE
*&---------------------------------------------------------------------*
* Prefixo cl_ para classes
* Prefixo lcl_ para classes locais
*
* Na definição da CLASS, se escreve METHODS
* Na implementação da CLASSE, se escreve METHOD
*
* Obs.:
*   - A classe possui 2 'momentos': DEFINITION E IMPLEMENTATION
*   - Classes sem métodos na definição não precisam da implementação
*&---------------------------------------------------------------------*

REPORT ZR_POO_1_PRIMEIRA_CLASSE.

" Definição das classes
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    DATA: nome          TYPE string,
          idade         TYPE i,
          estilo_cabelo TYPE string.

    METHODS: falar.

ENDCLASS.

CLASS lcl_tecnico DEFINITION. " Para fins de estudo, classe sem nada (incomum)
ENDCLASS.

CLASS lcl_jogador DEFINITION.
  PUBLIC SECTION.
    DATA: numero     TYPE i,
          posicao    TYPE string,
          posse_bola TYPE boolean.

    METHODS:
      chutar,
      movimentar IMPORTING distancia TYPE p.  " TYPE p: decimal
ENDCLASS.

CLASS lcl_chuteira DEFINITION.
  PUBLIC SECTION.
    DATA: tamanho       TYPE i,
          marca         TYPE string,
          tamanho_trava TYPE c,
          lado          TYPE c.
ENDCLASS.

CLASS lcl_selecao DEFINITION.
  PUBLIC SECTION.
    METHODS:
      escalar_jogador IMPORTING jogador TYPE REF TO lcl_jogador.  " Referência à classe jogador
ENDCLASS.


" Implementação: Classe que possuem métodos -----------------------------------------------
CLASS lcl_pessoa IMPLEMENTATION.
  METHOD: falar.
    " Codificação aqui
  ENDMETHOD.
ENDCLASS.

CLASS lcl_jogador IMPLEMENTATION.
  METHOD: chutar.
  ENDMETHOD.

  METHOD: movimentar.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_selecao IMPLEMENTATION.
  METHOD: escalar_jogador.
  ENDMETHOD.
ENDCLASS.
