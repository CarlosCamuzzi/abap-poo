*&---------------------------------------------------------------------*
*& Report ZR_POO_2_HERANCA_RELAC
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
*
* -----------------------------------------------------------------------
* HERANÇA:
*   - CLASS lcl_tecnico DEFINITION INHERITING FROM lcl_pessoa.
*
*
* RELACIONAMENTOS
*   - Quando existem relacionamentos com várias ocorrências como 1 : N,
*       teremos então uma tabela interna.
*  Ex.: DATA: JOGADORES TYPE TABLE OF REF TO lcl_pessoa.   " Tabela interna referenciando lcl_pessoa (1 : N)
*
*   - Relacionamentos 1 : 1, apenas referenciamos a classe
*  Ex.: selecao TYPE REF TO lcl_selecao.
*
* --> Ou seja: para 1:N usar TYPE TABLE OF REF TO, para referenciar uma tabela interna
*              para 1:1 ou 1:0 usar TYPE REF TO, para dizer que é referente ao tipo, mas não uma tabela
*
*
* GLOSSÁRIO DE CLASSES
*   - Se uma classe acima faz referência a uma classe que está abaixo dela,
*         resultará em erro.
*   - Para resolver temos que declar um Glossário para adiar a definição.
*  Ex.: CLASS lcl_jogador DEFINITION DEFERRED.
*&---------------------------------------------------------------------*

REPORT zr_poo_2_heranca_relac.


* Glossário de classes (definição adiada)
CLASS lcl_jogador  DEFINITION DEFERRED.
CLASS lcl_selecao  DEFINITION DEFERRED.
CLASS lcl_chuteira DEFINITION DEFERRED.
CLASS lcl_campo    DEFINITION DEFERRED.

" Definição das classes
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    DATA: nome          TYPE string,
          idade         TYPE i,
          estilo_cabelo TYPE string.

    METHODS: falar.
ENDCLASS.

CLASS lcl_tecnico DEFINITION INHERITING FROM lcl_pessoa. " Herança
ENDCLASS.

CLASS lcl_jogador DEFINITION INHERITING FROM lcl_pessoa.  " Herança
  PUBLIC SECTION.
    DATA: numero     TYPE i,
          posicao    TYPE string,
          posse_bola TYPE boolean,
          selecao    TYPE REF TO lcl_selecao,            " 1:1
          chuteiras  TYPE TABLE OF REF TO lcl_chuteira,  " Tabela interna referenciando lcl_chuteira (1:N)
          campo      TYPE REF TO lcl_campo.              " 1:1

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
    DATA: jogadores TYPE TABLE OF REF TO lcl_pessoa,   " Tabela interna referenciando lcl_pessoa (1:N)
          selecao   TYPE REF TO lcl_tecnico.           " 1:1

    METHODS:
      escalar_jogador IMPORTING jogador TYPE REF TO lcl_jogador.
ENDCLASS.

CLASS lcl_campo DEFINITION.
  PUBLIC SECTION.
    DATA: tamanho       TYPE i,
          marca         TYPE string,
          tamanho_trava TYPE c,
          lado          TYPE c.
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
