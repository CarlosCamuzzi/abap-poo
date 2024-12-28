*&---------------------------------------------------------------------*
*& Report ZR_POO_11_GET_SET
*&---------------------------------------------------------------------*
*&
*   GETTERS E SETTERS
*     - No abap, a convenção usada para definir nomes de métodos:
*       - set_nome
*       - get_nome
*
*   - O get e set vão trabalhar junto da ideia de encapsulamento,
*       de modo que o acesso ao atributo privado é feito com uso de
*       método público, tando a definição como a leitura desse valor.
*
*   - O get só vai retornar mesmo normalmente.
*   - No set podemos fazer validações para os atributos.
*
*   - Esses métodos em geral são simples, define e resgata o valor do atributo
*   - Como todo atributo privado precisa de um get e set, acaba ficando engessado
*       fazer isso manualmente, então criamos um comando para facilitar: getset
*
*  Obs.: Podemos criar com REGION partes definidas/comentadas
*&---------------------------------------------------------------------*

REPORT zr_poo_11_get_set.


CLASS lcl_doc_contabil DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_belnr TYPE belnr_d
                            iv_bukrs TYPE bukrs
                            iv_gjahr TYPE gjahr
                            iv_blart TYPE blart OPTIONAL,

      apresentar_documento,

      set_blart IMPORTING iv_blart TYPE blart,
      get_blart RETURNING VALUE(return) TYPE blart,

      set_belnr IMPORTING iv_belnr TYPE belnr_d,
      get_belnr RETURNING VALUE(return) TYPE belnr_d,

      set_bukrs IMPORTING iv_bukrs TYPE bukrs,
      get_bukrs RETURNING VALUE(return) TYPE bukrs,

      set_gjahr IMPORTING iv_gjahr TYPE gjahr,
      get_gjahr RETURNING VALUE(return) TYPE gjahr.

  PRIVATE SECTION.
    DATA:
      belnr TYPE belnr_d,
      bukrs TYPE bukrs,
      gjahr TYPE gjahr,
      blart TYPE blart.
ENDCLASS.
**********************************************************************

START-OF-SELECTION.

  " Instanciando o objeto sem o blart.
  DATA(lo_documento) = NEW lcl_doc_contabil( iv_belnr = '1'
                                             iv_bukrs = '9999'
                                             iv_gjahr = '2024').

  lo_documento->apresentar_documento( ).

  " Setando blart via setter
  lo_documento->set_blart( 'DZ' ).

  lo_documento->apresentar_documento( ).

  " Retornando valor de BUKRS com o get
  DATA(lv_bukrs) = lo_documento->get_bukrs( ).
  WRITE:/ lv_bukrs.

**********************************************************************
CLASS lcl_doc_contabil IMPLEMENTATION.
  METHOD: constructor.
    me->belnr = iv_belnr.
    me->bukrs = iv_bukrs.
    me->gjahr = iv_gjahr.
    me->blart = iv_blart.
  ENDMETHOD.

  METHOD: apresentar_documento.
    WRITE:/ 'Documento Contábil:', me->belnr.
    WRITE:/ 'Exercício:', me->gjahr.
    WRITE:/ 'Empresa:', me->bukrs.
    WRITE:/ 'Tipo de Documento:', me->blart.
    SKIP.
  ENDMETHOD.

  "$. Region Getter e Setters
  METHOD: set_gjahr.
    me->gjahr = iv_gjahr.
  ENDMETHOD.

  METHOD: get_gjahr.
    return = me->gjahr.
  ENDMETHOD.

  METHOD: set_bukrs.
    me->bukrs = iv_bukrs.
  ENDMETHOD.

  METHOD: get_bukrs.
    return = me->bukrs.
  ENDMETHOD.

  METHOD: set_belnr.
    me->belnr = iv_belnr.
  ENDMETHOD.

  METHOD: get_belnr.
    return = me->belnr.
  ENDMETHOD.

  METHOD: set_blart.
    me->blart = iv_blart.
  ENDMETHOD.

  METHOD: get_blart.
    return = me->blart.
  ENDMETHOD.
  "$. Endregion Getter e Setters
ENDCLASS.
