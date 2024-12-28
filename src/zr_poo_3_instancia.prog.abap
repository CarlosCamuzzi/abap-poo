*&---------------------------------------------------------------------*
*& Report ZR_POO_3_INSTANCIA
*&---------------------------------------------------------------------*
*&  Prefixo o_ para objetos (ou _r)
*&---------------------------------------------------------------------*
REPORT zr_poo_3_instancia.

" Definição da classe
CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    DATA: nome            TYPE string,
          idade           TYPE i,
          ultima_mensagem TYPE string.

    METHODS:
      falar IMPORTING iv_mensagem TYPE string.
ENDCLASS.

START-OF-SELECTION.

  " Formas de instanciar
  " Primeira forma ------------------------------------
  DATA: o_locutor TYPE REF TO lcl_locutor.

  CREATE OBJECT o_locutor. " Instância

  " WRITE:/ o_locutor->nome. " Acessando variável

  " Obs.: forma de verificar se objeto foi instanciado
  IF o_locutor IS BOUND.    " ou com IS INITIAL (quando não criado)
    WRITE:/ 'OBJETO CRIADO'.
  ELSE.
    WRITE:/ 'OBJETO NÃO CRIADO'.
  ENDIF.

  " Segunda Forma -------------------------------------
  DATA: o_locutor2 TYPE REF TO lcl_locutor.
  o_locutor2 = NEW lcl_locutor( ).

  " Implementação da classe (métodos)
CLASS lcl_locutor IMPLEMENTATION.
  METHOD: falar.
  ENDMETHOD.
ENDCLASS.
