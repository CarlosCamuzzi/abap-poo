*&---------------------------------------------------------------------*
*& Report ZR_POO_4_ATRIB_METODO_CLASSE
*&---------------------------------------------------------------------*
*& Atributos e Métodos de instância e atributos e métodos de Classes
*
* ATRIBUTOS DE CLASSE (static)
*   - Declaração de atributos de classe: CLASS-DATA
*   - Declaração de método de classe: CLASS-METHODS
*
*   ---> Na implementação não há diferença na declaração
*   Obs.: Boa prática iniciar com underscore: _variavel
*
*&---------------------------------------------------------------------*
REPORT zr_poo_4_atrib_metodo_classe NO STANDARD PAGE HEADING.

" Definição da classe
CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.

    CLASS-DATA:     " Atributo de classe (static)
      _instancia TYPE REF TO lcl_locutor.

    CLASS-METHODS:  " Método de classe (static)
      _gerar_instancia RETURNING VALUE(_instancia) TYPE REF TO lcl_locutor.

    DATA:           " Atributo de instância
      nome TYPE string.

    METHODS:        " Método de instância
      falar IMPORTING iv_mensagem TYPE string.
ENDCLASS.


" Implementação da classe (métodos)
CLASS lcl_locutor IMPLEMENTATION.
  METHOD: falar.
  ENDMETHOD.

  METHOD: _gerar_instancia.   " Declaração igual a qualquer outro método
  ENDMETHOD.
ENDCLASS.
