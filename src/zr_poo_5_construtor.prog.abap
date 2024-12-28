*&---------------------------------------------------------------------*
*& Report ZR_POO_5_CONSTRUTOR
*&---------------------------------------------------------------------*
*
*  CONSTRUTORES
*   - Nesse exemplo estamos gerando uma instância de domínio.
*   - É obrigatório que domínio tenha um nome
*   - Nome será atribuído no construtor da class
*   - É possível usar o exporting, importing, exceptions
*
*  CONSTRUTOR DE CLASSE (static)
*   - Declarado em CLASS-METHODS: class_constructor.
*   - Não é possível importar ou exportar valores
*   - Esse construtor é sempre acessado na primera vez que o objeto é instanciado
*
*&---------------------------------------------------------------------*

REPORT zr_poo_5_construtor.

CLASS lcl_dominio DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome  TYPE dd01l-domname,   " Será atribuído no construtor
      tab_a TYPE TABLE OF dd07v,
      tab_b TYPE TABLE OF DD07v.

    CLASS-METHODS:
      class_constructor.   " Sempre será executado primeiro

    METHODS:
      " Construtor recebe um nome, de mesmo tipo do atributo
      constructor IMPORTING iv_nome TYPE dd01l-domname,
      resgatar_valores.
ENDCLASS.
**********************************************************************

START-OF-SELECTION.
  DATA: lo_dominio_xfeld TYPE REF TO lcl_dominio.

  "Dará erro, pois o constructor exige o parâmetro 'nome'
  " CREATE OBJECT lo_dominio_xfeld.

  CREATE OBJECT lo_dominio_xfeld
    EXPORTING
      iv_nome = 'xfeld'.  " Nome do dominio


**********************************************************************
CLASS lcl_dominio IMPLEMENTATION.
  METHOD: constructor.
    nome = iv_nome.   " Construtor fazendo a atribuição do atributo
    WRITE:/ 'O domínio é ', nome.
  ENDMETHOD.

  METHOD: class_constructor.
    WRITE:/ 'Construtor de classe é iniciado primeiro'  .
  ENDMETHOD.

  METHOD: resgatar_valores.
    CALL FUNCTION 'DD_DOMA_GET' " Método para criar o dominio
      EXPORTING
        domain_name   = nome
      TABLES
        dd07v_tab_a   = tab_a
        dd07v_tab_n   = tab_b
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.
ENDCLASS.
