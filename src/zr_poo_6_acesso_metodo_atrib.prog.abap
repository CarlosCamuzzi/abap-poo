*&---------------------------------------------------------------------*
*& Report ZR_POO_6_ACESSO_METODO_ATRIB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*
* ACESSANDO MÉTODOS E ATRIBUTOS
*
* Métodos/Atributos de Instância
*   - nome_objeto->método_ou_atributo
*
* Métodos/Atributos Estáticos
*   - nome_classe=>método_ou_atributo
*&---------------------------------------------------------------------*

REPORT zr_poo_6_acesso_metodo_atrib.


CLASS lcl_dominio DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome  TYPE dd01l-domname,   " Será atribuído no construtor
      tab_a TYPE TABLE OF dd07v,
      tab_b TYPE TABLE OF DD07v.

    CLASS-METHODS:  " static
      class_constructor,

      " Recebe o nome e retorna uma instância do domínio (por valor)
      " É uma forma de automatizar a criação do objeto
      _gerar_dominio IMPORTING iv_nome          TYPE dd01l-domname
                     RETURNING VALUE(instancia) TYPE REF TO lcl_dominio.

    METHODS:
      " Construtor recebe um nome, de mesmo tipo do atributo
      constructor IMPORTING iv_nome TYPE dd01l-domname,
      "resgatar_valores,
      listar_valores.

ENDCLASS.
**********************************************************************

START-OF-SELECTION.

  DATA: ls_sflight       TYPE sflight,    " Apenas para exemplo
        lo_dominio_xfeld TYPE REF TO lcl_dominio.

**********************************************************************
  " Para acessar os campos do TYPE sflight usa o '-'
  "ls_sflight-carrid =

  " Para acesar os atributos e métodos estáticos de uma classe usa o '=>'
  " Para acesar os atributos e métodos de instância de uma classe usa o '->'
**********************************************************************

  " Acessando método estático para gerar uma nova instância
  " Forma antiga
*  CALL METHOD lcl_dominio=>_gerar_dominio
*    EXPORTING
*      iv_nome   = 'XFELD'
*    RECEIVING
*      instancia = lo_dominio_xfeld.

  " Forma nova
  " Aceita retorno, pois definimos RETURNING na classe
  lo_dominio_xfeld = lcl_dominio=>_gerar_dominio( iv_nome = 'XFELD' ).

  WRITE:/ 'O domínio é: ', lo_dominio_xfeld->nome.

**********************************************************************
CLASS lcl_dominio IMPLEMENTATION.
  METHOD: constructor.  " Construtor de instância
    nome = iv_nome.

    " Chamndo método
    " resgatar_valores( ).
  ENDMETHOD.

  METHOD: class_constructor.
    WRITE:/ 'Construtor de classe iniciado' .
  ENDMETHOD.

  METHOD: _gerar_dominio.
    " iv_nome para iv_nome: o que está à direita do igual (=) é o parâmetro que vem lá do chamador
    " O que está antes do igual (=) é o parâmetro iv_nome que está no IMPORT na definição do método

    " Forma antiga para criar objeto
    CREATE OBJECT instancia " instancia será retornada
      EXPORTING
        iv_nome = iv_nome.

    " Forma nova para criar objeto
    "instancia = new lcl_dominio( iv_nome = iv_nome ).  " instancia será retornada

  ENDMETHOD.

  METHOD: listar_valores.

    " LIKE LINE OF é usado para declarar uma variável que tenha o mesmo tipo de linha de uma tabela interna.
    " ls = line structure
    DATA: ls_data_a LIKE LINE OF tab_a.

    LOOP AT tab_a INTO ls_data_a.
      WRITE:/ ls_data_a.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
