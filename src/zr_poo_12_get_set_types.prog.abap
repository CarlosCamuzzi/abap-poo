*&---------------------------------------------------------------------*
*& Report ZR_POO_12_GET_SET_TYPES
*&---------------------------------------------------------------------*

*  Os TYPES em ABAP não são associados a instâncias; são apenas definições de tipos que existem no escopo da classe.
*   Assim, podem ser acessados diretamente pela classe, de forma semelhante a como atributos ou métodos estáticos funcionam.
*
* Características de um TYPES declarado em uma classe:
*   Acessível de forma estática:
*   - O tipo pode ser acessado diretamente pelo nome da classe, como classe=>nome_do_tipo, sem precisar criar uma instância da classe.
*
*   Escopo limitado à classe:
*   - O tipo é visível apenas dentro da classe onde foi declarado, a menos que esteja declarado como PUBLIC.
*Nesse caso, outras classes ou programas podem acessá-lo com classe=>nome_do_tipo.
*
*   Não é herdável:
*   - Tipos declarados com TYPES em uma classe não são herdados pelas subclasses. Se a subclasse precisar de um tipo semelhante, será necessário declará-lo novamente.
*

* A palavra-chave LINE OF indica que queremos que a variável tenha exatamente a mesma estrutura de uma linha individual da tabela interna.

*&---------------------------------------------------------------------*

REPORT zr_poo_12_get_set_types.

TABLES: makt. " Material Description

**********************************************************************
* DEFINIÇÃO DEFERIDA
**********************************************************************
CLASS lcl_material DEFINITION DEFERRED.


**********************************************************************
* DEFINIÇÕES DE CLASSE
**********************************************************************
CLASS lcl_descricao_material DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Esse idioma será adicionado pelo método adicionar_descrições no objeto material
      " Esse método criará uma instancia dessa classe lcl_material, fará a atribuição do objet material e do idioma
      constructor IMPORTING io_material TYPE REF TO lcl_material  " Objeto material
                            iv_idioma   TYPE sy-langu,

      set_idioma IMPORTING iv_idioma     TYPE sy-langu,
      get_idioma RETURNING VALUE(return) TYPE sy-langu,

      set_material IMPORTING iv_material   TYPE REF TO lcl_material,
      get_material RETURNING VALUE(return) TYPE REF TO lcl_material,

      set_descricao IMPORTING iv_descricao  TYPE makt-maktx,
      get_descricao RETURNING VALUE(return) TYPE makt-maktx.

  PRIVATE SECTION.
    DATA:
      idioma    TYPE sy-langu,
      material  TYPE REF TO lcl_material,
      descricao TYPE makt-maktx.
ENDCLASS.

CLASS lcl_material DEFINITION.
  PUBLIC SECTION.
*  WITH NON-UNIQUE KEY table_line:
*  Define a chave da tabela como sendo a linha inteira (table_line).
*  NON-UNIQUE: Permite que a chave não seja única, ou seja, linhas duplicadas são permitidas.
    TYPES: t_descricoes TYPE TABLE OF REF TO lcl_descricao_material WITH NON-UNIQUE KEY table_line.

    METHODS:
      constructor IMPORTING iv_material TYPE matnr, " Número de material

      set_matnr IMPORTING iv_matnr      TYPE matnr,
      get_matnr RETURNING VALUE(return) TYPE matnr,

      adicionar_descricao IMPORTING iv_idioma TYPE sy-langu,

      set_descricoes IMPORTING iv_descricoes TYPE t_descricoes,
      get_descricoes RETURNING VALUE(return) TYPE t_descricoes.

  PRIVATE SECTION.
    DATA:
      matnr      TYPE matnr,
      descricoes TYPE TABLE OF REF TO lcl_descricao_material.
ENDCLASS.

CLASS lcl_programa DEFINITION.
  PUBLIC SECTION.
    TYPES: r_langu TYPE RANGE OF sy-langu.

    CLASS-METHODS:
      _start_of_selection IMPORTING iv_material TYPE matnr
                                    ir_langu    TYPE r_langu.   " Range de sy-langu
ENDCLASS.



**********************************************************************
* TELA DE SELEÇÃO
**********************************************************************
" matnr: Cod do material. Está na classe lcl_material
PARAMETERS: p_matnr TYPE matnr DEFAULT '100-431'.

" makt-spras: Idioma. Está na classe lcl_material e na classe lcl_descricao_material
" Essa tabela tem o campos spras do idiom.
" iv_idioma TYPE sy-langu
SELECT-OPTIONS s_langu FOR makt-spras NO INTERVALS DEFAULT: 'PT' OPTION EQ SIGN I.  " Range

*NO INTERVALS:
*
*Restringe a entrada no SELECT-OPTIONS para não permitir intervalos (ranges). O usuário só poderá inserir valores individuais.
*DEFAULT: 'PT' OPTION EQ SIGN I.:
*
*Define um valor padrão na seleção, com as seguintes especificações:
*DEFAULT: 'PT': Preenche o valor padrão com 'PT', que provavelmente representa o idioma Português.
*OPTION EQ: Define a operação como =, ou seja, "igual a".
*SIGN I: Indica que a condição faz parte da inclusão (incluir os valores especificados). Outros valores poderiam ser SIGN E (excluir).


**********************************************************************
* INÍCIO DA APLICAÇÃO
**********************************************************************
START-OF-SELECTION.
  " r_langu é um range type sy-langu
  " ir_langu é um range type r_langu
  " s_langu é um range type mkt-spras (idioma)
  lcl_programa=>_start_of_selection( iv_material = p_matnr
                                     ir_langu    = s_langu[] ).



**********************************************************************
* IMPLEMENTAÇÃO DE CLASSE
**********************************************************************
CLASS lcl_programa IMPLEMENTATION.
  METHOD _start_of_selection.

    DATA: lo_material   TYPE REF TO lcl_material,           " Objeto material
          lo_descricao  TYPE REF TO lcl_descricao_material, " Objeto descrições de material
          lt_descricoes TYPE lcl_material=>t_descricoes,    " Tabela interna do type t_descricao (que referencia o type estático da classe lcl_material)
          " Especifica que o tipo de ls_langu será baseado no tipo de uma linha da tabela interna ir_langu.
          ls_langu      LIKE LINE OF ir_langu.              " Tabela type ir_langu - sy-langu

    "Cria o material
    CREATE OBJECT lo_material
      EXPORTING
        iv_material = iv_material. " Número de material

    " Cria as descrições
    " Loop na tabela transparente, incluindo o valor no obejto de  descrição do material
    LOOP AT ir_langu INTO ls_langu.   " Idiomas
      lo_material->adicionar_descricao( ls_langu-low ). " Ver esse LOW
    ENDLOOP.

    "Escreve o Material em tela
    WRITE:/ 'Material:', lo_material->get_matnr( ). " Número do material

    "Escreve as descrições em tela
    " Como pode ter mais de uma descrição (idioma), faz o loop
    " Aqui ele faz o loop pegando as descriçõs que foram adicionadas no loop acima
    lt_descricoes = lo_material->get_descricoes( ).
    LOOP AT lt_descricoes INTO lo_descricao.
      WRITE:/ lo_descricao->get_idioma( ), lo_descricao->get_descricao( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_material IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_matnr.
    me->matnr = iv_matnr.
  ENDMETHOD.
  METHOD get_matnr.
    return = me->matnr.
  ENDMETHOD.

  METHOD set_descricoes.
    me->descricoes = iv_descricoes.
  ENDMETHOD.
  METHOD get_descricoes.
    return = me->descricoes.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD constructor.
    me->matnr = iv_material.
  ENDMETHOD.

  METHOD adicionar_descricao.
    " Instanciando o objeto de descrições
    DATA: lo_descricao TYPE REF TO lcl_descricao_material.

    " No construtor da descrição, o mesmo recebe uma referência de material.
    " O 'me' indica a referência do atual material que está criando essa descrição
    CREATE OBJECT lo_descricao
      EXPORTING
        iv_idioma   = iv_idioma
        io_material = me.       " Referencia do material atual

    " Inclui o objeto lo_descrição na descrição da instancia do material
    APPEND lo_descricao TO me->descricoes.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_descricao_material IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_idioma.
    me->idioma = iv_idioma.
  ENDMETHOD.
  METHOD get_idioma.
    return = me->idioma.
  ENDMETHOD.

  METHOD set_material.
    me->material = iv_material.
  ENDMETHOD.
  METHOD get_material.
    return = me->material.
  ENDMETHOD.

  METHOD set_descricao.
    me->descricao = iv_descricao.
  ENDMETHOD.
  METHOD get_descricao.
    return = me->descricao.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD constructor. " lcl_descricao_material
    DATA: lv_matnr TYPE matnr.

    me->idioma   = iv_idioma.
    me->material = io_material.

    lv_matnr = me->material->get_matnr( ).

    "  Salva a descrição do material conforme o parameter e o select option passado
    SELECT SINGLE maktx
      INTO me->descricao
      FROM makt
     WHERE matnr = lv_matnr
       AND spras = me->idioma.
  ENDMETHOD.
ENDCLASS.
