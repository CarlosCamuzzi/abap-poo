*&---------------------------------------------------------------------*
*& Report ZR_POO_EX3_VENDAS
*&---------------------------------------------------------------------*
"Crie duas classes relacionadas:

"ZCL_PRODUCT:
"- Atributos: código, nome, preço, quantidade em estoque
"- Métodos para atualizar estoque e preço

"ZCL_SALE:
"- Deve usar a classe ZCL_PRODUCT
"- Implementar carrinho de compras
"- Calcular total com desconto
"- Verificar disponibilidade em estoque

" Obs.:
* - Quando você usa WITH DEFAULT KEY:
*       - A tabela usará como chave todos os campos não numéricos (exceto os de tipo string) que estão na estrutura/linha da tabela
*       - Se não houver campos não numéricos, a tabela será tratada como se não tivesse chave
*
" - Podemos especificar campos específicos como chave
*     WITH KEY id nome
*
*   "Sem chave nenhuma
*     WITH EMPTY KEY
*
*   "Definir chave única
*     WITH UNIQUE KEY id
*&---------------------------------------------------------------------*

REPORT zr_poo_ex3_vendas NO STANDARD PAGE HEADING.

CLASS lcl_product DEFINITION.

  PUBLIC SECTION.
    TYPES: ty_price   TYPE p LENGTH 13 DECIMALS 2,
           ty_code    TYPE c LENGTH 4,
           ty_product TYPE TABLE OF REF TO lcl_product.

    CLASS-DATA:
      _instance TYPE REF TO lcl_product.

    CLASS-METHODS:
      _create_instance IMPORTING iv_code          TYPE ty_code
                                 iv_name          TYPE string
                                 iv_stock         TYPE i
                                 iv_price         TYPE ty_price
                       RETURNING VALUE(_instance) TYPE REF TO lcl_product,

      _display_products IMPORTING lt_product TYPE ty_product.

    METHODS:
      constructor IMPORTING iv_code  TYPE ty_code
                            iv_name  TYPE string
                            iv_stock TYPE i
                            iv_price TYPE ty_price,

      get_code RETURNING VALUE(return) TYPE ty_code,    " ID: Get only

      get_name RETURNING VALUE(return) TYPE string,     " NAME: Get only

      set_stock IMPORTING iv_stock TYPE i,
      get_stock RETURNING VALUE(return) TYPE i,

      set_price IMPORTING iv_price TYPE ty_price,
      get_price RETURNING VALUE(return) TYPE ty_price.

  PRIVATE SECTION.
    DATA: code  TYPE ty_code,
          name  TYPE string,
          stock TYPE i,
          price TYPE ty_price.

ENDCLASS.

**********************************************************************
CLASS lcl_sale DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_discount   TYPE p LENGTH 13 DECIMALS 2,
           ty_cart_table TYPE STANDARD TABLE OF REF TO lcl_product WITH EMPTY KEY.

    CLASS-DATA: _instance TYPE REF TO lcl_sale.

    CLASS-METHODS:
      _create_instance IMPORTING it_cart          TYPE ty_cart_table
                       RETURNING VALUE(_instance) TYPE REF TO lcl_sale.

    METHODS:
      constructor IMPORTING it_cart TYPE ty_cart_table,

      set_cart IMPORTING it_cart TYPE ty_cart_table,
      get_cart RETURNING VALUE(return) TYPE ty_cart_table,

      " Discount applied on total cart amount
      set_discount IMPORTING iv_discount TYPE ty_discount,
      get_discount RETURNING VALUE(return) TYPE ty_discount,

      get_stock IMPORTING iv_stock TYPE abap_bool.

  PRIVATE SECTION.
    " The cart is a table of products
    DATA: cart     TYPE TABLE OF REF TO lcl_product,
          discount TYPE ty_discount,
          stock    TYPE abap_bool.
ENDCLASS.
**********************************************************************


**********************************************************************

START-OF-SELECTION.

  " PRODUCT  --------------------------------------------------------------------
  DATA: lt_products TYPE TABLE OF REF TO lcl_product,
        lr_product  TYPE REF TO lcl_product.

  lr_product = lcl_product=>_create_instance( iv_code = '0001'
                                                 iv_name = 'Filé de Lombo KG'
                                                 iv_stock = '10'
                                                 iv_price = '49.90' ).
  APPEND lr_product TO lt_products.
  FREE lr_product.

  lr_product = lcl_product=>_create_instance( iv_code = '0002'
                                                  iv_name = 'Alcatra KG'
                                                  iv_stock = '20'
                                                  iv_price = '59.90' ).
  APPEND lr_product TO lt_products.
  FREE lr_product.

  lcl_product=>_display_products( lt_products ).

**********************************************************************

  " SALE
  "DATA(lo_sale) = lcl_sale=>_create_instance().


**********************************************************************
CLASS lcl_product IMPLEMENTATION.
  METHOD: constructor.
    me->code = iv_code.
    me->name = iv_name.
    me->stock = iv_stock.
    me->price = iv_price.
  ENDMETHOD.

  METHOD: _create_instance.
    CREATE OBJECT _instance
      EXPORTING
        iv_code  = iv_code
        iv_name  = iv_name
        iv_stock = iv_stock
        iv_price = iv_price.
  ENDMETHOD.

  METHOD: _display_products.
    LOOP AT lt_product INTO DATA(lr_product).
      WRITE:/ 'Código:', lr_product->get_code( ),
            / 'Produto:', lr_product->get_name( ),
            / 'Estoque:', lr_product->get_stock( ),
            / 'Preço:', lr_product->get_price( ).
      SKIP.
    ENDLOOP.
  ENDMETHOD.

  "$. Region getters and setters
  METHOD: get_code.
    return = me->code.
  ENDMETHOD.

  METHOD: get_name.
    return = me->name.
  ENDMETHOD.

  METHOD: set_stock.
    me->stock = iv_stock.
  ENDMETHOD.

  METHOD: get_stock.
    return = me->stock.
  ENDMETHOD.

  METHOD: set_price.
    me->price = iv_price.
  ENDMETHOD.

  METHOD: get_price.
    return = me->price.
  ENDMETHOD.
  "$. Endregion getters and setters
ENDCLASS.


**********************************************************************
CLASS lcl_sale IMPLEMENTATION.
  METHOD: constructor.
    me->cart = it_cart.
  ENDMETHOD.

  METHOD: _create_instance.
    _instance = NEW lcl_sale( it_cart = it_cart ).
  ENDMETHOD.

  "$. Region getters and setters
  METHOD: set_cart.
    me->cart = it_cart.
  ENDMETHOD.

  METHOD: get_cart.
    return = me->cart.
  ENDMETHOD.

  METHOD: set_discount.
    me->discount = iv_discount.
  ENDMETHOD.

  METHOD: get_discount.
    return = me->discount.
  ENDMETHOD.

  METHOD: get_stock.
    me->stock = iv_stock.
  ENDMETHOD.
  "$. Endregion getters and setters
ENDCLASS.
