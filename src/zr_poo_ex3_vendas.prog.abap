*&---------------------------------------------------------------------*
*& Report ZR_POO_EX3_VENDAS
*&---------------------------------------------------------------------*

**********************************************************************

"Crie duas classes relacionadas:

"ZCL_PRODUCT:
"- Atributos: código, nome, preço, quantidade em estoque
"- Métodos para atualizar estoque e preço

"ZCL_SALE:
"- Deve usar a classe ZCL_PRODUCT
"- Implementar carrinho de compras
"- Calcular total com desconto (se total carrinho > 100, então 10% desconto)
"- Verificar disponibilidade em estoque

**********************************************************************

" ANOTAÇÕES:
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

**********************************************************************

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
                       RETURNING VALUE(_instance) TYPE REF TO lcl_product.

    METHODS:
      constructor IMPORTING iv_code  TYPE ty_code
                            iv_name  TYPE string
                            iv_stock TYPE i
                            iv_price TYPE ty_price,

      display_product,

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
           ty_cart_table TYPE TABLE OF REF TO lcl_product WITH EMPTY KEY.

    CLASS-DATA: _instance TYPE REF TO lcl_sale.

    CLASS-METHODS:
      _create_instance IMPORTING it_cart          TYPE ty_cart_table
                       RETURNING VALUE(_instance) TYPE REF TO lcl_sale.

    METHODS:
      constructor IMPORTING it_cart TYPE ty_cart_table,

      display_cart,

      set_cart IMPORTING it_cart TYPE ty_cart_table,
      get_cart RETURNING VALUE(return) TYPE ty_cart_table,

      " Discount applied on total cart amount
      " get_discount return cart total with discount
      set_discount IMPORTING iv_discount TYPE ty_discount,
      get_discount RETURNING VALUE(return) TYPE ty_discount,

      get_stock RETURNING VALUE(return) TYPE ty_cart_table.

  PRIVATE SECTION.
    " The cart is a table of products
    DATA: cart     TYPE ty_cart_table,
          discount TYPE ty_discount,
          stock    TYPE abap_bool.
ENDCLASS.
**********************************************************************


**********************************************************************

START-OF-SELECTION.

  " PRODUCT  --------------------------------------------------------------------
  DATA: lt_products TYPE TABLE OF REF TO lcl_product WITH EMPTY KEY,
        lr_product  TYPE REF TO lcl_product.

  lr_product = lcl_product=>_create_instance( iv_code = '0001'
                                              iv_name = 'Filé de Lombo KG'
                                              iv_stock = '10'
                                              iv_price = '49.90' ).
  APPEND lr_product TO lt_products.
  FREE lr_product.

  lr_product = lcl_product=>_create_instance( iv_code = '0002'
                                              iv_name = 'Alcatra KG'
                                              iv_stock = '50'
                                              iv_price = '59.90' ).
  "lr_product->display_product( ).   " Display product
  APPEND lr_product TO lt_products.
  FREE lr_product.

  lr_product = lcl_product=>_create_instance( iv_code = '0003'
                                              iv_name = 'Joelho Suíno KG'
                                              iv_stock = '0'
                                              iv_price = '9.90' ).
  APPEND lr_product TO lt_products.
  FREE lr_product.

**********************************************************************

  " SALE
  DATA: lo_cart   TYPE lcl_sale=>ty_cart_table,
        lt_filter TYPE lcl_sale=>ty_cart_table.

  DATA(lo_sale) = lcl_sale=>_create_instance( lt_products ).
  lo_sale->display_cart( ).
  lo_sale->set_discount( 10 ).  " 10%


  " loop in products with stock
  lt_filter = lo_sale->get_stock( ).
  LOOP AT lt_filter INTO DATA(ls_filter).
    WRITE:/ ls_filter->get_name( ).
  ENDLOOP.

 " Total cart value with discount
  DATA(lv_discount) = lo_sale->get_discount( ).
  WRITE:/ lv_discount.

  SKIP.
  WRITE:/ 'Fim do processamento'.

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

  METHOD: display_product.
    WRITE:/ 'Código:', me->get_code( ),
          / 'Produto:', me->get_name( ),
          / 'Estoque:', me->get_stock( ),
          / 'Preço:', me->get_price( ).
    SKIP.
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

  METHOD: display_cart.
    " ty_cart_table TYPE TABLE OF REF TO lcl_product WITH EMPTY KEY.
    DATA: lo_cart TYPE ty_cart_table.

    " Get cart products
    lo_cart = me->get_cart( ).

    LOOP AT lo_cart INTO DATA(lo_product).
      WRITE:/ 'Código:', lo_product->get_code( ),
            / 'Produto:', lo_product->get_name( ),
            / 'Estoque:', lo_product->get_stock( ),
            / 'Preço:', lo_product->get_price( ).
      SKIP.
    ENDLOOP.
  ENDMETHOD.

  "$. Region getters and setters
  METHOD: set_cart.
    me->cart = it_cart.
  ENDMETHOD.

  METHOD: get_cart.
    return = me->cart.
  ENDMETHOD.

  METHOD: set_discount.
    DATA: lo_cart  TYPE ty_cart_table,
          lv_total TYPE p LENGTH 13 DECIMALS 2.

    lo_cart = me->get_cart( ).

    lv_total = REDUCE #(
      INIT total = CONV #( '0.00' )
      FOR  ls_product IN lo_cart
      "NEXT total = total + ls_product->get_price( )
        NEXT total = COND #(
          WHEN ls_product->get_stock( ) > 0     " only products with stock > 0
          THEN total + ls_product->get_price( )
          ELSE total
        )
    ).

    IF lv_total > 100.
      me->discount = lv_total - ( lv_total * ( iv_discount / 100 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD: get_discount.
    return = me->discount.
  ENDMETHOD.

  METHOD: get_stock.
    DATA: lo_cart   TYPE ty_cart_table,
          lt_filter TYPE ty_cart_table.

    lo_cart = me->get_cart( ).

    LOOP AT lo_cart INTO DATA(ls_product).
      IF ls_product->get_stock( ) > 0.
        APPEND ls_product TO lt_filter.
      ENDIF.
    ENDLOOP.

    return = lt_filter.

  ENDMETHOD.
  "$. Endregion getters and setters
ENDCLASS.
