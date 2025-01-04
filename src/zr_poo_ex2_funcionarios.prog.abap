*&---------------------------------------------------------------------*
*& Report ZR_POO_EX2_FUNCIONARIOS
*&---------------------------------------------------------------------*
" Crie uma classe ZCL_EMPLOYEE com:
"   - Atributos privados: ID, nome, salário, departamento
"   - Métodos públicos para:
"    - Criar novo funcionário
"    - Calcular aumento de salário baseado em porcentagem
"    - Transferir funcionário de departamento
"    - Exibir dados do funcionário
*&---------------------------------------------------------------------*

REPORT zr_poo_ex2_funcionarios NO STANDARD PAGE HEADING.

CLASS lcl_employee DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_salary TYPE p LENGTH 13 DECIMALS 2,
           ty_id     TYPE c LENGTH 4.

    CLASS-DATA:
      _instance TYPE REF TO lcl_employee.

    CLASS-METHODS:
      _create_instance IMPORTING iv_id            TYPE ty_id
                                 iv_name          TYPE string
                                 iv_salary        TYPE ty_salary
                                 iv_department    TYPE string
                       RETURNING VALUE(_instance) TYPE REF TO lcl_employee,

      _display_employee_data IMPORTING lo_employee TYPE REF TO lcl_employee.

    METHODS:
      constructor IMPORTING iv_id         TYPE ty_id
                            iv_name       TYPE string
                            iv_salary     TYPE ty_salary
                            iv_department TYPE string,

      get_id RETURNING VALUE(return) TYPE ty_id,          " Get only

      set_name IMPORTING iv_name TYPE string,
      get_name RETURNING VALUE(return) TYPE string,

      get_salary RETURNING VALUE(return) TYPE ty_salary,  " Get only

      set_department IMPORTING iv_department TYPE string,
      get_department RETURNING VALUE(return) TYPE string,

      change_salary_by_porcentage IMPORTING iv_porcentage TYPE p
                                  RETURNING VALUE(return) TYPE ty_salary.


  PRIVATE SECTION.
    DATA: id(4)      TYPE c,
          name       TYPE string,
          salary     TYPE ty_salary,
          department TYPE string.

ENDCLASS.

**********************************************************************

START-OF-SELECTION.

  DATA(lo_employee_n1) = NEW lcl_employee( iv_id = '0001'
                                           iv_name = 'Carl'
                                           iv_salary = 1000
                                           iv_department = 'Human Resources'
  ).

  lcl_employee=>_display_employee_data( lo_employee_n1 ).

  " Change Name by set_name -------------------------------------------
  lo_employee_n1->set_name( 'Carl Sagan' ).
  lcl_employee=>_display_employee_data( lo_employee_n1 ).

  " Change salary by method -------------------------------------------
  DATA(v_nsalary) = lo_employee_n1->change_salary_by_porcentage( 10 ).  " Get return from method
  lcl_employee=>_display_employee_data( lo_employee_n1 ).

  WRITE:/ 'Name: ', lo_employee_n1->get_name( ),
        / 'New Salary', lo_employee_n1->get_salary( ).
  SKIP.

  " Change department ------------------------------------------------
  lo_employee_n1->set_department( 'Astronomy Sector' ).
  lcl_employee=>_display_employee_data( lo_employee_n1 ).

**********************************************************************

CLASS lcl_employee IMPLEMENTATION.

  METHOD: constructor.
    me->id = iv_id.
    me->name = iv_name.
    me->salary = iv_salary.
    me->department = iv_department.
  ENDMETHOD.

  METHOD: _create_instance.
    DATA instance TYPE REF TO lcl_employee.

    instance = NEW lcl_employee( iv_id   = iv_id
                                 iv_name = iv_name
                                 iv_salary = iv_salary
                                 iv_department = iv_department ).
  ENDMETHOD.

  METHOD: _display_employee_data.

    " Check if the instance object exists
    IF lo_employee IS NOT BOUND.
      WRITE: 'No employee'.
      RETURN.
    ENDIF.

    WRITE:/ 'ID:', lo_employee->get_id( ),
          /  'Name:', lo_employee->get_name( ),
          /  'Salary:', lo_employee->get_salary( ),
          /  'Department:', lo_employee->get_department( ).
    SKIP.

  ENDMETHOD.

  METHOD: change_salary_by_porcentage.
    return = me->salary = me->salary + ( me->salary * ( iv_porcentage / 100 ) ).
  ENDMETHOD.

  "$. Region getters and setters
  METHOD: get_id.
    return = me->id.
  ENDMETHOD.

  METHOD: set_name.
    me->name = iv_name.
  ENDMETHOD.

  METHOD: get_name.
    return = me->name.
  ENDMETHOD.

  METHOD: get_salary.
    return = me->salary.
  ENDMETHOD.

  METHOD: set_department.
    me->department = iv_department.
  ENDMETHOD.

  METHOD: get_department.
    return = me->department.
  ENDMETHOD.
  "$. Endregion getters and setters

ENDCLASS.
