*&---------------------------------------------------------------------*
*& Report ZR_POO_EX1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZR_POO_EX1.


CLASS lcl_motorista DEFINITION.
  PUBLIC SECTION.
    DATA: motorista_id TYPE zemoto_001,
          nome         TYPE zemoto_002,
          cpf          TYPE zemoto_003,
          cnh          TYPE zemoto_004,
          validade_cnh TYPE erdat,
          status       TYPE zemoto_005.

    CLASS-METHODS:
      _gerar_motorista IMPORTING iv_motorista_id  TYPE zemoto_001
                                 iv_nome          TYPE zemoto_002
                                 iv_cpf           TYPE zemoto_003
                                 iv_cnh           TYPE zemoto_004
                                 iv_validade_cnh  TYPE erdat
                                 iv_status        TYPE zemoto_005
                       RETURNING VALUE(instancia) TYPE REF TO lcl_motorista.

    METHODS:
      constructor IMPORTING iv_motorista_id TYPE zemoto_001
                            iv_nome         TYPE zemoto_002
                            iv_cpf          TYPE zemoto_003
                            iv_cnh          TYPE zemoto_004
                            iv_validade_cnh TYPE erdat
                            iv_status       TYPE zemoto_005,

      apresentar_motorista IMPORTING io_motorista    TYPE REF TO lcl_motorista,
      salvar_motorista     IMPORTING io_motorista    TYPE REF TO lcl_motorista.

ENDCLASS.

**********************************************************************

START-OF-SELECTION.
  DATA: lo_motorista TYPE REF TO lcl_motorista.

" Instanciando forma normal
*  lo_motorista = NEW lcl_motorista( iv_motorista_id = '00003'
*                      iv_nome = 'Rafaela schutz'
*                      iv_cpf = '12655464247'
*                      iv_cnh = '123447779'
*                      iv_validade_cnh = sy-datum
*                      iv_status = 'Disponível' ).

  " Otimizando com gerador de instâncias
  lo_motorista = lcl_motorista=>_gerar_motorista( iv_motorista_id = '00003'
                                                  iv_nome = 'Rafaela schutz'
                                                  iv_cpf = '12655464247'
                                                  iv_cnh = '123447779'
                                                  iv_validade_cnh = sy-datum
                                                  iv_status = 'Disponível' ).

  lo_motorista->apresentar_motorista( lo_motorista ).
  lo_motorista->salvar_motorista( lo_motorista ).

**********************************************************************

CLASS lcl_motorista IMPLEMENTATION.
  METHOD: _gerar_motorista.
    instancia = NEW lcl_motorista( iv_motorista_id = iv_motorista_id
                                   iv_nome         = iv_nome
                                   iv_cpf          = iv_cpf
                                   iv_cnh          = iv_cnh
                                   iv_validade_cnh = iv_validade_cnh
                                   iv_status       = iv_status ).
  ENDMETHOD.

  METHOD: constructor.
    motorista_id = iv_motorista_id.
    nome         = iv_nome.
    cpf          = iv_cpf.
    cnh          = iv_cnh.
    validade_cnh = iv_validade_cnh.
    status       = iv_status.
  ENDMETHOD.

  METHOD: apresentar_motorista.
    WRITE:/ io_motorista->motorista_id, io_motorista->nome, io_motorista->cpf,
            io_motorista->cnh, io_motorista->validade_cnh, io_motorista->status.
  ENDMETHOD.

  METHOD: salvar_motorista.

    DATA: ls_motorista TYPE ztmotoristas.

    ls_motorista-motorista_id = io_motorista->motorista_id.
    ls_motorista-nome         = to_upper( io_motorista->nome ).
    ls_motorista-cpf          = io_motorista->cpf.
    ls_motorista-cnh          = io_motorista->cnh.
    ls_motorista-validade_cnh = io_motorista->validade_cnh.
    ls_motorista-status       = to_upper( io_motorista->status ).
    INSERT ztmotoristas FROM ls_motorista.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      MESSAGE 'Registro salvo' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Erro' TYPE 'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
