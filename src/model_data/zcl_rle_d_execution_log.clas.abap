CLASS zcl_rle_d_execution_log DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.
    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
    DATA mo_master_data TYPE REF TO zcl_rle_master_data.
    METHODS descriptions
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_d_execution_log IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.
    CASE is_ctx-det_key.
      WHEN zif_rle_bo_process_c=>sc_determination-execution_log-descriptions.
        descriptions(
                EXPORTING
                  is_ctx        = is_ctx
                  it_key        = it_key
                  io_read       = io_read
                  io_modify     = io_modify ).
    ENDCASE.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mo_master_data = NEW #(  ).

  ENDMETHOD.

  METHOD descriptions.

    DATA lt_data TYPE zrle_bo_i_execution_log.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.


      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        "lo_data->decision_taken_desc = mo_master_data->get_decision_desc( lo_data-decision_taken ).
        lo_data->rule_name_desc = mo_master_data->get_rule_desc( lo_data->rule_name ).
        lo_data->next_rule_name_desc = mo_master_data->get_rule_desc( lo_data->next_rule_name ).
        lo_data->decision_taken_desc = mo_master_data->get_decision_desc( lo_data->decision_taken ).

        io_modify->update( iv_node = is_ctx-node_key
                                        iv_key  = lo_data->key
                                        is_data = lo_data ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
