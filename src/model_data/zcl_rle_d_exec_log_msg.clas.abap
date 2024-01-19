CLASS zcl_rle_d_exec_log_msg DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS message
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_d_exec_log_msg IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.
    CASE is_ctx-det_key.
      WHEN zif_rle_bo_process_c=>sc_determination-execution_log_msg-message.
        message( EXPORTING is_ctx        = is_ctx
                           it_key        = it_key
                           io_read       = io_read
                           io_modify     = io_modify ).
    ENDCASE.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.

  METHOD message.
    DATA lt_data TYPE zrle_bo_i_exec_log_msg.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.


      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        lo_data->message = zcl_ca_utilities=>fill_return( EXPORTING iv_type       = lo_data->msgty
                                                                iv_id         = lo_data->msgid
                                                                iv_number     = lo_data->msgno
                                                                iv_message_v1 = lo_data->msgv1
                                                                iv_message_v2 = lo_data->msgv2
                                                                iv_message_v3 = lo_data->msgv3
                                                                iv_message_v4 = lo_data->msgv4 )-message.

        io_modify->update( iv_node = is_ctx-node_key
                                        iv_key  = lo_data->key
                                        is_data = lo_data ).

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
