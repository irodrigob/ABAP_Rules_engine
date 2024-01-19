CLASS zcl_rle_bo_process_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_add_messages TYPE STANDARD TABLE OF zrle_bo_sp_exec_log_msg WITH DEFAULT KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Nuevo proceso</p>
    "! @parameter is_process | <p class="shorttext synchronized">Datos del proceso</p>
    "! @parameter et_return | <p class="shorttext synchronized">Posibles mensajes</p>
    "! @parameter es_data | <p class="shorttext synchronized">Datos del proceso creado</p>
    METHODS new_process
      IMPORTING is_process TYPE zrle_bo_sp_process
      EXPORTING et_return  TYPE zcl_rle_data=>tt_return
                es_data    TYPE zrle_bo_sc_process.
    "! <p class="shorttext synchronized">Nuevo ejecución del log</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Nodo cabecera</p>
    "! @parameter is_execution_log | <p class="shorttext synchronized">Datos de ejecución de log</p>
    "! @parameter et_return | <p class="shorttext synchronized">Posibles mensajes</p>
    "! @parameter es_data | <p class="shorttext synchronized">Datos del proceso creado</p>
    METHODS new_execution_log
      IMPORTING iv_root_key      TYPE /bobf/conf_key
                is_execution_log TYPE zrle_bo_sp_execution_log
      EXPORTING et_return        TYPE zcl_rle_data=>tt_return
                es_data          TYPE zrle_bo_sc_execution_log.
    "! <p class="shorttext synchronized">Edición ejecución del log</p>
    "! @parameter is_execution_log | <p class="shorttext synchronized">Datos de ejecución de log</p>
    "! @parameter et_return | <p class="shorttext synchronized">Posibles mensajes</p>
    "! @parameter es_data | <p class="shorttext synchronized">Datos del proceso creado</p>
    METHODS edit_execution_log
      IMPORTING is_execution_log TYPE zrle_bo_sc_execution_log
      EXPORTING et_return        TYPE zcl_rle_data=>tt_return
                es_data          TYPE zrle_bo_sc_execution_log.
    "! <p class="shorttext synchronized">Añadir mensajes a la ejecución del log</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Nodo root</p>
    "! @parameter iv_parent_key | <p class="shorttext synchronized">Nodo padre</p>
    "! @parameter it_messages | <p class="shorttext synchronized">Mensajes</p>
    "! @parameter et_return | <p class="shorttext synchronized">Posibles mensajes</p>
    "! @parameter es_data | <p class="shorttext synchronized">Datos del proceso creado</p>
    METHODS add_execution_log_messages
      IMPORTING iv_root_key   TYPE /bobf/conf_key
                iv_parent_key TYPE /bobf/conf_key
                it_messages   TYPE tt_add_messages
      EXPORTING et_return     TYPE zcl_rle_data=>tt_return
                et_data       TYPE zrle_bo_i_exec_log_msg.
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
    DATA mo_bopf_util TYPE REF TO zcl_ca_bopf_util.
    DATA mo_query TYPE REF TO zcl_rle_bo_process_query.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Inicialización variables para el BOPF</p>
    METHODS init_bopf.
ENDCLASS.



CLASS zcl_rle_bo_process_model IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

    mo_query = NEW #( ).

    init_bopf(  ).
  ENDMETHOD.

  METHOD init_bopf.
    " Instancia las variables para los BOPF
    TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_rle_bo_process_c=>sc_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( zif_rle_bo_process_c=>sc_bo_key ).

        " Clase con utilidades de BOPF
        mo_bopf_util = NEW zcl_ca_bopf_util( zif_rle_bo_process_c=>sc_bo_key ).

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.
  ENDMETHOD.

  METHOD new_process.

    CLEAR: et_return, es_data.

    DATA(lo_root) = NEW zrle_bo_sc_process(  ).
    ASSIGN lo_root->* TO FIELD-SYMBOL(<ls_root>).

    " Se pasan los campos de valores.
    <ls_root> = CORRESPONDING #( is_process ).

    " Clave del nodo
    lo_root->key = /bobf/cl_frw_factory=>get_new_key( ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification( ( node = zif_rle_bo_process_c=>sc_node-root
                    change_mode = /bobf/if_frw_c=>sc_modify_create
                    key = lo_root->key
                    data = lo_root ) ).

    mo_bopf_util->modify_save_data(
      EXPORTING
        it_mod      = lt_mod
        iv_langu    = mv_langu
      IMPORTING
        et_return = DATA(lt_return_modify) ).

    IF line_exists( lt_return_modify[ type = zcl_cf_data=>cs_msg-type_error ]  ).
      et_return = CORRESPONDING #( lt_return_modify ).
      mo_bopf_util->rollback(  ).
    ELSE.
      DATA(lt_process_created) = mo_query->get_process_header_data( iv_key = lo_root->key ).
      es_data = lt_process_created[ 1 ].
    ENDIF.

  ENDMETHOD.

  METHOD new_execution_log.

    CLEAR: et_return, es_data.

    DATA(lo_execution_log) = NEW zrle_bo_sc_execution_log( ).
    ASSIGN lo_execution_log->* TO FIELD-SYMBOL(<ls_execution_log>).

    <ls_execution_log> = CORRESPONDING #( is_execution_log ).
    lo_execution_log->parent_key = iv_root_key.
    lo_execution_log->key = /bobf/cl_frw_factory=>get_new_key( ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification( ( node = zif_rle_bo_process_c=>sc_node-execution_log
                    change_mode = /bobf/if_frw_c=>sc_modify_create
                    key = lo_execution_log->key
                    data = lo_execution_log
                     source_node = zif_rle_bo_process_c=>sc_node-root
                    association = zif_rle_bo_process_c=>sc_association-root-execution_log
                    source_key = iv_root_key ) ).

    mo_bopf_util->modify_save_data(
         EXPORTING
           it_mod      = lt_mod
           iv_langu    = mv_langu
         IMPORTING
           et_return = DATA(lt_return_modify) ).

    IF line_exists( lt_return_modify[ type = zcl_cf_data=>cs_msg-type_error ]  ).
      et_return = CORRESPONDING #( lt_return_modify ).
      mo_bopf_util->rollback(  ).
    ELSE.
      DATA(lt_execution_log_created) = mo_query->get_execution_log( iv_key = lo_execution_log->key ).
      es_data = lt_execution_log_created[ 1 ].
    ENDIF.
  ENDMETHOD.

  METHOD edit_execution_log.

    DATA(lo_execution_log) = NEW zrle_bo_sc_execution_log( is_execution_log ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification( ( node = zif_rle_bo_process_c=>sc_node-execution_log
                                                        change_mode = /bobf/if_frw_c=>sc_modify_update
                                                        key = lo_execution_log->key
                                                        data = lo_execution_log ) ).

    mo_bopf_util->modify_save_data(
            EXPORTING
              it_mod      = lt_mod
              iv_langu    = mv_langu
            IMPORTING
              et_return = DATA(lt_return_modify) ).

    IF line_exists( lt_return_modify[ type = zcl_cf_data=>cs_msg-type_error ]  ).
      et_return = CORRESPONDING #( lt_return_modify ).
      mo_bopf_util->rollback(  ).
    ELSE.
      DATA(lt_execution_log_created) = mo_query->get_execution_log( iv_key = lo_execution_log->key ).
      es_data = lt_execution_log_created[ 1 ].
    ENDIF.

  ENDMETHOD.

  METHOD add_execution_log_messages.
    DATA lt_mod TYPE /bobf/t_frw_modification.
    DATA lt_keys TYPE /bobf/t_frw_key.

    CLEAR: et_data, et_return.

    LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
      DATA(lo_exec_msg) = NEW zrle_bo_sc_exec_log_msg( ).
      ASSIGN lo_exec_msg->* TO FIELD-SYMBOL(<ls_exec_msg>).
      <ls_exec_msg> = CORRESPONDING #( <ls_messages> ).
      lo_exec_msg->parent_key = iv_parent_key   .
      lo_exec_msg->root_key = iv_root_key.
      lo_exec_msg->key = /bobf/cl_frw_factory=>get_new_key( ).

      INSERT VALUE #( node = zif_rle_bo_process_c=>sc_node-execution_log_message
                   change_mode = /bobf/if_frw_c=>sc_modify_create
                   key = lo_exec_msg->key
                   data = lo_exec_msg
                    source_node = zif_rle_bo_process_c=>sc_node-root
                   association = zif_rle_bo_process_c=>sc_association-execution_log-execution_log_msg
                   source_key = lo_exec_msg->parent_key
                   root_key = lo_exec_msg->root_key )
                INTO TABLE lt_mod.

      INSERT VALUE #( key = lo_exec_msg->key ) INTO TABLE lt_keys.

    ENDLOOP.

    mo_bopf_util->modify_save_data(
            EXPORTING
              it_mod      = lt_mod
              iv_langu    = mv_langu
            IMPORTING
              et_return = DATA(lt_return_modify) ).

    IF line_exists( lt_return_modify[ type = zcl_cf_data=>cs_msg-type_error ]  ).
      et_return = CORRESPONDING #( lt_return_modify ).
      mo_bopf_util->rollback(  ).
    ELSE.
      et_data = mo_query->get_execution_log_msg( it_keys = lt_keys ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
