CLASS zcl_rle_bo_process_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_execute_log_msg.
             INCLUDE TYPE zrle_bo_sc_execution_log.
             TYPES: messages TYPE bapiret2_t,
           END OF ts_execute_log_msg.
    TYPES: tt_execute_log_msg TYPE STANDARD TABLE OF ts_execute_log_msg WITH DEFAULT KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Devuelve los datos de cabecera del proceso</p>
    "! @parameter iv_process_name | <p class="shorttext synchronized">Proceso</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave de la solicitud</p>
    "! @parameter it_keys | <p class="shorttext synchronized">Claves de la solicitud</p>
    "! @parameter it_params | <p class="shorttext synchronized">Parámetros de búsqueda</p>
    "! @parameter rt_header_data | <p class="shorttext synchronized">Datos de cabecera</p>
    METHODS get_process_header_data
      IMPORTING
        iv_process_name       TYPE zrle_e_process OPTIONAL
        iv_key                TYPE /bobf/conf_key OPTIONAL
        it_keys               TYPE /bobf/t_frw_key OPTIONAL
        it_params             TYPE /bobf/t_frw_query_selparam  OPTIONAL
      RETURNING
        VALUE(rt_header_data) TYPE zrle_bo_i_process.
    "! <p class="shorttext synchronized">Devuelve los datos de la ejecucuón de log</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo padre</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del nodo</p>
    "! @parameter it_params | <p class="shorttext synchronized">Parámetros de búsqueda</p>
    "! @parameter rt_execution_log | <p class="shorttext synchronized">Datos de ejecución de log</p>
    METHODS get_execution_log
      IMPORTING
                iv_root_key             TYPE /bobf/conf_key OPTIONAL
                iv_key                  TYPE /bobf/conf_key OPTIONAL
                it_params               TYPE /bobf/t_frw_query_selparam  OPTIONAL
      RETURNING VALUE(rt_execution_log) TYPE zrle_bo_i_execution_log.
    "! <p class="shorttext synchronized">Devuelve los datos de la ejecucuón de log con mensajes</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo padre</p>
    "! @parameter it_params | <p class="shorttext synchronized">Parámetros de búsqueda</p>
    "! @parameter rt_execution_log | <p class="shorttext synchronized">Datos de ejecución de log</p>
    METHODS get_execution_log_complete
      IMPORTING
                iv_root_key                 TYPE /bobf/conf_key OPTIONAL
                it_params                   TYPE /bobf/t_frw_query_selparam  OPTIONAL
      RETURNING VALUE(rt_execution_log_msg) TYPE tt_execute_log_msg.
    "! <p class="shorttext synchronized">Devuelve los datos de la ejecucuón de log con mensajes</p>
    "! @parameter iv_parent_key | <p class="shorttext synchronized">Clave del nodo padre</p>
    "! @parameter it_keys | <p class="shorttext synchronized">Clave de los mensajes</p>
    "! @parameter it_params | <p class="shorttext synchronized">Parámetros de búsqueda</p>
    "! @parameter rt_execution_log | <p class="shorttext synchronized">Datos de ejecución de log</p>
    METHODS get_execution_log_msg
      IMPORTING
                iv_parent_key               TYPE /bobf/conf_key OPTIONAL
                it_keys                     TYPE /bobf/t_frw_key  OPTIONAL
                it_params                   TYPE /bobf/t_frw_query_selparam  OPTIONAL
      RETURNING VALUE(rt_execution_log_msg) TYPE zrle_bo_i_exec_log_msg.
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
    DATA mo_bopf_util TYPE REF TO zcl_ca_bopf_util.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Inicialización variables para el BOPF</p>
    METHODS init_bopf.
ENDCLASS.



CLASS zcl_rle_bo_process_query IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

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

  METHOD get_process_header_data.
    CLEAR: rt_header_data.

    IF iv_process_name IS NOT INITIAL.
      mo_svc_mngr->query(
              EXPORTING
                iv_query_key            = zif_rle_bo_process_c=>sc_query-root-select_by_elements
                it_selection_parameters = VALUE #( ( attribute_name = zif_rle_bo_process_c=>sc_query_attribute-root-select_by_elements-process_name
                                                     sign = 'I' option = 'EQ' low = iv_process_name ) )
                iv_fill_data            = abap_true
              IMPORTING
                et_data                 = rt_header_data ).
    ELSEIF iv_key IS NOT INITIAL.
      mo_svc_mngr->retrieve(
        EXPORTING
          iv_node_key             = zif_rle_bo_process_c=>sc_node-root
          it_key                  = VALUE #( ( key = iv_key ) )
        IMPORTING
          et_data                 = rt_header_data
      ).
    ELSEIF it_keys IS NOT INITIAL.
      mo_svc_mngr->retrieve(
      EXPORTING
        iv_node_key             = zif_rle_bo_process_c=>sc_node-root
        it_key                  = it_keys
      IMPORTING
        et_data                 = rt_header_data
    ).
    ELSE.
      mo_svc_mngr->query(
        EXPORTING
          iv_query_key            = zif_rle_bo_process_c=>sc_query-root-select_by_elements
          it_selection_parameters = it_params
          iv_fill_data            = abap_true
        IMPORTING
          et_data                 = rt_header_data
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_execution_log.

    CLEAR rt_execution_log.

    IF iv_root_key IS NOT INITIAL.
      mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_rle_bo_process_c=>sc_node-root
                                            it_key                  = VALUE #( ( key = iv_root_key ) )
                                            iv_association          = zif_rle_bo_process_c=>sc_association-root-execution_log
                                            iv_fill_data            = abap_true
                                  IMPORTING et_data                 = rt_execution_log ).
    ELSEIF iv_key IS NOT INITIAL.
      mo_svc_mngr->retrieve(
              EXPORTING
                iv_node_key             = zif_rle_bo_process_c=>sc_node-execution_log
                it_key                  = VALUE #( ( key = iv_key ) )
              IMPORTING
                et_data                 = rt_execution_log
            ).
    ELSEIF it_params IS NOT INITIAL.
      mo_svc_mngr->query(
             EXPORTING
               iv_query_key            = zif_rle_bo_process_c=>sc_query-execution_log-select_by_elements
               it_selection_parameters = it_params
               iv_fill_data            = abap_true
             IMPORTING
               et_data                 = rt_execution_log ).
    ENDIF.
  ENDMETHOD.

  METHOD get_execution_log_complete.
    DATA lt_exec_log_msg TYPE zrle_bo_i_exec_log_msg.

    CLEAR rt_execution_log_msg.


    DATA(lt_execution_log) = get_execution_log( iv_root_key = iv_root_key
                                                it_params   = it_params ).

    IF lt_execution_log IS NOT INITIAL.
      DATA(lt_keys) = VALUE /bobf/t_frw_key( FOR <wa> IN lt_execution_log ( key = <wa>-key ) ).
      SORT lt_keys.
      DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING ALL FIELDS.

      mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_rle_bo_process_c=>sc_node-execution_log_message
                                           it_key                  = lt_keys
                                           iv_association          = zif_rle_bo_process_c=>sc_association-execution_log-execution_log_msg
                                           iv_fill_data            = abap_true
                                 IMPORTING et_data                 = lt_exec_log_msg ).

      LOOP AT lt_execution_log ASSIGNING FIELD-SYMBOL(<ls_execution_log>).
        INSERT CORRESPONDING #( <ls_execution_log> ) INTO TABLE rt_execution_log_msg ASSIGNING FIELD-SYMBOL(<ls_exec_log_msg>).

        <ls_exec_log_msg>-messages = VALUE #( FOR <wa1> IN lt_exec_log_msg
                                              WHERE ( parent_key = <ls_execution_log>-key )
                                              ( type = <wa1>-msgty
                                                id = <wa1>-msgid
                                                number = <wa1>-msgno
                                                message_v1 = <wa1>-msgv1
                                                message_v2 = <wa1>-msgv2
                                                message_v3 = <wa1>-msgv3
                                                message_v4 = <wa1>-msgv4
                                                message = <wa1>-message ) ).

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD get_execution_log_msg.
    CLEAR rt_execution_log_msg.

    IF iv_parent_key IS NOT INITIAL.
      mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_rle_bo_process_c=>sc_node-execution_log
                                            it_key                  = VALUE #( ( key = iv_parent_key ) )
                                            iv_association          = zif_rle_bo_process_c=>sc_association-execution_log-execution_log_msg
                                            iv_fill_data            = abap_true
                                  IMPORTING et_data                 = rt_execution_log_msg ).
    ELSEIF it_keys IS NOT INITIAL.
      mo_svc_mngr->retrieve( EXPORTING iv_node_key             = zif_rle_bo_process_c=>sc_node-execution_log_message
                                       it_key                  = it_keys
                             IMPORTING et_data                 = rt_execution_log_msg ).
    ELSEIF it_params IS NOT INITIAL.
      mo_svc_mngr->query(
             EXPORTING
               iv_query_key            = zif_rle_bo_process_c=>sc_query-execution_log_msg-select_by_elements
               it_selection_parameters = it_params
               iv_fill_data            = abap_true
             IMPORTING
               et_data                 = rt_execution_log_msg ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
