CLASS zcl_rle_engine DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_result_process,
             last_rule_name      TYPE zrle_e_rule,
             last_rule_name_desc TYPE zrle_e_rule_description,
             messages            TYPE zcl_rle_rule_decision=>tt_messages,
           END OF ts_result_process.
    TYPES: BEGIN OF ts_result_complete,
             step                TYPE zrle_e_step_number,
             rule_name           TYPE zrle_e_rule,
             rule_name_desc      TYPE zrle_e_rule_description,
             decision_taken      TYPE zrle_e_rule_decision,
             decision_taken_desc TYPE zrle_e_decision_taken_desc,
             messages            TYPE zcl_rle_rule_decision=>tt_messages,
             next_rule_name      TYPE zrle_e_rule_next,
             next_rule_name_desc TYPE zrle_e_rule_description,
             username            TYPE syuname,
             begin_date          TYPE zrle_e_begin_date,
             begin_time          TYPE zrle_e_begin_time,
             end_date            TYPE zrle_e_end_date,
             end_time            TYPE zrle_e_end_time,
           END OF ts_result_complete.
    TYPES: tt_result_complete TYPE STANDARD TABLE OF ts_result_complete WITH EMPTY KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter iv_proceso | <p class="shorttext synchronized">Proceso</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING iv_app     TYPE zrle_e_app
                iv_process TYPE zrle_e_process
                iv_langu   TYPE sylangu DEFAULT sy-langu
      RAISING   zcx_rle.
    "! <p class="shorttext synchronized">Inicio del proceso</p>
    "! @parameter io_values | <p class="shorttext synchronized">Valores para evaluar el proceso</p>
    METHODS start_process
      IMPORTING io_values TYPE REF TO zcl_rle_container OPTIONAL
      EXPORTING es_result TYPE ts_result_process.
    "! <p class="shorttext synchronized">Devuelve el proceso completo</p>
    "! @parameter rt_process | <p class="shorttext synchronized">Proceso completo</p>
    METHODS get_complete_process
      RETURNING VALUE(rt_process) TYPE tt_result_complete.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_log_rules,
             step                TYPE i,
             rule_name           TYPE zrle_e_rule,
             rule_name_desc      TYPE zrle_e_rule_description,
             decision_object     TYPE REF TO zcl_rle_rule_decision,
             next_rule_name      TYPE zrle_e_rule,
             next_rule_name_desc TYPE zrle_e_rule_description,
             username            TYPE syuname,
             begin_date          TYPE zrle_e_begin_date,
             begin_time          TYPE zrle_e_begin_time,
             end_date            TYPE zrle_e_end_date,
             end_time            TYPE zrle_e_end_time,
           END OF ts_log_rules.
    TYPES: BEGIN OF ts_rules.
             INCLUDE TYPE zcl_rle_master_data=>ts_rules.
           TYPES:
                    instance TYPE REF TO zif_rle_rule,
                  END OF ts_rules.
    TYPES: tt_rules TYPE STANDARD TABLE OF ts_rules WITH DEFAULT KEY.

    TYPES tt_log_rules TYPE STANDARD TABLE OF ts_log_rules WITH DEFAULT KEY.
    DATA mt_log_rules TYPE tt_log_rules.
    DATA mv_langu TYPE sylangu.
    DATA mv_process TYPE zrle_e_process.
    DATA mv_app TYPE zrle_e_app.
    DATA ms_process TYPE zcl_rle_master_data=>ts_process.
    DATA mt_process_rules TYPE zcl_rle_master_data=>tt_process_rules.
    DATA mt_rules TYPE tt_rules.
    DATA ms_log_rule_active TYPE ts_log_rules.
    DATA mo_master_data TYPE REF TO zcl_rle_master_data.
    DATA mo_model_data TYPE REF TO zcl_rle_bo_process_model.
    DATA mo_model_query TYPE REF TO zcl_rle_bo_process_query.
    DATA ms_bo_process TYPE zrle_bo_sc_process.
    DATA ms_bo_execution_log TYPE zrle_bo_sc_execution_log.

    "! <p class="shorttext synchronized">Lectura de la configuración del proceso</p>
    METHODS load_process_configuration
      RAISING zcx_rle.
    "! <p class="shorttext synchronized">Lectura y verificación de reglas</p>
    METHODS load_check_rules
      RAISING zcx_rle.
    "! <p class="shorttext synchronized">verifica e instancia la regla</p>
    METHODS check_instance_rule
      IMPORTING
                iv_rule TYPE zrle_e_rule
      RAISING   zcx_rle.
    "! <p class="shorttext synchronized">verifica que exista regla inicio</p>
    METHODS check_initial_rule
      RAISING zcx_rle.
    "! <p class="shorttext synchronized">Ejecuta la regla</p>
    "! @parameter io_values | <p class="shorttext synchronized">Valores para evaluar el proceso</p>
    "! @parameter iv_rule_name | <p class="shorttext synchronized">Nombre de la regla</p>
    METHODS execute_rule
      IMPORTING
        io_values    TYPE REF TO zcl_rle_container
        iv_rule_name TYPE zrle_e_rule.
    "! <p class="shorttext synchronized">Inicio log de ejecución de la regla</p>
    "! @parameter iv_rule_name | <p class="shorttext synchronized">Nombre de la regla</p>
    METHODS start_log_rule
      IMPORTING
        iv_rule_name TYPE zrle_e_rule.
    "! <p class="shorttext synchronized">Fin log de ejecución de la regla</p>
    "! @parameter io_decision | <p class="shorttext synchronized">Decision de la regla</p>
    "! @parameter iv_next_rule | <p class="shorttext synchronized">Siguiente regla determinada</p>
    METHODS end_log_rule
      IMPORTING
        io_decision  TYPE REF TO zcl_rle_rule_decision
        iv_next_rule TYPE zrle_e_rule.
    "! <p class="shorttext synchronized">Persistencia del log del proceso</p>
    "! @parameter iv_end | <p class="shorttext synchronized">Fin de grabacion del log</p>
    METHODS persist_log_rule
      IMPORTING iv_end TYPE sap_bool OPTIONAL.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_rle_engine IMPLEMENTATION.
  METHOD constructor.
    mv_process = iv_process.
    mv_app = iv_app.
    mv_langu = iv_langu.

    mo_master_data = NEW #( iv_langu = mv_langu ).
    mo_model_data = NEW #( iv_langu = mv_langu ).
    mo_model_query = NEW #( iv_langu = mv_langu ).
    load_process_configuration(  ).

  ENDMETHOD.

  METHOD load_process_configuration.

    ms_process = CORRESPONDING #( mo_master_data->get_process( iv_app = mv_app iv_process = mv_process ) ).
    IF ms_process IS NOT INITIAL.
      mt_process_rules = mo_master_data->get_process_rules( iv_app = mv_app iv_process = mv_process ).
      IF mt_process_rules IS NOT INITIAL.

        check_initial_rule( ).

        load_check_rules( ).

      ELSE.
        RAISE EXCEPTION TYPE zcx_rle
          EXPORTING
            textid   = zcx_rle=>process_wo_rules
            mv_msgv1 = CONV #( mv_process ).
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_rle
        EXPORTING
          textid   = zcx_rle=>process_not_configured
          mv_msgv1 = CONV #( mv_process ).
    ENDIF.
  ENDMETHOD.


  METHOD load_check_rules.

    check_initial_rule( ).

    DATA(lt_r_rules) = VALUE zcl_rle_data=>tt_r_rules( ).

    LOOP AT mt_process_rules ASSIGNING FIELD-SYMBOL(<ls_rules_process>).
      IF <ls_rules_process>-rule_name IS NOT INITIAL.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_rules_process>-rule_name ) INTO TABLE lt_r_rules.
      ENDIF.
      IF <ls_rules_process>-rule_name_else IS NOT INITIAL.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_rules_process>-rule_name_else ) INTO TABLE lt_r_rules.
      ENDIF.
      IF <ls_rules_process>-rule_name_then IS NOT INITIAL.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_rules_process>-rule_name_then ) INTO TABLE lt_r_rules.
      ENDIF.
    ENDLOOP.

    SORT lt_r_rules.
    DELETE ADJACENT DUPLICATES FROM lt_r_rules.

    IF lt_r_rules IS NOT INITIAL.

      mt_rules = CORRESPONDING #( mo_master_data->get_rules( iv_app = zcl_cps_iwr_data=>cv_app_rules
                                                             it_r_rules = lt_r_rules ) ).
      IF mt_rules IS NOT INITIAL.
        LOOP AT mt_process_rules ASSIGNING <ls_rules_process>.
          check_instance_rule( <ls_rules_process>-rule_name ).
          IF <ls_rules_process>-rule_name_then IS NOT INITIAL.
            check_instance_rule( <ls_rules_process>-rule_name_then ).
          ENDIF.
          IF <ls_rules_process>-rule_name_else IS NOT INITIAL.
            check_instance_rule( <ls_rules_process>-rule_name_else ).
          ENDIF.
        ENDLOOP.
      ELSE.
        RAISE EXCEPTION TYPE zcx_rle
          EXPORTING
            textid   = zcx_rle=>process_wo_rules
            mv_msgv1 = CONV #( mv_process ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_rle
        EXPORTING
          textid   = zcx_rle=>process_wo_rules
          mv_msgv1 = CONV #( mv_process ).
    ENDIF.
  ENDMETHOD.


  METHOD check_instance_rule.
    ASSIGN mt_rules[ rule_name = iv_rule ] TO FIELD-SYMBOL(<ls_rule>).
    IF sy-subrc = 0.
      IF <ls_rule>-controller_class IS NOT INITIAL.
        IF <ls_rule>-instance IS NOT BOUND.
          TRY.
              CREATE OBJECT <ls_rule>-instance TYPE (<ls_rule>-controller_class).
            CATCH cx_root.
              RAISE EXCEPTION TYPE zcx_rle
                EXPORTING
                  textid   = zcx_rle=>rule_not_class_controller_conf
                  mv_msgv1 = CONV #( iv_rule ).
          ENDTRY.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_rle
          EXPORTING
            textid   = zcx_rle=>rule_not_class_controller_conf
            mv_msgv1 = CONV #( iv_rule ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_rle
        EXPORTING
          textid   = zcx_rle=>rule_not_configured
          mv_msgv1 = CONV #( iv_rule ).
    ENDIF.

  ENDMETHOD.


  METHOD check_initial_rule.
    LOOP AT mt_process_rules TRANSPORTING NO FIELDS WHERE start_process = abap_true AND rule_name IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_rle
        EXPORTING
          textid   = zcx_rle=>process_wo_initial_rule
          mv_msgv1 = CONV #( mv_process ).
    ENDIF.

  ENDMETHOD.

  METHOD start_process.

    CLEAR: mt_log_rules, es_result.

    " Se inicia con la regla inicial. Existaría porque se ha validado al inicar el proceso
    ASSIGN mt_process_rules[ start_process = abap_true ] TO FIELD-SYMBOL(<ls_rules_process>).

    " Ejecuta la regla
    execute_rule( EXPORTING io_values = io_values
                            iv_rule_name = <ls_rules_process>-rule_name  ).


    READ TABLE mt_log_rules ASSIGNING FIELD-SYMBOL(<ls_log_rules>) INDEX lines( mt_log_rules ).
    IF sy-subrc = 0.
      es_result-last_rule_name = <ls_log_rules>-rule_name.
      es_result-last_rule_name_desc = <ls_log_rules>-rule_name_desc.
      es_result-messages = <ls_log_rules>-decision_object->get_messages( ).
    ENDIF.
  ENDMETHOD.


  METHOD execute_rule.
    DATA lv_next_rule TYPE zrle_e_rule.

    start_log_rule( EXPORTING iv_rule_name = iv_rule_name ).


    ASSIGN mt_process_rules[ rule_name = iv_rule_name ] TO FIELD-SYMBOL(<ls_rules_process>).

    ASSIGN mt_rules[ rule_name = iv_rule_name ] TO FIELD-SYMBOL(<ls_rule>).
    DATA(lo_decision) = zcl_rle_rule_decision=>get_instance(
                          iv_rule_name    = iv_rule_name
                          iv_process = mv_process ).

    TRY.

        <ls_rule>-instance->evaluate(
          EXPORTING
            io_values   = io_values
            io_decision = lo_decision ).

        DATA(lv_decision) = lo_decision->get_decision( ).

        IF lv_decision = zcl_rle_data=>cs_rules-decision-then
           OR lv_decision = zcl_rle_data=>cs_rules-decision-else.

          lv_next_rule = SWITCH #( lv_decision
                                 WHEN zcl_rle_data=>cs_rules-decision-then THEN <ls_rules_process>-rule_name_then
                                 WHEN zcl_rle_data=>cs_rules-decision-else THEN <ls_rules_process>-rule_name_else ).

        ELSE.
          lo_decision->add_message( is_message = VALUE #( msgid = zcl_rle_data=>cs_messages-id
                                                                msgno = '007'
                                                                msgv1 = iv_rule_name
                                                                msgty = zcl_fpy_data=>cs_msg-type_error ) ).

        ENDIF.

      CATCH cx_root.
        lo_decision->add_message( is_message = VALUE #( msgid = zcl_rle_data=>cs_messages-id
                                                        msgno = '006'
                                                        msgv1 = iv_rule_name
                                                        msgty = zcl_fpy_data=>cs_msg-type_error ) ).
    ENDTRY.

    end_log_rule( EXPORTING io_decision = lo_decision
                            iv_next_rule = lv_next_rule ).

    " Si hay siguiente regla se llama al mismo método para ejecutar la nueva regla.
    IF lv_next_rule IS NOT INITIAL.
      execute_rule( io_values = io_values
                    iv_rule_name = lv_next_rule ).
    ENDIF.
  ENDMETHOD.


  METHOD start_log_rule.
    GET TIME.
    ms_log_rule_active-step = lines( mt_log_rules ) + 1.
    ms_log_rule_active-rule_name = iv_rule_name.
    ms_log_rule_active-rule_name_desc = mt_rules[ rule_name = iv_rule_name ]-rule_name_desc.
    ms_log_rule_active-username = sy-uname.
    ms_log_rule_active-begin_date = sy-datum.
    ms_log_rule_active-begin_time = sy-uzeit.

    IF ms_process-save_trace = abap_true.
      persist_log_rule( ).
    ENDIF.

  ENDMETHOD.


  METHOD end_log_rule.
    GET TIME.

    ms_log_rule_active-end_date = sy-datum.
    ms_log_rule_active-end_time = sy-uzeit.
    ms_log_rule_active-decision_object = io_decision.

    ms_log_rule_active-next_rule_name = iv_next_rule.
    IF iv_next_rule IS NOT INITIAL.
      ms_log_rule_active-next_rule_name_desc = mt_rules[ rule_name = ms_log_rule_active-next_rule_name ]-rule_name_desc.
    ENDIF.

    INSERT ms_log_rule_active INTO TABLE mt_log_rules.
    CLEAR: ms_log_rule_active.

    IF ms_process-save_trace = abap_true.
      persist_log_rule( iv_end = abap_true ).
    ENDIF.

  ENDMETHOD.

  METHOD get_complete_process.

    CLEAR: rt_process.

    LOOP AT mt_log_rules ASSIGNING FIELD-SYMBOL(<ls_log_rules>).
      INSERT CORRESPONDING #( <ls_log_rules> ) INTO TABLE rt_process ASSIGNING FIELD-SYMBOL(<ls_process>).
      <ls_process>-messages = <ls_log_rules>-decision_object->get_messages( ).
      <ls_process>-decision_taken = <ls_log_rules>-decision_object->get_decision( ).
      <ls_process>-decision_taken_desc = mo_master_data->get_decision_desc( <ls_process>-decision_taken ).

    ENDLOOP.

  ENDMETHOD.


  METHOD persist_log_rule.

    IF ms_bo_process IS INITIAL.
      DATA(lt_header_process) = mo_model_query->get_process_header_data( iv_process_name = mv_process ).
      IF lt_header_process IS INITIAL.
        mo_model_data->new_process( EXPORTING is_process = VALUE #( process_name = mv_process )
                                    IMPORTING es_data = ms_bo_process ).
      ELSE.
        ms_bo_process = lt_header_process[ 1 ].
      ENDIF.
    ENDIF.
    " No debería ocurrir que este vacio porque no hay controles ni nada a la hora de grabar el proceso. Pero no hacer
    " persistencia no debe influir en el flujo de proceso
    IF ms_bo_process IS INITIAL. EXIT. ENDIF.

    IF ms_bo_execution_log IS INITIAL. " Si esta vacio se crea el proceso
      mo_model_data->new_execution_log(
        EXPORTING
          iv_root_key      = ms_bo_process-key
          is_execution_log = VALUE #( step = ms_log_rule_active-step
                                      rule_name = ms_log_rule_active-rule_name
                                      begin_date = ms_log_rule_active-begin_date
                                      begin_time = ms_log_rule_active-begin_time
                                      username = ms_log_rule_active-username  )
        IMPORTING
          es_data          = ms_bo_execution_log
      ).
    ELSE. " Se actualiza con el fin del proceso

      ms_bo_execution_log-decision_taken = ms_log_rule_active-decision_object->get_decision( ).
      ms_bo_execution_log-next_rule_name = ms_log_rule_active-next_rule_name.
      ms_bo_execution_log-end_date = ms_log_rule_active-next_rule_name.
      ms_bo_execution_log-end_time = ms_log_rule_active-end_time.

      mo_model_data->edit_execution_log(
        EXPORTING
          is_execution_log = ms_bo_execution_log
        IMPORTING
          es_data          = ms_bo_execution_log
      ).

      " Si el objeto de decision esta instanciado y hay datos del BO de procesos recupero los mensajes y los añado
      IF ms_log_rule_active-decision_object IS BOUND AND ms_bo_execution_log IS NOT INITIAL.

        mo_model_data->add_execution_log_messages(
          EXPORTING
            iv_root_key   = ms_bo_process-key
            iv_parent_key = ms_bo_execution_log-key
            it_messages   = CORRESPONDING #( ms_log_rule_active-decision_object->get_messages( ) ) ).
      ENDIF.

    ENDIF.

    " Al final del proceso se hace reset de los datos del log de ejecución
    IF iv_end = abap_true.
      CLEAR ms_bo_execution_log.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
