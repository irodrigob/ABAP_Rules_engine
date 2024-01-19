CLASS zcl_rle_rule_decision DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_messages.
             INCLUDE TYPE symsg.
             TYPES: message_text TYPE bapi_msg,
           END OF ts_messages.
    TYPES: tt_messages TYPE STANDARD TABLE OF ts_messages WITH DEFAULT KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_rule | <p class="shorttext synchronized">Instancia de la toma de decision</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    CLASS-METHODS get_instance
      IMPORTING
                iv_rule_name       TYPE zrle_e_rule
                iv_process         TYPE zrle_e_process
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_rle_rule_decision.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_rule | <p class="shorttext synchronized">Regla</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    METHODS constructor
      IMPORTING
        iv_rule_name TYPE zrle_e_rule
        iv_process   TYPE zrle_e_process.
    "! <p class="shorttext synchronized">Establece la decisión en SI</p>
    METHODS set_decision_yes.
    "! <p class="shorttext synchronized">Establece la decisión en NO</p>
    METHODS set_decision_no.
    "! <p class="shorttext synchronized">Devuelve la decisión tomada</p>
    "! @parameter rv_decision | <p class="shorttext synchronized">Decision</p>
    METHODS get_decision
      RETURNING VALUE(rv_decision) TYPE zrle_e_rule_decision.
    "! <p class="shorttext synchronized">Añade mensaje</p>
    "! @parameter is_message | <p class="shorttext synchronized">Mensaje</p>
    METHODS add_message
      IMPORTING is_message TYPE symsg.
    "! <p class="shorttext synchronized">Añade mensaje de texto</p>
    "! @parameter iv_message | <p class="shorttext synchronized">Mensaje</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de mensaje</p>
    METHODS add_message_text
      IMPORTING iv_message TYPE string
                iv_type    TYPE msgty OPTIONAL.
    "! <p class="shorttext synchronized">Devuelve los mensajes de la decisión</p>
    "! @parameter rt_messages | <p class="shorttext synchronized">Mensajes</p>
    METHODS get_messages
      RETURNING VALUE(rt_messages) TYPE tt_messages.
  PROTECTED SECTION.
    DATA mv_rule_name TYPE zrle_e_rule.
    DATA mv_process TYPE zrle_e_process.
    DATA mo_messages TYPE REF TO zcl_rle_messages.

    "! <p class="shorttext synchronized">Establece la decisión</p>
    METHODS set_decision
      IMPORTING iv_decision TYPE zrle_e_rule_decision .
    DATA mv_decision TYPE zrle_e_rule_decision.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_rule_decision IMPLEMENTATION.
  METHOD get_instance.
    ro_instance = NEW #( iv_process = iv_process
                         iv_rule_name = iv_rule_name ).
  ENDMETHOD.

  METHOD constructor.
    mv_rule_name = iv_rule_name.
    mv_process = iv_process.

    mo_messages = NEW #(  ).
  ENDMETHOD.


  METHOD set_decision_yes.
    set_decision( iv_decision = zcl_rle_data=>cs_rules-decision-then ).
  ENDMETHOD.

  METHOD set_decision.
    mv_decision = iv_decision.
  ENDMETHOD.

  METHOD set_decision_no.
    set_decision( iv_decision = zcl_rle_data=>cs_rules-decision-else ).
  ENDMETHOD.

  METHOD add_message.
    mo_messages->add_message( is_message = is_message
                              iv_process = mv_process
                              iv_rule = mv_rule_name ).
  ENDMETHOD.

  METHOD get_decision.
    rv_decision = mv_decision.
  ENDMETHOD.

  METHOD get_messages.
    rt_messages = CORRESPONDING #( mo_messages->get_messages( ) ).
  ENDMETHOD.

  METHOD add_message_text.
    mo_messages->add_message_text( iv_message = iv_message
                                   iv_type = iv_type
                                   iv_process = mv_process
                                   iv_rule = mv_rule_name ).
  ENDMETHOD.

ENDCLASS.
