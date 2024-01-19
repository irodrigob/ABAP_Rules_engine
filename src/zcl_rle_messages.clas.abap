CLASS zcl_rle_messages DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_message,
             process      TYPE zrle_e_process,
             rule         TYPE zrle_e_rule.
             INCLUDE TYPE symsg.
           TYPES:
                    message_text TYPE string,
                  END OF ts_message.
    TYPES: tt_messages TYPE STANDARD TABLE OF ts_message WITH EMPTY KEY.
    "! <p class="shorttext synchronized">Añade mensaje</p>
    "! @parameter is_message | <p class="shorttext synchronized">Mensaje</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    "! @parameter iv_rule | <p class="shorttext synchronized">Regla</p>
    METHODS add_message
      IMPORTING is_message TYPE symsg
                iv_process TYPE zrle_e_process OPTIONAL
                iv_rule    TYPE zrle_e_rule OPTIONAL.
    "! <p class="shorttext synchronized">Añade mensaje de texto</p>
    "! @parameter iv_message | <p class="shorttext synchronized">Mensaje</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de mensaje</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    "! @parameter iv_rule | <p class="shorttext synchronized">Regla</p>
    METHODS add_message_text
      IMPORTING iv_message TYPE string
                iv_type    TYPE msgty OPTIONAL
                iv_process TYPE zrle_e_process OPTIONAL
                iv_rule    TYPE zrle_e_rule OPTIONAL.
    "! <p class="shorttext synchronized">Devuelve los mensajes</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter rt_messages | <p class="shorttext synchronized">Mensajes</p>
    METHODS get_messages
      IMPORTING iv_langu           TYPE sylangu DEFAULT sy-langu
      RETURNING VALUE(rt_messages) TYPE tt_messages.
    "! <p class="shorttext synchronized">Limpia de mensajes</p>
    METHODS clear.
  PROTECTED SECTION.

    DATA mt_messages TYPE tt_messages.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_messages IMPLEMENTATION.
  METHOD add_message.
    INSERT CORRESPONDING #( is_message ) INTO TABLE mt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
    <ls_message>-rule = iv_rule.
    <ls_message>-process = iv_process.
  ENDMETHOD.

  METHOD get_messages.
    CLEAR rt_messages.

    LOOP AT mt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
      INSERT CORRESPONDING #( <ls_messages> ) INTO TABLE rt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF <ls_messages>-message_text IS INITIAL.
        <ls_message>-message_text = zcl_rle_utilities=>fill_return( iv_id = <ls_message>-msgid
                                                                    iv_type = <ls_message>-msgty
                                                                    iv_number = <ls_message>-msgno
                                                                    iv_message_v1 = <ls_message>-msgv1
                                                                    iv_message_v2 = <ls_message>-msgv2
                                                                    iv_message_v3 = <ls_message>-msgv3
                                                                    iv_message_v4 = <ls_message>-msgv4
                                                                    iv_langu = iv_langu )-message.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD clear.
    CLEAR: mt_messages.
  ENDMETHOD.

  METHOD add_message_text.
    INSERT VALUE #( message_text = iv_message
                    msgty = iv_type ) INTO TABLE mt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
    <ls_message>-rule = iv_rule.
    <ls_message>-process = iv_process.
  ENDMETHOD.

ENDCLASS.
