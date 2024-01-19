CLASS zcl_rle_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Rellena estructura BAPIRET2</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de mensaje</p>
    "! @parameter iv_id | <p class="shorttext synchronized">ID mensaje</p>
    "! @parameter iv_number | <p class="shorttext synchronized">Numero</p>
    "! @parameter iv_message_v1 | <p class="shorttext synchronized">Variable mensaje 1</p>
    "! @parameter iv_message_v2 | <p class="shorttext synchronized">Variable mensaje 2</p>
    "! @parameter iv_message_v3 | <p class="shorttext synchronized">Variable mensaje 3</p>
    "! @parameter iv_message_v4 | <p class="shorttext synchronized">Variable mensaje 4</p>
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any DEFAULT zcl_rle_data=>cs_messages-type_success
        !iv_id           TYPE any DEFAULT zcl_rle_data=>cs_messages-id
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_utilities IMPLEMENTATION.
  METHOD fill_return.
    CLEAR rs_return.

    rs_return-type = iv_type.
    rs_return-id = iv_id.
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = rs_return-id
        number     = rs_return-number
        language   = iv_langu
        textformat = 'ASC'
        message_v1 = rs_return-message_v1
        message_v2 = rs_return-message_v2
        message_v3 = rs_return-message_v3
        message_v4 = rs_return-message_v4
      IMPORTING
        message    = rs_return-message.
  ENDMETHOD.

ENDCLASS.
