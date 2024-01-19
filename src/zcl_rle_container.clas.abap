CLASS zcl_rle_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_variable,
        name  TYPE string,
        value TYPE REF TO data,
      END OF ts_variable .
    TYPES:
      tt_variables TYPE HASHED TABLE OF ts_variable WITH UNIQUE KEY primary_key COMPONENTS name .

    DATA mt_variables TYPE tt_variables .

    "! <p class="shorttext synchronized">Grabar nueva variable</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Nombre de la variable</p>
    "! @parameter iv_value | <p class="shorttext synchronized">Valores</p>
    METHODS set_variable
      IMPORTING
        !iv_name  TYPE any
        !iv_value TYPE any .
    "! <p class="shorttext synchronized">Recupera el valor de una variable</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Nombre de la variable</p>
    "! @parameter ev_value | <p class="shorttext synchronized">Valor</p>
    METHODS get_variable
      IMPORTING
        !iv_name  TYPE any
      EXPORTING
        !ev_value TYPE any.
    "! <p class="shorttext synchronized">Borra una variable</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Nombre de la variable</p>
    METHODS delete_variable
      IMPORTING
        !iv_name TYPE any.
    "! <p class="shorttext synchronized">Limpia las variables del contenedor</p>
    METHODS clear_all .
    "! <p class="shorttext synchronized">Devuelve si una variable existe</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Nombre de la variable</p>
    "! @parameter rv_exist | <p class="shorttext synchronized">Existe</p>
    METHODS exist_variable
      IMPORTING
                !iv_name        TYPE any
      RETURNING VALUE(rv_exist) TYPE sap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_container IMPLEMENTATION.
  METHOD set_variable.
    FIELD-SYMBOLS <ls_var> TYPE LINE OF tt_variables.
    FIELD-SYMBOLS <value> TYPE any.
    DATA ls_var TYPE LINE OF tt_variables.
    DATA lo_data TYPE REF TO data.

    CREATE DATA lo_data LIKE iv_value.
    ASSIGN lo_data->* TO <value>.
    <value> = iv_value.

    READ TABLE mt_variables ASSIGNING <ls_var> WITH KEY primary_key COMPONENTS name = iv_name.
    IF sy-subrc = 0.
      <ls_var>-value = lo_data.
    ELSE.
      ls_var-name = iv_name.
      ls_var-value = lo_data.
      INSERT ls_var INTO TABLE mt_variables.
    ENDIF.
  ENDMETHOD.
  METHOD get_variable.
    FIELD-SYMBOLS <ls_var> TYPE ts_variable.
    FIELD-SYMBOLS <value> TYPE any.
    CLEAR ev_value.

    READ TABLE mt_variables ASSIGNING <ls_var> WITH KEY primary_key COMPONENTS name = iv_name.
    IF sy-subrc = 0.
      ASSIGN <ls_var>-value->* TO <value>.
      ev_value = <value>.

    ENDIF.
  ENDMETHOD.

  METHOD clear_all.
    CLEAR mt_variables.
  ENDMETHOD.

  METHOD delete_variable.
    DELETE mt_variables WHERE name = iv_name.
  ENDMETHOD.
  METHOD exist_variable.
    rv_exist = COND #( WHEN line_exists( mt_variables[ name = iv_name ] ) THEN abap_true ELSE abap_false ).
  ENDMETHOD.

ENDCLASS.
