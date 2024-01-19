CLASS zcl_rle_master_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_decision,
             decision      TYPE zrle_e_rule_decision,
             decision_desc TYPE val_text,
           END OF ts_decision.
    TYPES: tt_decision TYPE STANDARD TABLE OF ts_decision WITH EMPTY KEY.
    TYPES: BEGIN OF ts_rules,
             application      TYPE zrle_e_app,
             rule_name        TYPE zrle_e_rule,
             rule_name_desc   TYPE zrle_e_rule_description,
             controller_class TYPE zrle_e_controller_class,
           END OF ts_rules.
    TYPES: tt_rules TYPE STANDARD TABLE OF ts_rules WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_process,
             application  TYPE zrle_e_app,
             process_name TYPE zrle_e_process,
             save_trace   TYPE zrle_e_save_trace,
             process_desc TYPE zrle_e_process_description,
           END OF ts_process.
    TYPES: tt_process TYPE STANDARD TABLE OF ts_process WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_process_rules,
             application    TYPE zrle_e_app,
             process_name   TYPE zrle_e_process,
             rule_name      TYPE zrle_e_rule,
             start_process  TYPE zrle_e_start_rule,
             rule_name_then TYPE zrle_e_rule_then,
             rule_name_else TYPE zrle_e_rule_else,
           END OF ts_process_rules.
    TYPES: tt_process_rules TYPE STANDARD TABLE OF ts_process_rules WITH EMPTY KEY.
    "! <p class="shorttext synchronized">Lectura de toma decisiones</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING iv_langu TYPE sy-langu DEFAULT sy-langu.

    "! <p class="shorttext synchronized">Descripción de una decision</p>
    "! @parameter iv_decision | <p class="shorttext synchronized">Decisión</p>
    "! @parameter rv_description | <p class="shorttext synchronized">Descripción</p>
    METHODS get_decision_desc
      IMPORTING iv_decision           TYPE zrle_e_rule_decision
      RETURNING VALUE(rv_description) TYPE zrle_e_decision_taken_desc.
    "! <p class="shorttext synchronized">Devuelve las reglas</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter it_r_rules | <p class="shorttext synchronized">Rango de reglas</p>
    "! @parameter rt_rules | <p class="shorttext synchronized">Reglas</p>
    METHODS get_rules
      IMPORTING iv_app          TYPE zrle_e_app
                it_r_rules      TYPE zcl_rle_data=>tt_r_rules OPTIONAL
      RETURNING VALUE(rt_rules) TYPE tt_rules.
    "! <p class="shorttext synchronized">Descripción de un proceso</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    "! @parameter rs_process | <p class="shorttext synchronized">Datos del proceso</p>
    METHODS get_process
      IMPORTING iv_app            TYPE zrle_e_app
                iv_process        TYPE zrle_e_process
      RETURNING VALUE(rs_process) TYPE ts_process.
    "! <p class="shorttext synchronized">Descripción de un proceso</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter iv_rule | <p class="shorttext synchronized">Regla</p>
    "! @parameter rv_description | <p class="shorttext synchronized">Descripción</p>
    METHODS get_rule_desc
      IMPORTING iv_app                TYPE zrle_e_app
                iv_rule               TYPE zrle_e_rule
      RETURNING VALUE(rv_description) TYPE zrle_e_rule_description.
    "! <p class="shorttext synchronized">Devuelve las reglas de un proceso</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    "! @parameter rt_process_rules | <p class="shorttext synchronized">Reglas del proceso</p>
    METHODS get_process_rules
      IMPORTING iv_app                  TYPE zrle_e_app
                iv_process              TYPE zrle_e_process
      RETURNING VALUE(rt_process_rules) TYPE tt_process_rules.
  PROTECTED SECTION.
    DATA mt_decisions TYPE tt_decision.
    DATA mt_rules TYPE tt_rules.
    DATA mt_process TYPE tt_process.
    DATA mt_process_rules TYPE tt_process_rules.
    DATA mv_langu TYPE sylangu.

    "! <p class="shorttext synchronized">Lectura de toma decisiones</p>
    METHODS read_decisions.
    "! <p class="shorttext synchronized">Lectura descripciones de las reglas</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    METHODS read_rules
      IMPORTING iv_app TYPE zrle_e_app.
    "! <p class="shorttext synchronized">Lectura procesos</p>
    METHODS read_process.
    "! <p class="shorttext synchronized">Lectura de reglas de un proceso</p>
    "! @parameter iv_app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter iv_process | <p class="shorttext synchronized">Proceso</p>
    METHODS read_process_rules
      IMPORTING iv_app     TYPE zrle_e_app
                iv_process TYPE zrle_e_process.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_master_data IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.
  ENDMETHOD.
  METHOD read_decisions.

    CLEAR: mt_decisions.

    SELECT domvalue_l AS decision ddtext AS decision_desc
            INTO TABLE mt_decisions
           FROM dd07t
           WHERE domname = zcl_rle_data=>cs_domains-decision
                 AND ddlanguage = mv_langu.

  ENDMETHOD.



  METHOD get_decision_desc.
    CLEAR: rv_description.

    IF mt_decisions IS INITIAL.
      read_decisions( ).
    ENDIF.

    TRY.
        rv_description = mt_decisions[ decision = iv_decision ]-decision_desc.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD read_rules.

    SELECT a~*, b~rule_desc AS rule_name_desc APPENDING CORRESPONDING FIELDS OF TABLE @mt_rules
             FROM zrle_t001 AS a LEFT OUTER JOIN zrle_t001t AS b
                     ON a~rule_name = b~rule_name
                        AND a~application = b~application
                        AND b~spras = @mv_langu
             WHERE a~application = @iv_app.

  ENDMETHOD.

  METHOD get_rules.
    CLEAR rt_rules.

    IF NOT line_exists( mt_rules[ application = iv_app ] ).
      read_rules( iv_app ).
    ENDIF.

    rt_rules = VALUE #( FOR <wa> IN mt_rules WHERE ( application = iv_app
                                                     AND rule_name IN it_r_rules ) ( <wa> ) ).
  ENDMETHOD.

  METHOD read_process.
    SELECT a~application  a~process_name a~save_trace b~process_desc FROM zrle_t002 AS a
                  LEFT OUTER JOIN zrle_t002t AS b ON
                        a~process_name = b~process_name
                        AND a~application = b~application
                        AND b~spras = mv_langu
           INTO TABLE mt_process.
  ENDMETHOD.

  METHOD get_process.
    CLEAR: rs_process.

    IF mt_process IS INITIAL.
      read_process( ).
    ENDIF.

    TRY.
        rs_process = mt_process[ application = iv_app
                                 process_name = iv_process ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_rule_desc.
    CLEAR: rv_description.

    DATA(lt_rules) = get_rules( iv_app = iv_app
                                it_r_rules = VALUE #( ( sign = 'I' option = 'EQ' low = iv_rule ) ) ).
    IF lt_rules IS NOT INITIAL.
      rv_description = lt_rules[ 1 ]-rule_name_desc.
    ENDIF.
  ENDMETHOD.

  METHOD read_process_rules.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE mt_process_rules
                FROM zrle_t003
                WHERE application = iv_app
                      AND process_name = iv_process.
  ENDMETHOD.

  METHOD get_process_rules.
    CLEAR rt_process_rules.

    IF NOT line_exists( mt_process_rules[ application = iv_app process_name = iv_process ] ).
      read_process_rules( iv_app = iv_app iv_process = iv_process ).
    ENDIF.

    rt_process_rules = VALUE #( FOR <wa> IN mt_process_rules WHERE ( application = iv_app
                                                                     AND process_name = iv_process ) ( <wa> ) ).
  ENDMETHOD.

ENDCLASS.
