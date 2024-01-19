CLASS zcx_rle DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF process_not_configured,
        msgid TYPE symsgid VALUE 'ZRLE',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF process_not_configured .
    CONSTANTS:
      BEGIN OF process_wo_rules,
        msgid TYPE symsgid VALUE 'ZRLE',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF process_wo_rules .
    CONSTANTS:
      BEGIN OF rule_not_configured,
        msgid TYPE symsgid VALUE 'ZRLE',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF rule_not_configured .
    CONSTANTS: BEGIN OF rule_not_class_controller_conf,
                 msgid TYPE symsgid VALUE 'ZRLE',
                 msgno TYPE symsgno VALUE '004',
                 attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF rule_not_class_controller_conf .
    CONSTANTS: BEGIN OF process_wo_initial_rule,
                 msgid TYPE symsgid VALUE 'ZRLE',
                 msgno TYPE symsgno VALUE '005',
                 attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF process_wo_initial_rule .
    CONSTANTS: BEGIN OF rule_wo_class_controller,
                 msgid TYPE symsgid VALUE 'ZRLE',
                 msgno TYPE symsgno VALUE '006',
                 attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF rule_wo_class_controller .
    DATA mv_msgv1 TYPE string .
    DATA mv_msgv2 TYPE string .
    DATA mv_msgv3 TYPE string .
    DATA mv_msgv4 TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !mv_msgv1 TYPE string OPTIONAL
        !mv_msgv2 TYPE string OPTIONAL
        !mv_msgv3 TYPE string OPTIONAL
        !mv_msgv4 TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_rle IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    me->mv_msgv1 = mv_msgv1 .
    me->mv_msgv2 = mv_msgv2 .
    me->mv_msgv3 = mv_msgv3 .
    me->mv_msgv4 = mv_msgv4 .
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
