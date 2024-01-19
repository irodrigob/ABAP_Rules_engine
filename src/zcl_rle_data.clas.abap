CLASS zcl_rle_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_r_rules TYPE RANGE OF zrle_e_rule.
    TYPES: tt_r_process TYPE RANGE OF zrle_e_process.
    TYPES: BEGIN OF ts_return,
             type    TYPE bapi_mtype,
             message TYPE string,
           END OF ts_return.
    TYPES: tt_return TYPE STANDARD TABLE OF ts_return WITH DEFAULT KEY.
    CONSTANTS: BEGIN OF cs_messages,
                 type_error   TYPE bapi_mtype VALUE 'E',
                 type_dump    TYPE bapi_mtype VALUE 'X',
                 type_success TYPE bapi_mtype VALUE 'S',
                 type_warning TYPE bapi_mtype VALUE 'W',
                 type_info    TYPE bapi_mtype VALUE 'I',
                 id           TYPE arbgb VALUE 'ZRLE',
               END OF cs_messages.
    CONSTANTS: BEGIN OF cs_rules,
                 BEGIN OF decision,
                   then TYPE zrle_e_rule_decision VALUE 'T',
                   else TYPE zrle_e_rule_decision VALUE 'E',
                 END OF decision,
               END OF cs_rules.
    CONSTANTS: BEGIN OF cs_domains,
                 decision TYPE domname VALUE 'ZRLE_D_RULE_DECISION',
               END OF cs_domains.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rle_data IMPLEMENTATION.
ENDCLASS.
