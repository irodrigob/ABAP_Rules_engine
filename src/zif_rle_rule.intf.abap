INTERFACE zif_rle_rule
  PUBLIC .

  "! <p class="shorttext synchronized">Evalua la regla</p>
  "! @parameter io_values | <p class="shorttext synchronized">Contenedor de datos</p>
  "! @parameter io_decision | <p class="shorttext synchronized">Decision de la regla</p>
  METHODS evaluate
    IMPORTING io_values   TYPE REF TO zcl_rle_container
              io_decision TYPE REF TO zcl_rle_rule_decision.
ENDINTERFACE.
