*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZRLE_TBL
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZRLE_TBL           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
