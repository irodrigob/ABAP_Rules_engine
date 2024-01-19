*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRLE_T001.......................................*
DATA:  BEGIN OF STATUS_ZRLE_T001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRLE_T001                     .
CONTROLS: TCTRL_ZRLE_T001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZRLE_T002.......................................*
DATA:  BEGIN OF STATUS_ZRLE_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRLE_T002                     .
CONTROLS: TCTRL_ZRLE_T002
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZRLE_T003.......................................*
DATA:  BEGIN OF STATUS_ZRLE_T003                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRLE_T003                     .
CONTROLS: TCTRL_ZRLE_T003
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZRLE_T007.......................................*
DATA:  BEGIN OF STATUS_ZRLE_T007                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRLE_T007                     .
CONTROLS: TCTRL_ZRLE_T007
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZRLE_V001.......................................*
TABLES: ZRLE_V001, *ZRLE_V001. "view work areas
CONTROLS: TCTRL_ZRLE_V001
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_ZRLE_V001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZRLE_V001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZRLE_V001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZRLE_V001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V001_TOTAL.

*...processing: ZRLE_V002.......................................*
TABLES: ZRLE_V002, *ZRLE_V002. "view work areas
CONTROLS: TCTRL_ZRLE_V002
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZRLE_V002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZRLE_V002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZRLE_V002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZRLE_V002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V002_TOTAL.

*...processing: ZRLE_V003.......................................*
TABLES: ZRLE_V003, *ZRLE_V003. "view work areas
CONTROLS: TCTRL_ZRLE_V003
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_ZRLE_V003. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZRLE_V003.
* Table for entries selected to show on screen
DATA: BEGIN OF ZRLE_V003_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V003_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZRLE_V003_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZRLE_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZRLE_V003_TOTAL.

*.........table declarations:.................................*
TABLES: *ZRLE_T001                     .
TABLES: *ZRLE_T001T                    .
TABLES: *ZRLE_T002                     .
TABLES: *ZRLE_T002T                    .
TABLES: *ZRLE_T003                     .
TABLES: *ZRLE_T007                     .
TABLES: ZRLE_T001                      .
TABLES: ZRLE_T001T                     .
TABLES: ZRLE_T002                      .
TABLES: ZRLE_T002T                     .
TABLES: ZRLE_T003                      .
TABLES: ZRLE_T007                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
