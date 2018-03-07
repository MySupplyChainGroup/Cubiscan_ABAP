*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02/15/2018 at 15:27:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCUBI_TOLERANCES................................*
DATA:  BEGIN OF STATUS_ZCUBI_TOLERANCES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUBI_TOLERANCES              .
CONTROLS: TCTRL_ZCUBI_TOLERANCES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCUBI_TOLERANCES              .
TABLES: ZCUBI_TOLERANCES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
