*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02/15/2018 at 20:10:39
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCUBI_DEVICES...................................*
DATA:  BEGIN OF STATUS_ZCUBI_DEVICES                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUBI_DEVICES                 .
CONTROLS: TCTRL_ZCUBI_DEVICES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCUBI_DEVICES                 .
TABLES: ZCUBI_DEVICES                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
