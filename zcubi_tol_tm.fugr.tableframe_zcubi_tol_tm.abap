*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCUBI_TOL_TM
*   generation date: 02/15/2018 at 15:27:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCUBI_TOL_TM       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
