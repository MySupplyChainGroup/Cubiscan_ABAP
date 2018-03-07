CLASS zcl_cubiscan_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_status_new TYPE char1 VALUE 'N' ##NO_TEXT.

    CLASS-METHODS matnr_has_case_packspec
      IMPORTING
        !iv_product_no         TYPE /scdl/dl_productno
      RETURNING
        VALUE(rv_has_case_qty) TYPE boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CUBISCAN_UTILITIES IMPLEMENTATION.


  METHOD matnr_has_case_packspec.
    DATA: ls_packspec_query     TYPE /scwm/s_ps_content_query,
          ls_packspec_query_mat TYPE /scmb/mde_matnr_rstr,
          lt_packspec_keys      TYPE /scwm/tt_ps_header_key,
          lt_packspec           TYPE /scwm/tt_ps_object,
          lt_packspec_levels    TYPE /scwm/tt_ps_level,
          lv_severity           TYPE bapi_mtype.

    FIELD-SYMBOLS: <fs_packspec>       LIKE LINE OF lt_packspec,
                   <fs_packspec_level> LIKE LINE OF lt_packspec_levels.

    " Default the return value.
    rv_has_case_qty = abap_false.

    " Fill the packspec query struc.
    ls_packspec_query_mat-sign = 'I'.
    ls_packspec_query_mat-option = 'EQ'.
    ls_packspec_query_mat-low = iv_product_no.
    APPEND ls_packspec_query_mat TO ls_packspec_query-matnr_rng.

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = ls_packspec_query
      IMPORTING
        et_ps_keys       = lt_packspec_keys
        ev_severity      = lv_severity.

    IF lv_severity NE 'E'.
      IF lines( lt_packspec_keys ) GT 0.
        CALL FUNCTION '/SCWM/API_PACKSPEC_READ'
          EXPORTING
            it_ps_id    = lt_packspec_keys
          IMPORTING
            et_object   = lt_packspec
            ev_severity = lv_severity.

        IF lv_severity NE 'E'.
          READ TABLE lt_packspec ASSIGNING <fs_packspec> INDEX 1.
          IF <fs_packspec> IS ASSIGNED.
            " Get the levels.
            lt_packspec_levels = <fs_packspec>-levels.
            LOOP AT lt_packspec_levels ASSIGNING <fs_packspec_level> WHERE operat_unit EQ 'CS'.
              " Found a CV record, so the material has case qty.
              rv_has_case_qty = abap_true.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
