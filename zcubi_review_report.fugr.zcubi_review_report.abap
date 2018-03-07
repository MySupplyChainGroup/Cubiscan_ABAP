FUNCTION zcubi_review_report.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_CATEGORY) TYPE  /SCWM/DE_CATEGORY OPTIONAL
*"     REFERENCE(IV_VARIANT) TYPE  VARIANT OPTIONAL
*"     REFERENCE(IV_MODE) TYPE  /SCWM/DE_MON_FM_MODE DEFAULT '1'
*"  EXPORTING
*"     REFERENCE(ET_DATA) TYPE  ZCUBI_WORKLIST_REVIEW_ITEM_TT
*"     REFERENCE(EV_RETURNCODE) TYPE  XFELD
*"     REFERENCE(EV_VARIANT) TYPE  VARIANT
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT OPTIONAL
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------
  DATA: lo_worklist                 TYPE REF TO zif_cubi_wrklst,
        ls_select_option            TYPE /iwbep/s_cod_select_option,
        lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
        ls_fcat                     TYPE lvc_s_fcat.

  FIELD-SYMBOLS:  <lfs_field> LIKE LINE OF ct_fieldcat.

  ls_select_option-sign = 'I'.
  ls_select_option-option = 'EQ'.
  ls_select_option-low = iv_lgnum.
  APPEND ls_select_option TO lt_warehouse_select_options.

  CALL METHOD zcl_cubi_wrklist_factory=>create
    RECEIVING
      ro_worklist = lo_worklist.

  CALL METHOD lo_worklist->query_review_items
    EXPORTING
      it_warehouse_select_options = lt_warehouse_select_options
    RECEIVING
      rt_items                    = et_data.

  LOOP AT ct_fieldcat ASSIGNING <lfs_field>.
    CASE <lfs_field>-fieldname.
      WHEN 'PRODUCT'.
        <lfs_field>-col_pos = '1'.
        <lfs_field>-coltext = 'Product Number'.
        <lfs_field>-seltext = 'Product Number'.
      WHEN 'UOM'.
        <lfs_field>-col_pos = '2'.
        <lfs_field>-coltext = 'UOM'.
        <lfs_field>-seltext = 'UOM'.
      WHEN 'LENGTH'.
        <lfs_field>-col_pos = '3'.
        <lfs_field>-coltext = 'Length'.
        <lfs_field>-seltext = 'Length'.
      WHEN 'LENGTH_OLD'.
        <lfs_field>-col_pos = '4'.
        <lfs_field>-coltext = 'Length(Old)'.
        <lfs_field>-seltext = 'Length(Old)'.
      WHEN 'WIDTH'.
        <lfs_field>-col_pos = '5'.
        <lfs_field>-coltext = 'Width'.
        <lfs_field>-seltext = 'Width'.
      WHEN 'WIDTH_OLD'.
        <lfs_field>-col_pos = '6'.
        <lfs_field>-coltext = 'Width(Old)'.
        <lfs_field>-seltext = 'Width(Old)'.
      WHEN 'HEIGHT'.
        <lfs_field>-col_pos = '7'.
        <lfs_field>-coltext = 'Height'.
        <lfs_field>-seltext = 'Height'.
      WHEN 'HEIGHT_OLD'.
        <lfs_field>-col_pos = '8'.
        <lfs_field>-coltext = 'Height(Old)'.
        <lfs_field>-seltext = 'Height(Old)'.
      WHEN 'MEABM'.
        <lfs_field>-col_pos = '9'.
        <lfs_field>-coltext = 'Dimension UOM'.
        <lfs_field>-seltext = 'Dimension UOM'.
      WHEN 'G_WEIGHT'.
        <lfs_field>-col_pos = '10'.
        <lfs_field>-coltext = 'Gross Weight'.
        <lfs_field>-seltext = 'Gross Weight'.
      WHEN 'G_WEIGHT_OLD'.
        <lfs_field>-col_pos = '11'.
        <lfs_field>-coltext = 'Gross Weight(Old)'.
        <lfs_field>-seltext = 'Gross Weight(Old)'.
      WHEN 'GEWEI'.
        <lfs_field>-col_pos = '12'.
        <lfs_field>-coltext = 'Weight UOM'.
        <lfs_field>-seltext = 'Weight UOM'.
      WHEN 'VOLUME'.
        <lfs_field>-col_pos = '13'.
        <lfs_field>-coltext = 'Volume'.
        <lfs_field>-seltext = 'Volume'.
      WHEN 'VOLUME_OLD'.
        <lfs_field>-col_pos = '14'.
        <lfs_field>-coltext = 'Volume(Old)'.
        <lfs_field>-seltext = 'Volume(Old)'.
      WHEN 'VOLEH'.
        <lfs_field>-col_pos = '15'.
        <lfs_field>-coltext = 'Volume UOM'.
        <lfs_field>-seltext = 'Volume UOM'.
      WHEN 'UPDATED_BY'.
        <lfs_field>-col_pos = '16'.
        <lfs_field>-coltext = 'Last Updated By'.
        <lfs_field>-seltext = 'Last Updated By'.
      WHEN 'UPDATED_TIMESTAMP'.
        <lfs_field>-col_pos = '17'.
        <lfs_field>-tech = abap_false.
        <lfs_field>-coltext = 'Last Updated'.
        <lfs_field>-seltext = 'Last Updated'.
      WHEN 'STATUS_TEXT'.
        <lfs_field>-col_pos = '18'.
        <lfs_field>-coltext = 'Status'.
        <lfs_field>-seltext = 'Status'.
      WHEN 'HAS_EXCEPTIONS'.
        <lfs_field>-col_pos = '19'.
        <lfs_field>-coltext = 'Has Exceptions'.
        <lfs_field>-seltext = 'Has Exceptions'.
      WHEN OTHERS.
        <lfs_field>-no_out = abap_true.
    ENDCASE.
  ENDLOOP.
ENDFUNCTION.
