*&---------------------------------------------------------------------*
*&  Include           ZCUBI_MEASURE_AND_UPDATE_MODS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_UNIT_OF_MEASURE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_unit_of_measure INPUT.
  DATA: lv_mat_id TYPE /sapapo/matid.
  FIELD-SYMBOLS: <fs_entry> LIKE LINE OF gt_uom_lookup_data.

  " Select the mat id first.
  SELECT SINGLE matid FROM /sapapo/matkey
    INTO lv_mat_id
    WHERE matnr EQ zcubi_cubiscan_data-material.

  SELECT meinh FROM /sapapo/marm
    INTO CORRESPONDING FIELDS OF TABLE gt_uom_lookup_data
    WHERE matid EQ lv_mat_id.
  LOOP AT gt_uom_lookup_data ASSIGNING <fs_entry>.
    <fs_entry>-matnr = zcubi_cubiscan_data-material.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'meinh'
      dynpprog    = gv_progname
      dynpnr      = gv_dynnum
      dynprofield = 'ZCUBI_CUBISCAN_DATA-UNIT_OF_MEASURE'
      value_org   = 'S'
    TABLES
      value_tab   = gt_uom_lookup_data.
ENDMODULE.

MODULE validate_product_number INPUT.
  " First check if the sy-ucomm EQ 'CLEAR'.  Then we don't want to validate.
  " Validate that the product number is valid.
  IF sy-ucomm NE 'CLEAR' AND zcubi_cubiscan_data-material IS NOT INITIAL AND gv_product_number_valid NE abap_true.
    PERFORM validate_product_number
                USING
                   zcubi_screen_data-warehouse_number
                   'ZCUBI_CUBISCAN_DATA-UNIT_OF_MEASURE'
                   gc_validation_type_normal
                CHANGING
                   zcubi_cubiscan_data-material
                   gv_product_number_valid.
  ENDIF.
ENDMODULE.

MODULE adhoc_validate_product_number INPUT.
  " Validate that the product number is valid.
  IF ( sy-ucomm NE 'CANC' AND sy-ucomm NE 'CLEAR' ) AND zcubi_adhoc_data-material IS NOT INITIAL AND gv_adhoc_product_number_valid NE abap_true.
    PERFORM validate_product_number
                USING
                      zcubi_screen_data-warehouse_number
                      'ZCUBI_ADHOC_DATA-UNIT_OF_MEASURE'
                      gc_validation_type_adhoc
                CHANGING
                   zcubi_adhoc_data-material
                   gv_adhoc_product_number_valid.
  ENDIF.
ENDMODULE.

MODULE validate_adhoc_entry INPUT.
  " Validate the adhoc entry, make sure both fields are supplied.
  IF sy-ucomm EQ 'OK' AND gv_adhoc_product_number_valid EQ abap_true AND ( zcubi_adhoc_data-material IS INITIAL OR zcubi_adhoc_data-unit_of_measure IS INITIAL ).
    " Invalid...  Raise a message.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '016'.
  ENDIF.
ENDMODULE.

MODULE validate_worklist_entry INPUT.
  DATA: lv_valid   TYPE boolean,
        lv_product LIKE zcubi_cubiscan_data-material,
        lv_uom     LIKE zcubi_cubiscan_data-unit_of_measure.

  IF zcubi_cubiscan_data-material IS NOT INITIAL AND zcubi_cubiscan_data-unit_of_measure IS NOT INITIAL
    AND gv_product_number_valid EQ abap_true.
    PERFORM worklist_entry_exists
                USING
                   zcubi_cubiscan_data-material
                   zcubi_cubiscan_data-unit_of_measure
                CHANGING lv_valid.

    IF lv_valid NE abap_true.
      lv_product = zcubi_cubiscan_data-material.
      lv_uom = zcubi_cubiscan_data-unit_of_measure.

      CLEAR zcubi_cubiscan_data.
      MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '003' WITH lv_product lv_uom.
    ENDIF.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1003 INPUT.
  gv_ok_code = sy-ucomm.

  CASE gv_ok_code.
    WHEN 'CLEAR'.
      CLEAR zcubi_adhoc_data.
      gv_adhoc_product_number_valid = abap_false.
    WHEN 'CANC'.
      CLEAR zcubi_adhoc_data.
      gv_adhoc_product_number_valid = abap_false.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      PERFORM add_adhoc_entry
          USING
              zcubi_adhoc_data
              zcubi_screen_data-warehouse_number.
      CLEAR zcubi_adhoc_data.
      gv_adhoc_product_number_valid = abap_false.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1003 OUTPUT.
  SET PF-STATUS 'ZCUBI_DIALOG_STATUS'.
  SET TITLEBAR 'ZCUBI_TITLE'.

  LOOP AT SCREEN.
    IF gv_adhoc_product_number_valid NE abap_true.
      IF screen-group1 EQ 'PNV'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'PNV'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'PV'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_1006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1006 OUTPUT.
  SET PF-STATUS 'ZCUBI_DIALOG_STATUS'.
  SET TITLEBAR 'ZCUBI_TITLE'.

  PERFORM load_exception_data.
  PERFORM display_exceptions.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_1006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1006 INPUT.
  gv_ok_code = sy-ucomm.

  CASE gv_ok_code.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_1005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1005 OUTPUT.
  SET PF-STATUS 'ZCUBI_DIALOG_STATUS'.
  SET TITLEBAR 'ZCUBI_TITLE'.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode = '=ENT'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1005 INPUT.
  gv_ok_code = sy-ucomm.

  CASE gv_ok_code.
    WHEN 'ENT'.
      PERFORM trigger_cubiscan.
      PERFORM wait_for_cubiscan_message.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1004 INPUT.
  gv_ok_code = sy-ucomm.

  CASE gv_ok_code.
    WHEN  gc_cancel OR gc_back OR gc_exit.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      PERFORM set_and_initialize_warehouse.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1004 OUTPUT.
  SET PF-STATUS 'ZCUBI_DIALOG_STATUS'.
  SET TITLEBAR 'ZCUBI_TITLE'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDATE_WAREHOUSE_NUMBER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_warehouse_number INPUT.
  PERFORM validate_warehouse_number
              USING
                 zcubi_screen_data-warehouse_number
              CHANGING
                 gv_valid.

  IF gv_valid NE abap_true.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '010' WITH zcubi_screen_data-warehouse_number.
  ENDIF.

  CLEAR gv_valid.
ENDMODULE.
