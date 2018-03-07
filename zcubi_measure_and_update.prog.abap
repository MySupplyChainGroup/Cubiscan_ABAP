*&---------------------------------------------------------------------*
*& Report ZCUBI_MEASURE_AND_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcubi_measure_and_update.

CALL SCREEN 1000.

INCLUDE zcubi_measure_and_update_top.
INCLUDE zcubi_measure_and_update_mods.
INCLUDE zcubi_measure_and_update_form.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1000 OUTPUT.
  gv_progname = sy-repid.
  gv_dynnum   = sy-dynnr.

  " Build out the field catalog if it is not already built, this means
  " that it is the initial load of the screen.
  IF gt_worklist_fieldcat[] IS INITIAL.
    PERFORM get_status_exclusions.
    SET PF-STATUS 'ZCUBI_STATUS' EXCLUDING gt_status_exclusions.
    SET TITLEBAR 'ZCUBI_TITLE'.

    IF go_worklist IS NOT BOUND.
      " Instantiate the proper worklist implementation, left to the factory.
      go_worklist = zcl_cubi_wrklist_factory=>create( ).
    ENDIF.

    PERFORM prepare_worklist_field_catalog
                  CHANGING
                     gt_worklist_fieldcat.

    PERFORM prepare_excp_field_catalog
                CHANGING
                   gt_exceptions_fieldcat.

    PERFORM load_alv_exclusions(zcubi_measure_and_update)
                CHANGING
                   gt_alv_functions.

    CALL SCREEN 1004 STARTING AT 35 15.
  ENDIF.

  " Clear table for the worklist.
  CLEAR gt_worklist[].

  " Load data for worklist.
  PERFORM load_worklist_data.
  PERFORM display_worklist.

  LOOP AT SCREEN.
    IF gv_product_number_valid NE abap_true.
      IF screen-group1 EQ gc_after_validation.
        IF screen-name EQ gc_validation_button.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      ENDIF.
      IF screen-group1 EQ gc_after_measure.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ELSE.
      " Product number is valid.
      " Enable/Disable the after validation buttons.
      IF screen-group1 EQ gc_after_validation.
        " After validation group.
        IF gv_product_data_loaded EQ abap_true.
          " Product data loaded.
          IF screen-name EQ gc_validation_button.
            " Disabled
            screen-input = '0'.
          ELSE.
            " Enabled
            screen-input = '1'.
          ENDIF.
        ELSE.
          " Product data not loaded.
          IF screen-name EQ gc_validation_button.
            " Enabled
            screen-input = '1'.
          ELSE.
            " Disabled
            screen-input = '0'.
          ENDIF.
        ENDIF.
      ENDIF.

      " Disable the inputs after the product number and UOM are input and verified.
      IF screen-group2 EQ gc_material_input AND gv_product_data_loaded EQ abap_true.
        " Disabled
        screen-input = '0'.
      ENDIF.

      " Disable the Update button if the Cubiscan Measurement has not been completed.
      IF screen-group2 EQ gc_after_measure.
        IF gv_cubiscan_scan_complete EQ abap_true.
          CASE screen-name.
            WHEN gc_measure_button.
              " Disabled
              screen-input = '0'.
            WHEN gc_update_button OR gc_exceptions_button.
              " Enabled
              screen-input = '1'.
          ENDCASE.
        ELSE.
          CASE screen-name.
            WHEN gc_update_button OR gc_exceptions_button.
              " Disabled
              screen-input = '0'.
          ENDCASE.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1000 INPUT.
  CLEAR gv_valid.

  gv_ok_code = sy-ucomm.

  CASE gv_ok_code.
*-- When Fcode is Back,Cancel and Exit
    WHEN  gc_cancel OR gc_back OR gc_exit.
      LEAVE TO SCREEN 0.
    WHEN gc_validate.
      " Load the data for the product number.
      PERFORM load_product_data
                  USING
                     zcubi_cubiscan_data-material
                     zcubi_cubiscan_data-unit_of_measure
                  CHANGING
                     zcubi_product_data.
    WHEN 'CLEAR'.
      PERFORM clear_data.
    WHEN 'ADHOC'.
      CALL SCREEN 1003 STARTING AT 35 15.
    WHEN 'CHGWH'.
      CALL SCREEN 1004 STARTING AT 35 15.
    WHEN 'EXCP'.
      CALL SCREEN 1006 STARTING AT 35 15.
    WHEN 'MEASURE'.
      CALL SCREEN 1005 STARTING AT 35 15.
    WHEN 'UPDATE'.
      PERFORM update_cubiscan_record
                 USING
                    zcubi_cubiscan_data
                 CHANGING
                    gv_success.
      IF gv_success NE abap_true.
        " Raise error message.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '007'.
      ELSE.
        PERFORM clear_data.
      ENDIF.
    WHEN 'DISC'.
      PERFORM flag_discrepancy
                  USING
                     zcubi_cubiscan_data
                  CHANGING
                     gv_success.
      IF gv_success NE abap_true.
        " Raise error message.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '007'.
      ELSE.
        PERFORM clear_data.
      ENDIF.
  ENDCASE.
ENDMODULE.
