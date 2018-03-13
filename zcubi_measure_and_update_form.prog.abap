*&---------------------------------------------------------------------*
*&  Include           ZCUBI_MEASURE_AND_UPDATE_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LOAD_PRODUCT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_product_data USING pv_product_number TYPE /sapapo/matnr pv_uom TYPE char3 CHANGING ps_product_data TYPE zcubi_product_data.
  DATA: lv_mat_id TYPE /sapapo/matid,
        ls_marm   TYPE /sapapo/marm,
        lv_uom    TYPE char3.

  " Select the mat id first.
  SELECT SINGLE matid matkl FROM /sapapo/matkey
    INTO (lv_mat_id, gv_material_group)
    WHERE matnr EQ pv_product_number.

  " Convert the UOM.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = pv_uom
    IMPORTING
      output = lv_uom.

  SELECT SINGLE * FROM /sapapo/marm
    INTO CORRESPONDING FIELDS OF ls_marm
    WHERE matid = lv_mat_id
      AND meinh = lv_uom.

  IF sy-subrc NE 0.
    zcubi_cubiscan_data-material = ''.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '000'.
  ELSE.
    " If this is a carton, then we need to see if the UOM's are supplied,
    " if not, then we need to query them from the 'EA' UOM.
    IF pv_uom EQ 'KAR' AND ( ls_marm-meabm IS INITIAL OR ls_marm-gewei IS INITIAL OR ls_marm-voleh IS INITIAL ).
      " Now lookup the info for 'EA'.
      SELECT SINGLE meabm gewei voleh FROM /sapapo/marm
        INTO CORRESPONDING FIELDS OF ls_marm
        WHERE matid = lv_mat_id
          AND meinh = 'EA'.
    ENDIF.

    ps_product_data-material = pv_product_number.
    ps_product_data-unit_of_measure = pv_uom.
    ps_product_data-length = ls_marm-laeng.
    ps_product_data-length_uom = ls_marm-meabm.
    ps_product_data-width = ls_marm-breit.
    ps_product_data-width_uom = ls_marm-meabm.
    ps_product_data-height = ls_marm-hoehe.
    ps_product_data-height_uom = ls_marm-meabm.
    ps_product_data-gross_weight = ls_marm-brgew.
    ps_product_data-gross_weight_uom = ls_marm-gewei.
    ps_product_data-net_weight = ls_marm-ntgew.
    ps_product_data-net_weight_uom = ls_marm-gewei.
    ps_product_data-volume = ls_marm-volum.
    ps_product_data-volume_uom = ls_marm-voleh.

    " Need to select the Case record and pull the numerator and denominator.
    " Hard coded.
    IF pv_uom EQ 'KAR'.
      SELECT SINGLE * FROM /sapapo/marm
        INTO CORRESPONDING FIELDS OF ls_marm
        WHERE matid = lv_mat_id
          AND meinh = 'KAR'.
      IF sy-subrc EQ 0.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'S' NUMBER '014'
          WITH ls_marm-umren ls_marm-umrez
          INTO ps_product_data-uom_conversion_txt.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM /sapapo/marm
        INTO CORRESPONDING FIELDS OF ls_marm
        WHERE matid = lv_mat_id
          AND meinh = 'CS'.
      IF sy-subrc EQ 0.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'S' NUMBER '009'
          WITH ls_marm-umren ls_marm-umrez
          INTO ps_product_data-uom_conversion_txt.
      ENDIF.
    ENDIF.

    gv_product_data_loaded = abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRIGGER_CUBISCAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_status_exclusions.
  DATA: lt_configuration TYPE zmscg_configuration_tt.

  FIELD-SYMBOLS:  <lfs_entry> LIKE LINE OF lt_configuration.

  CALL METHOD zcl_mscg_common_utils=>get_configuration
    EXPORTING
      iv_config_application = 'CUBISCAN'
      iv_config_type        = 2
    RECEIVING
      rt_configuration      = lt_configuration.

  READ TABLE lt_configuration ASSIGNING <lfs_entry> WITH KEY config_key = 'settings.allowManualCreation'.
  IF <lfs_entry> IS ASSIGNED.
    IF <lfs_entry>-config_value EQ 'false'.
      APPEND 'ADHOC' TO gt_status_exclusions.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRIGGER_CUBISCAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trigger_cubiscan.
  DATA: lv_uom TYPE /sapapo/meins.

  " Need to convert the UOM.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = zcubi_cubiscan_data-unit_of_measure
    IMPORTING
      output = lv_uom.

  CALL FUNCTION 'ZCUBI_POLL_CUBISCAN' STARTING NEW TASK 'CUBISCAN'
    EXPORTING
      iv_warehouse_number = zcubi_screen_data-warehouse_number
      iv_device_name      = zcubi_cubiscan_data-cubiscan_device
      iv_product          = zcubi_cubiscan_data-material
      iv_uom              = lv_uom.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WAIT_FOR_CUBISCAN_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM wait_for_cubiscan_message.
  TYPES: BEGIN OF ltyp_inbound_message,
           warehouse_number TYPE /scwm/lgnum,
           device_name      TYPE char40,
           product          TYPE /sapapo/matnr,
           uom              TYPE /sapapo/meins,
           object           TYPE char40,
           action           TYPE char40.
      INCLUDE TYPE zcubi_gate_measure_data.
  TYPES: END OF ltyp_inbound_message.

  DATA: lo_consumer  TYPE REF TO if_amc_message_consumer,
        lo_receiver  TYPE REF TO lcl_amc_message_receiver,
        lx_amc_error TYPE REF TO cx_amc_error,
        lv_message   TYPE string,
        ls_message   TYPE ltyp_inbound_message,
        lv_uom       TYPE /sapapo/meins.

  FIELD-SYMBOLS:  <lfs_cubiscan> LIKE LINE OF gt_cubiscan_devices.

  " Clear the message list.
  CLEAR gt_amc_message_list[].

  " Convert the UOM to internal format.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = zcubi_cubiscan_data-unit_of_measure
    IMPORTING
      output = lv_uom.

  " Read the Cubiscan device record.
  READ TABLE gt_cubiscan_devices ASSIGNING <lfs_cubiscan> WITH KEY device_name = zcubi_cubiscan_data-cubiscan_device.

  TRY.
      lo_consumer = cl_amc_channel_manager=>create_message_consumer( i_application_id = 'ZCUBI_MESSAGE_CHANNEL' i_channel_id = '/outbound' ).
      CREATE OBJECT lo_receiver.

      " Start of message delivery
      lo_consumer->start_message_delivery( i_receiver = lo_receiver ).
    CATCH cx_amc_error INTO lx_amc_error.
      MESSAGE lx_amc_error->get_text( ) TYPE 'E'.
  ENDTRY.

  " Wait until a message is received but not longer than waiting time in seconds
  WAIT FOR MESSAGING CHANNELS UNTIL lines( gt_amc_message_list ) >= 1 UP TO <lfs_cubiscan>-polling_period SECONDS.

  " Check the message.
  IF sy-subrc = 8 AND lines( gt_amc_message_list ) = 0.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'I' NUMBER '017'.
  ELSE.
    " Loop through the messages, use the one destined for us.
    LOOP AT gt_amc_message_list INTO lv_message.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = lv_message
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data = ls_message ).

      " If the product and UOM match, then fill the data.
      IF ls_message-object EQ 'cubiscanMeasurement' AND ls_message-product EQ zcubi_cubiscan_data-material AND ls_message-uom EQ lv_uom.
        " Check message type.
        CASE ls_message-action.
          WHEN 'connectError'.
            MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '004' WITH zcubi_cubiscan_data-cubiscan_device.
          WHEN 'timeout'.
            MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '006' WITH zcubi_cubiscan_data-cubiscan_device.
          WHEN 'success'.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input  = ls_message-dimension_uom
              IMPORTING
                output = ls_message-dimension_uom.

            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input  = ls_message-weight_uom
              IMPORTING
                output = ls_message-weight_uom.

            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input  = ls_message-volume_uom
              IMPORTING
                output = ls_message-volume_uom.

            zcubi_cubiscan_data-length = ls_message-length.
            PERFORM render_icon USING ls_message-length_status CHANGING zcubi_cubiscan_data-length_status_icon.

            zcubi_cubiscan_data-width = ls_message-width.
            PERFORM render_icon USING ls_message-width_status CHANGING zcubi_cubiscan_data-width_status_icon.

            zcubi_cubiscan_data-height = ls_message-height.
            PERFORM render_icon USING ls_message-height_status CHANGING zcubi_cubiscan_data-height_status_icon.

            zcubi_cubiscan_data-gross_weight = ls_message-gross_weight.
            PERFORM render_icon USING ls_message-gross_weight_status CHANGING zcubi_cubiscan_data-gross_weight_status_icon.

            zcubi_cubiscan_data-volume = ls_message-volume.
            PERFORM render_icon USING ls_message-volume_status CHANGING zcubi_cubiscan_data-volume_status_icon.

            zcubi_cubiscan_data-length_uom = ls_message-dimension_uom.
            zcubi_cubiscan_data-width_uom = ls_message-dimension_uom.
            zcubi_cubiscan_data-height_uom = ls_message-dimension_uom.
            zcubi_cubiscan_data-gross_weight_uom = ls_message-weight_uom.
            zcubi_cubiscan_data-net_weight_uom = ls_message-weight_uom.
            zcubi_cubiscan_data-volume_uom = ls_message-volume_uom.

            " Set the scan complete flag and exit the modal window.
            gv_cubiscan_scan_complete = abap_true.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_WAREHOUSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_and_initialize_warehouse.
  DATA: ls_select_option TYPE /iwbep/s_cod_select_option,
        lt_list_values   TYPE vrm_values,
        ls_value         TYPE vrm_value.

  FIELD-SYMBOLS: <lfs_device> LIKE LINE OF gt_cubiscan_devices,
                 <lfs_value>  LIKE LINE OF lt_list_values.

  CLEAR gs_warehouse_filter-select_options[].
  CLEAR gt_cubiscan_devices[].
  CLEAR zcubi_cubiscan_data-cubiscan_device.

  ls_select_option-sign = 'I'.
  ls_select_option-option = 'EQ'.
  ls_select_option-low = zcubi_screen_data-warehouse_number.
  APPEND ls_select_option TO gs_warehouse_filter-select_options.

  " Load Cubiscan Devices.
  SELECT * FROM zcubi_devices
      INTO CORRESPONDING FIELDS OF TABLE gt_cubiscan_devices
      WHERE warehouse_number IN gs_warehouse_filter-select_options
      ORDER BY device_name.

  LOOP AT gt_cubiscan_devices ASSIGNING <lfs_device>.
    ls_value-key = <lfs_device>-device_name.
    ls_value-text = <lfs_device>-device_name.
    APPEND ls_value TO lt_list_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'ZCUBI_CUBISCAN_DATA-CUBISCAN_DEVICE'
      values          = lt_list_values
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

  IF zcubi_cubiscan_data-cubiscan_device IS INITIAL.
    IF lines( lt_list_values ) GT 0.
      READ TABLE lt_list_values ASSIGNING <lfs_value> INDEX 1.
      zcubi_cubiscan_data-cubiscan_device = <lfs_value>-key.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_WORKLIST_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_worklist_field_catalog CHANGING pt_fieldcat TYPE lvc_t_fcat.
  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'PRODUCT'.
  ls_fcat-ref_field = '/SCWM/DE_MATNR'.
  ls_fcat-outputlen = '40'.
  ls_fcat-coltext = 'Product Number'.
  ls_fcat-seltext = 'Product Number'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'UOM'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = 'Unit of Measure'.
  ls_fcat-seltext = 'Unit of Measure'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'STATUS_TEXT'.
  ls_fcat-outputlen = '30'.
  ls_fcat-coltext = 'Status'.
  ls_fcat-seltext = 'Status'.
  APPEND ls_fcat TO pt_fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_EXCEPTIONS_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_excp_field_catalog CHANGING pt_fieldcat TYPE lvc_t_fcat.
  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'ITEM_EXCEPTION'.
  ls_fcat-ref_field = 'ZCUBI_EXCEPTION'.
  ls_fcat-outputlen = '40'.
  ls_fcat-coltext = 'Exception'.
  ls_fcat-seltext = 'Exception'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'EXCEPTION_MESSAGE'.
  ls_fcat-ref_field = 'ZCUBI_EXCEPTION_MESSAGE'.
  ls_fcat-outputlen = '100'.
  ls_fcat-coltext = 'Exception Message'.
  ls_fcat-seltext = 'Exception Message'.
  APPEND ls_fcat TO pt_fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_GRID_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_grid_layout USING pv_title TYPE lvc_title CHANGING ps_layout TYPE lvc_s_layo.
  ps_layout-grid_title = pv_title.
  ps_layout-sel_mode = 'B'.
  ps_layout-zebra = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOAD_WORKLIST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_worklist_data.
  DATA: ls_select_option TYPE /iwbep/s_cod_select_option,
        ls_status_filter TYPE /iwbep/s_mgw_select_option.

  FIELD-SYMBOLS: <lfs_worklist> LIKE LINE OF gt_ui_worklist.

  " New Entries
  ls_select_option-sign = 'I'.
  ls_select_option-option = 'EQ'.
  ls_select_option-low = 'N'.
  APPEND ls_select_option TO ls_status_filter-select_options.

  " Re-measure Entries.
  ls_select_option-sign = 'I'.
  ls_select_option-option = 'EQ'.
  ls_select_option-low = 'R'.
  APPEND ls_select_option TO ls_status_filter-select_options.

  CALL METHOD go_worklist->query_items
    EXPORTING
      it_warehouse_select_options = gs_warehouse_filter-select_options
      it_status_select_options    = ls_status_filter-select_options
    RECEIVING
      rt_items                    = gt_worklist.

  gt_ui_worklist[] = gt_worklist[].
  LOOP AT gt_ui_worklist ASSIGNING <lfs_worklist>.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = <lfs_worklist>-uom
      IMPORTING
        output = <lfs_worklist>-uom.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOAD_EXCEPTION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_exception_data.
  DATA: lv_uom TYPE /sapapo/meins.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = zcubi_cubiscan_data-unit_of_measure
    IMPORTING
      output = lv_uom.

  SELECT * FROM zcubi_wrklst_exp
    INTO CORRESPONDING FIELDS OF TABLE gt_exceptions
    WHERE warehouse EQ zcubi_screen_data-warehouse_number
      AND product EQ zcubi_cubiscan_data-material
      AND uom EQ lv_uom.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_WORKLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_worklist.
  IF go_worklist_container IS INITIAL.
    CREATE OBJECT go_worklist_container
      EXPORTING
        container_name = 'WORKLIST_CONTAINER'.

    CREATE OBJECT go_worklist_grid
      EXPORTING
        i_parent = go_worklist_container.

    PERFORM prepare_grid_layout
                USING
                   'Worklist'
                CHANGING
                   gs_worklist_layout.

    CALL METHOD go_worklist_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = space
        i_bypassing_buffer            = abap_true
        i_save                        = space
        is_layout                     = gs_worklist_layout
        it_toolbar_excluding          = gt_alv_functions
      CHANGING
        it_outtab                     = gt_ui_worklist
        it_fieldcatalog               = gt_worklist_fieldcat
*       it_sort                       =
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

    CREATE OBJECT go_worklist_handler.
    SET HANDLER go_worklist_handler->delayed_change_select FOR go_worklist_grid.
    SET HANDLER go_worklist_handler->handle_user_command FOR go_worklist_grid.
    SET HANDLER go_worklist_handler->handle_toolbar FOR go_worklist_grid.

    CALL METHOD go_worklist_grid->register_delayed_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select.
    CALL METHOD go_worklist_grid->set_toolbar_interactive.
  ELSE.
    CALL METHOD go_worklist_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*--Exception handling
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_EXCEPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_exceptions.
  IF go_exceptions_container IS INITIAL.
    CREATE OBJECT go_exceptions_container
      EXPORTING
        container_name = 'EXCEPTIONS_CONTAINER'.

    CREATE OBJECT go_exceptions_grid
      EXPORTING
        i_parent = go_exceptions_container.

    PERFORM prepare_grid_layout
                USING
                   'Exceptions'
                CHANGING
                   gs_exceptions_layout.

    CALL METHOD go_exceptions_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = space
        i_bypassing_buffer            = abap_true
        i_save                        = space
        is_layout                     = gs_exceptions_layout
        it_toolbar_excluding          = gt_alv_functions
      CHANGING
        it_outtab                     = gt_exceptions
        it_fieldcatalog               = gt_exceptions_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  ELSE.
    CALL METHOD go_exceptions_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
  DATA: lv_device TYPE char40.

  lv_device = zcubi_cubiscan_data-cubiscan_device.

  CLEAR zcubi_cubiscan_data.
  CLEAR zcubi_product_data.
  CLEAR zcubi_tolerances.

  CALL FUNCTION 'VRM_DELETE_VALUES'
    EXPORTING
      id = 'ZCUBI_CUBISCAN_DATA-UNIT_OF_MEASURE'.

  zcubi_cubiscan_data-cubiscan_device = lv_device.
  gv_product_number_valid = abap_false.
  gv_product_data_loaded = abap_false.
  gv_cubiscan_scan_complete = abap_false.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOOKUP_PRODUCT_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lookup_product_number USING pv_product_number TYPE char40 CHANGING pv_value TYPE char40.
  DATA: lv_mat_id         TYPE /sapapo/matid,
        lv_product_number TYPE char40,
        lt_records        TYPE STANDARD TABLE OF char40.

  " First, we need to select from MATKEY, if we can't find it there, then
  " we need to look at the old material number in MATKEY.
  SELECT SINGLE matnr FROM /sapapo/matkey
    INTO pv_value WHERE matnr EQ pv_product_number.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

  " Still not found, try looking it up by EAN/UPC number.
  SELECT SINGLE matid FROM /sapapo/marm
    INTO lv_mat_id
    WHERE ean11 EQ pv_product_number.
  IF sy-subrc IS INITIAL.
    " Found a record.  Now, look up the material number from MATKEY.
    SELECT SINGLE matnr FROM /sapapo/matkey
      INTO pv_value
      WHERE matid EQ lv_mat_id.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_PRODUCT_NUMBER
*&---------------------------------------------------------------------*
*       Validates the product number and then updates it in the
*       structure as well.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_product_number
    USING
          pv_warehouse_number TYPE /scwm/lgnum
          pv_control_id TYPE vrm_id
          pv_validation_type TYPE char1
    CHANGING
      pv_product_number TYPE char40
      pv_product_number_valid TYPE boolean.

  DATA: lv_product_number          TYPE char40,
        lv_mat_id                  TYPE /sapapo/matid,
        lt_uom                     TYPE zcubi_product_uom_tt,
        lt_list_values             TYPE vrm_values,
        ls_value                   TYPE vrm_value,
        lv_count                   TYPE i,
        lt_cubiscan_entries        TYPE zcubi_worklist_item_tt,
        lv_valid                   TYPE boolean VALUE abap_true,
        lv_filter_worklist_entries TYPE boolean VALUE abap_true.

  FIELD-SYMBOLS: <lfs_uom>   LIKE LINE OF lt_uom,
                 <lfs_entry> LIKE LINE OF gt_worklist,
                 <lfs_value> LIKE LINE OF lt_list_values.

  PERFORM lookup_product_number
              USING
                 pv_product_number
              CHANGING
                 lv_product_number.
  IF lv_product_number IS NOT INITIAL.
    pv_product_number = lv_product_number.

    " Check to see if the product exists in the cubiscan table.
    LOOP AT gt_worklist ASSIGNING <lfs_entry> WHERE product = lv_product_number.
      APPEND <lfs_entry> TO lt_cubiscan_entries.
    ENDLOOP.

    " Check which validation we are doing.
    IF pv_validation_type EQ gc_validation_type_normal.
      lv_filter_worklist_entries = abap_false.

      IF lines( lt_cubiscan_entries ) EQ 0.
        pv_product_number_valid = abap_false.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '004' WITH lv_product_number.
        lv_valid = abap_false.
      ENDIF.
    ENDIF.

    IF lv_valid EQ abap_true.
      pv_product_number_valid = abap_true.

      IF lv_filter_worklist_entries EQ abap_true.
        CALL METHOD go_worklist->query_uoms_not_in_worklist
          EXPORTING
            iv_warehouse_number = pv_warehouse_number
            iv_product_number   = pv_product_number
          IMPORTING
            et_uoms             = lt_uom.
      ELSE.
        CALL METHOD go_worklist->query_uoms_in_worklist
          EXPORTING
            iv_warehouse_number = pv_warehouse_number
            iv_product_number   = pv_product_number
          IMPORTING
            et_uoms             = lt_uom.
      ENDIF.

      IF lines( lt_uom ) EQ 0.
        pv_product_number_valid = abap_false.
        MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '005' WITH pv_product_number.
      ELSE.
        LOOP AT lt_uom ASSIGNING <lfs_uom>.
          ls_value-key = <lfs_uom>-uom_key.
          ls_value-text = <lfs_uom>-uom_key.
          APPEND ls_value TO lt_list_values.
        ENDLOOP.

        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id              = pv_control_id
            values          = lt_list_values
          EXCEPTIONS
            id_illegal_name = 0
            OTHERS          = 0.

        IF zcubi_cubiscan_data-unit_of_measure IS INITIAL.
          READ TABLE lt_list_values ASSIGNING <lfs_value> INDEX 1.
          zcubi_cubiscan_data-unit_of_measure = <lfs_value>-key.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    pv_product_number_valid = abap_false.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '000'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_ADHOC_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_adhoc_entry USING ps_adhoc_data TYPE zcubi_adhoc_data pv_warehouse_number TYPE /scwm/lgnum.
  DATA: ls_adhoc_entry TYPE zcubi_worklist,
        lv_timestamp   TYPE timestamp.

  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.

  " Make sure this record does not exist as an 'N' or 'R' record.  If it does, then we should
  " not insert.
  SELECT SINGLE * FROM zcubi_worklist INTO ls_adhoc_entry
    WHERE warehouse EQ pv_warehouse_number AND product EQ ps_adhoc_data-material
      AND uom EQ ps_adhoc_data-unit_of_measure AND status IN ('N', 'R').
  IF sy-subrc IS INITIAL.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '012'
      WITH ps_adhoc_data-material ps_adhoc_data-unit_of_measure.
    EXIT.
  ENDIF.

  " Make sure this record does not exist as a 'U' record.  If it does, then we should
  " not insert.
  SELECT SINGLE * FROM zcubi_worklist INTO ls_adhoc_entry
    WHERE warehouse EQ pv_warehouse_number AND product EQ ps_adhoc_data-material
      AND uom EQ ps_adhoc_data-unit_of_measure AND status IN ('S').
  IF sy-subrc IS INITIAL.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '015'
      WITH ps_adhoc_data-material ps_adhoc_data-unit_of_measure.
    EXIT.
  ENDIF.

  ls_adhoc_entry-warehouse = pv_warehouse_number.
  ls_adhoc_entry-product = ps_adhoc_data-material.
  ls_adhoc_entry-uom = ps_adhoc_data-unit_of_measure.
  ls_adhoc_entry-createuser = sy-uname.
  ls_adhoc_entry-createutc = lv_timestamp.
  ls_adhoc_entry-status = 'N'.

  MODIFY zcubi_worklist FROM ls_adhoc_entry.
  IF sy-subrc NE 0.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'E' NUMBER '002'.
  ELSE.
    MESSAGE ID 'ZCUBI_DIALOG_SCREEN' TYPE 'S' NUMBER '001'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WORKLIST_ENTRY_EXISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM worklist_entry_exists USING pv_product_number TYPE char40 pv_uom TYPE /sapapo/lrmei CHANGING pv_valid TYPE boolean.
  DATA: lv_count TYPE i.

  " Check to see if the product exists in the cubiscan table.
  SELECT COUNT(*) FROM zcubi_worklist
    INTO lv_count
    WHERE product EQ pv_product_number AND uom EQ pv_uom.
  IF lv_count EQ 0.
    pv_valid = abap_false.
  ELSE.
    pv_valid = abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RENDER_ICON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM render_icon USING pv_status TYPE zcubi_tolerance_status CHANGING pv_field TYPE icon_text.
  DATA: lv_icon_name TYPE char20.

  CASE pv_status.
    WHEN gc_status_ok.
      lv_icon_name = 'ICON_GREEN_LIGHT'.
    WHEN gc_status_caution.
      lv_icon_name = 'ICON_YELLOW_LIGHT'.
    WHEN gc_status_error.
      lv_icon_name = 'ICON_RED_LIGHT'.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_icon_name
      text                  = ''
      info                  = 'Status'
      add_stdinf            = 'X'
    IMPORTING
      result                = pv_field
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CUBISCAN_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_cubiscan_record USING ps_cubiscan_data TYPE zcubi_cubiscan_data CHANGING pv_success.
  DATA: lv_success  TYPE boolean,
        lt_messages TYPE bapiret2_t,
        lv_uom      TYPE /sapapo/meins.

  FIELD-SYMBOLS:  <lfs_worklist> LIKE LINE OF gt_worklist.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = ps_cubiscan_data-unit_of_measure
    IMPORTING
      output = lv_uom.

  " Read from the worklist.
  READ TABLE gt_worklist ASSIGNING <lfs_worklist> WITH KEY product = ps_cubiscan_data-material uom = lv_uom.
  IF <lfs_worklist> IS ASSIGNED.
    <lfs_worklist>-status = 'S'.

    CALL METHOD go_worklist->update_item
      EXPORTING
        is_worklist_item = <lfs_worklist>
      IMPORTING
        ev_success       = pv_success
        et_messages      = lt_messages.
  ELSE.
    pv_success = abap_false.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FLAG_DISCREPANCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM flag_discrepancy USING ps_cubiscan_data TYPE zcubi_cubiscan_data CHANGING pv_success.
  DATA: lv_success  TYPE boolean,
        lt_messages TYPE bapiret2_t,
        lv_uom      TYPE /sapapo/meins.

  FIELD-SYMBOLS:  <lfs_worklist> LIKE LINE OF gt_worklist.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input  = ps_cubiscan_data-unit_of_measure
    IMPORTING
      output = lv_uom.

  " Read from the worklist.
  READ TABLE gt_worklist ASSIGNING <lfs_worklist> WITH KEY product = ps_cubiscan_data-material uom = lv_uom.
  IF <lfs_worklist> IS ASSIGNED.
    <lfs_worklist>-status = 'R'.

    CALL METHOD go_worklist->update_item
      EXPORTING
        is_worklist_item = <lfs_worklist>
      IMPORTING
        ev_success       = pv_success
        et_messages      = lt_messages.
  ELSE.
    pv_success = abap_false.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_WAREHOUSE_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_warehouse_number USING pv_warehouse_number TYPE /scwm/lgnum CHANGING pv_valid TYPE boolean.
  DATA: lv_count TYPE i.

  SELECT COUNT(*) INTO lv_count
    FROM /scwm/t300
    WHERE lgnum EQ pv_warehouse_number.
  IF sy-subrc NE 0.
    pv_valid = abap_false.
  ELSE.
    pv_valid = abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOAD_ALV_EXCLUSIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_alv_exclusions CHANGING pt_exclusions TYPE ui_functions.
  DATA: ls_fun TYPE ui_func.

  ls_fun = cl_gui_alv_grid=>mc_fc_excl_all.
  APPEND ls_fun TO pt_exclusions.
ENDFORM.
