*&---------------------------------------------------------------------*
*&  Include           ZCUBI_MEASURE_AND_UPDATE_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_worklist_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_worklist_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      delayed_change_select
        FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid,
      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.
ENDCLASS.                    "lcl_worklist_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_amc_message_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_amc_message_receiver DEFINITION
FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Interface for AMC messages of type TEXT
    INTERFACES if_amc_message_receiver_text .
ENDCLASS.                   "lcl_amc_message_receiver DEFINITION

CONSTANTS : gc_back                   TYPE char4    VALUE 'BACK',      " Function code for Back Button
            gc_exit                   TYPE char4    VALUE 'EXIT',   " Function code for Exit Button
            gc_cancel                 TYPE char4    VALUE 'CANC',   " Function code for Cancel Button
            gc_validate               TYPE char10   VALUE 'VALIDATE',   " Function code for Validate data Button
            gc_validation_type_adhoc  TYPE char1    VALUE 'A',
            gc_validation_type_normal TYPE char1    VALUE 'N',
            gc_validation_button      TYPE char20   VALUE 'VALIDATE_BUTTON',
            gc_measure_button         TYPE char20   VALUE 'MEASURE_BUTTON',
            gc_update_button          TYPE char20   VALUE 'UPDATE_BUTTON',
            gc_exceptions_button      TYPE char20   VALUE 'EXCEPTIONS_BUTTON',
            gc_after_validation       TYPE char5    VALUE 'AV',
            gc_material_input         TYPE char5    VALUE 'INP',
            gc_after_measure          TYPE char5    VALUE 'AM',
            gc_status_ok              TYPE zcubi_tolerance_status VALUE 1,
            gc_status_caution         TYPE zcubi_tolerance_status VALUE 2,
            gc_status_error           TYPE zcubi_tolerance_status VALUE 3.

TYPES: BEGIN OF type_uom_lookup,
         matnr TYPE matnr,
         meinh TYPE meinh,
       END OF type_uom_lookup.

TYPES: gtyp_worklist_tt TYPE STANDARD TABLE OF zcubi_worklist.

TYPES: BEGIN OF type_functions,
         fcode LIKE rsmpe-func,
       END OF type_functions.

DATA: gs_marm                       TYPE /sapapo/marm,
      go_worklist_grid              TYPE REF TO cl_gui_alv_grid,
      go_worklist_handler           TYPE REF TO lcl_worklist_event_receiver,
      go_worklist_container         TYPE REF TO cl_gui_custom_container,
      gt_worklist                   TYPE zcubi_worklist_item_tt,
      gt_ui_worklist                TYPE zcubi_worklist_item_tt,
      gs_worklist_layout            TYPE lvc_s_layo,
      gt_worklist_fieldcat          TYPE lvc_t_fcat,
      go_exceptions_container       TYPE REF TO cl_gui_custom_container,
      go_exceptions_grid            TYPE REF TO cl_gui_alv_grid,
      gs_exceptions_layout          TYPE lvc_s_layo,
      gt_exceptions_fieldcat        TYPE lvc_t_fcat,
      gt_exceptions                 TYPE STANDARD TABLE OF zcubi_worklist_item_exception,
      gv_progname                   TYPE sy-repid,
      gv_dynnum                     TYPE sy-dynnr,
      gt_uom_lookup_data            TYPE STANDARD TABLE OF type_uom_lookup,
      gv_product_number_valid       TYPE boolean VALUE abap_false,
      gv_product_uom_valid          TYPE boolean VALUE abap_false,
      gv_product_data_loaded        TYPE boolean VALUE abap_false,
      gv_adhoc_product_number_valid TYPE boolean VALUE abap_false,
      gv_storage_bin_valid          TYPE boolean VALUE abap_false,
      gv_cubiscan_scan_complete     TYPE boolean VALUE abap_false,
      gv_valid                      TYPE boolean,
      gv_ok_code                    TYPE sy-ucomm,
      gv_success                    TYPE boolean,
      gs_function                   TYPE type_functions,
      gt_functions                  TYPE STANDARD TABLE OF type_functions,
      gt_alv_functions              TYPE ui_functions,
      gv_material_group             TYPE /sapapo/matkl,
      go_worklist                   TYPE REF TO zif_cubi_wrklst,
      gs_warehouse_filter           TYPE /iwbep/s_mgw_select_option,
      gt_cubiscan_devices           TYPE STANDARD TABLE OF zcubi_device,
      gt_amc_message_list           TYPE TABLE OF string,
      gt_status_exclusions          TYPE STANDARD TABLE OF sy-ucomm.

"DATA:      ok_code LIKE sy-ucomm.

TABLES: zcubi_worklist,
        zcubi_cubiscan_data,
        zcubi_product_data,
        zcubi_adhoc_data,
        zcubi_tolerances,
        zcubi_screen_data,
        zcubi_wrklst_exp.

*----------------------------------------------------------------------*
*       CLASS lcl_worklist_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_worklist_event_receiver IMPLEMENTATION.
  METHOD delayed_change_select.
    DATA: lt_rows TYPE lvc_t_row.

    FIELD-SYMBOLS: <fs_row>  TYPE lvc_s_row,
                   <fs_data> LIKE LINE OF gt_worklist.

    CALL METHOD go_worklist_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.
    READ TABLE lt_rows INDEX 1 ASSIGNING <fs_row>.
    IF <fs_row> IS ASSIGNED.
      READ TABLE gt_ui_worklist INDEX <fs_row>-index ASSIGNING <fs_data>.
      IF <fs_data> IS ASSIGNED.
        zcubi_cubiscan_data-material = <fs_data>-product.
        zcubi_cubiscan_data-unit_of_measure = <fs_data>-uom.

        PERFORM load_product_data
                  USING
                     zcubi_cubiscan_data-material
                     zcubi_cubiscan_data-unit_of_measure
                  CHANGING
                     zcubi_product_data.

        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = 'REFRESH'.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "delayed_change_selection

  METHOD handle_toolbar.
    DATA: ls_button TYPE stb_button.

    " Add a separator to the ALV toolbar.
    ls_button-butn_type = 3.
    APPEND ls_button TO e_object->mt_toolbar.

    " Add a refresh button to the ALV toolbar.
    ls_button-function  = 'REFRESH'.
    ls_button-icon      = icon_refresh.
    ls_button-quickinfo = 'Refresh'.
    ls_button-butn_type = 0.
    APPEND ls_button TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    IF e_ucomm EQ 'REFRESH'.
      PERFORM load_worklist_data.
      go_worklist_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "lcl_worklist_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_amc_message_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_amc_message_receiver IMPLEMENTATION.
  METHOD if_amc_message_receiver_text~receive.
    APPEND i_message TO gt_amc_message_list.
  ENDMETHOD.
ENDCLASS.                     "lcl_amc_message_receiver IMPLEMENTATION
