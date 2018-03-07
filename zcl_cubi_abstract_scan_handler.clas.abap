class ZCL_CUBI_ABSTRACT_SCAN_HANDLER definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_CUBI_SCAN_HANDLER
      abstract methods CONNECT
                       DISCONNECT
                       START_POLLING .

  methods CONSTRUCTOR
    importing
      !IS_CUBISCAN_DEVICE type ZCUBI_DEVICE
      !IV_PRODUCT type /SAPAPO/MATNR
      !IV_UOM type /SAPAPO/MEINS .
PROTECTED SECTION.

  TYPES:
    BEGIN OF ltyp_outbound_message,
      warehouse_number TYPE /scwm/lgnum,
      device_name      TYPE char40,
      product          TYPE /sapapo/matnr,
      uom              TYPE /sapapo/meins,
      object           TYPE char40,
      action           TYPE char40.
      INCLUDE TYPE zcubi_gate_measure_data.
  TYPES: END OF ltyp_outbound_message .

  DATA gv_message TYPE string .
  DATA gv_log_handle TYPE balloghndl .
  DATA gs_cubiscan_device TYPE zcubi_device .
  DATA gv_product TYPE /sapapo/matnr .
  DATA gv_uom TYPE /sapapo/meins .
  DATA gt_log_handles TYPE bal_t_logh .
  DATA gs_gate_measure_data TYPE zcubi_gate_measure_data .
  CONSTANTS gc_object TYPE string VALUE 'cubiscanMeasurement' ##NO_TEXT.
  CONSTANTS gc_action_success TYPE string VALUE 'success' ##NO_TEXT.
  CONSTANTS gc_action_timeout TYPE string VALUE 'timeout' ##NO_TEXT.
  CONSTANTS gc_action_connect_error TYPE string VALUE 'connectError' ##NO_TEXT.

  METHODS send_outbound_message
    IMPORTING
      !iv_warehouse_number  TYPE /scwm/lgnum
      !iv_device_name       TYPE char40
      !iv_product           TYPE /sapapo/matnr
      !iv_uom               TYPE /sapapo/meins
      !iv_object            TYPE string
      !iv_action            TYPE string
      !is_gate_measure_data TYPE zcubi_gate_measure_data OPTIONAL
    RAISING
      zcx_cubiscan_exception .
private section.
ENDCLASS.



CLASS ZCL_CUBI_ABSTRACT_SCAN_HANDLER IMPLEMENTATION.


  METHOD constructor.
    DATA: ls_log         TYPE bal_s_log.

    " Persist the Cubiscan device information to the global variable.
    gs_cubiscan_device = is_cubiscan_device.
    gv_product = iv_product.
    gv_uom = iv_uom.

    " Create the logging handle.
    ls_log-object = 'ZCUBISCAN'.
    ls_log-subobject = 'TCP_CLIENT'.
    ls_log-aluser = sy-uname.
    ls_log-alprog = sy-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = gv_log_handle.
    APPEND gv_log_handle TO gt_log_handles.
  ENDMETHOD.


  METHOD send_outbound_message.
    DATA: lv_outbound_message TYPE string,
          lv_json             TYPE string,
          lo_producer_text    TYPE REF TO if_amc_message_producer_text,
          lx_amc_error        TYPE REF TO cx_amc_error,
          ls_outbound_message TYPE ltyp_outbound_message,
          ls_msg              TYPE bal_s_msg.

    ls_outbound_message-warehouse_number = iv_warehouse_number.
    ls_outbound_message-device_name = iv_device_name.
    ls_outbound_message-product = iv_product.
    ls_outbound_message-uom = iv_uom.
    ls_outbound_message-object = iv_object.
    ls_outbound_message-action = iv_action.
    IF is_gate_measure_data IS SUPPLIED.
      MOVE-CORRESPONDING is_gate_measure_data TO ls_outbound_message.
    ENDIF.

    " Do some cleanup of the data.
    TRANSLATE ls_outbound_message-dimension_uom TO UPPER CASE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_outbound_message-dimension_uom
      IMPORTING
        output = ls_outbound_message-dimension_uom.

    TRANSLATE ls_outbound_message-weight_uom TO UPPER CASE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_outbound_message-weight_uom
      IMPORTING
        output = ls_outbound_message-weight_uom.

    TRANSLATE ls_outbound_message-volume_uom TO UPPER CASE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_outbound_message-volume_uom
      IMPORTING
        output = ls_outbound_message-volume_uom.

    " serialize data into JSON, skipping initial fields and converting ABAP field names into camelCase
    lv_json = /ui2/cl_json=>serialize( data = ls_outbound_message compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    TRY.
        lo_producer_text ?= cl_amc_channel_manager=>create_message_producer( i_application_id = 'ZCUBI_MESSAGE_CHANNEL' i_channel_id = '/outbound' ).

        " Send message to the amc channel
        lo_producer_text->send( i_message = lv_json ).
      CATCH cx_amc_error INTO lx_amc_error.
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '013'.
        ls_msg-msgv1 = gs_cubiscan_device-device_name.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg.

        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_save_all     = abap_true
            i_t_log_handle = gt_log_handles.

        RAISE EXCEPTION TYPE zcx_cubiscan_exception
          EXPORTING
            previous = lx_amc_error.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cubi_scan_handler~get_gate_measure_data.
    rv_gate_measure_data = me->gs_gate_measure_data.
  ENDMETHOD.
ENDCLASS.
