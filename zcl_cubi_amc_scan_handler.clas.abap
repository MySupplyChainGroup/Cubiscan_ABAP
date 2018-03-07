class ZCL_CUBI_AMC_SCAN_HANDLER definition
  public
  inheriting from ZCL_CUBI_ABSTRACT_SCAN_HANDLER
  final
  create public .

public section.

  interfaces IF_AMC_MESSAGE_RECEIVER .
  interfaces IF_AMC_MESSAGE_RECEIVER_TEXT .

  methods ZIF_CUBI_SCAN_HANDLER~CONNECT
    redefinition .
  methods ZIF_CUBI_SCAN_HANDLER~DISCONNECT
    redefinition .
  methods ZIF_CUBI_SCAN_HANDLER~START_POLLING
    redefinition .
protected section.

  types:
    ltyp_message_list TYPE STANDARD TABLE OF string .

  data GO_MESSAGE_CONSUMER type ref to IF_AMC_MESSAGE_CONSUMER .
  data GT_MESSAGE_LIST type LTYP_MESSAGE_LIST .
private section.
ENDCLASS.



CLASS ZCL_CUBI_AMC_SCAN_HANDLER IMPLEMENTATION.


  METHOD if_amc_message_receiver_text~receive.
    " Insert received messages into the global table
    APPEND i_message TO gt_message_list.
  ENDMETHOD.


  METHOD zif_cubi_scan_handler~connect.
    DATA: lx_amc_error TYPE REF TO cx_amc_error,
          ls_msg       TYPE bal_s_msg.

    TRY.
        go_message_consumer = cl_amc_channel_manager=>create_message_consumer( i_application_id = 'ZCUBI_MESSAGE_CHANNEL' i_channel_id = '/inbound' ).

        " Start of message delivery
        go_message_consumer->start_message_delivery( i_receiver = me ).
      CATCH cx_amc_error INTO lx_amc_error.
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '014'.
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


  METHOD zif_cubi_scan_handler~disconnect.
    DATA: lx_amc_error TYPE REF TO cx_amc_error,
          ls_msg       TYPE bal_s_msg.

    TRY.
        " Stop message delivery
        go_message_consumer->stop_message_delivery( i_receiver = me ).
      CATCH cx_amc_error INTO lx_amc_error.
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '015'.
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


  METHOD zif_cubi_scan_handler~start_polling.
    DATA: ls_gate_measure_data TYPE zcubi_gate_measure_data,
          ls_msg               TYPE bal_s_msg,
          lo_cubiscan_scan     TYPE REF TO zcl_cubiscan_scan.

    FIELD-SYMBOLS: <lfs_message> LIKE LINE OF gt_message_list.

    " Wait until a message is received but not longer than waiting time in seconds
    WAIT FOR MESSAGING CHANNELS UNTIL lines( gt_message_list ) >= 1 UP TO gs_cubiscan_device-polling_period SECONDS.

    IF sy-subrc = 8 AND  lines( gt_message_list ) = 0.
      " Handle no message received.
      CALL METHOD me->send_outbound_message
        EXPORTING
          iv_warehouse_number = gs_cubiscan_device-warehouse_number
          iv_device_name      = gs_cubiscan_device-device_name
          iv_product          = me->gv_product
          iv_uom              = me->gv_uom
          iv_object           = me->gc_object
          iv_action           = me->gc_action_timeout.

      " Log the message to the application log.
      ls_msg-msgty = 'I'.
      ls_msg-msgid = 'ZCUBI_MESSAGES'.
      ls_msg-msgno = '016'.
      ls_msg-msgv1 = gs_cubiscan_device-device_name.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = gv_log_handle
          i_s_msg      = ls_msg.

      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_save_all     = abap_true
          i_t_log_handle = gt_log_handles.
    ELSE.
      " Push the messages to the outbound AMC.
      LOOP AT gt_message_list ASSIGNING <lfs_message>.
        CREATE OBJECT lo_cubiscan_scan
          EXPORTING
            iv_warehouse_number = gs_cubiscan_device-warehouse_number
            iv_product          = me->gv_product
            iv_uom              = me->gv_uom
            iv_gate_measure_str = <lfs_message>.
        ls_gate_measure_data = lo_cubiscan_scan->get_data( ).
        CALL METHOD me->send_outbound_message
          EXPORTING
            iv_warehouse_number  = gs_cubiscan_device-warehouse_number
            iv_device_name       = gs_cubiscan_device-device_name
            iv_product           = me->gv_product
            iv_uom               = me->gv_uom
            iv_object            = me->gc_object
            iv_action            = me->gc_action_success
            is_gate_measure_data = ls_gate_measure_data.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
