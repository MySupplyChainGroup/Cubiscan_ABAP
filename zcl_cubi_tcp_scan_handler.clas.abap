class ZCL_CUBI_TCP_SCAN_HANDLER definition
  public
  inheriting from ZCL_CUBI_ABSTRACT_SCAN_HANDLER
  final
  create public .

public section.

  interfaces IF_APC_WSP_EVENT_HANDLER .
  interfaces IF_APC_WSP_EVENT_HANDLER_BASE .

  events ON_MESSAGE
    exporting
      value(IV_MESSAGE) type STRING .

  methods CONSTRUCTOR
    importing
      !IS_CUBISCAN_DEVICE type ZCUBI_DEVICE
      !IV_PRODUCT type /SAPAPO/MATNR
      !IV_UOM type /SAPAPO/MEINS
    raising
      CX_APC_ERROR .

  methods ZIF_CUBI_SCAN_HANDLER~CONNECT
    redefinition .
  methods ZIF_CUBI_SCAN_HANDLER~DISCONNECT
    redefinition .
  methods ZIF_CUBI_SCAN_HANDLER~START_POLLING
    redefinition .
protected section.

  data GO_CLIENT type ref to IF_APC_WSP_CLIENT .
  data GO_MESSAGE_MANAGER type ref to IF_APC_WSP_MESSAGE_MANAGER .
private section.
ENDCLASS.



CLASS ZCL_CUBI_TCP_SCAN_HANDLER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_cubiscan_device = is_cubiscan_device iv_product = iv_product iv_uom = iv_uom ).

    DATA: lv_frame       TYPE apc_tcp_frame,
          lv_frame_t     TYPE string VALUE '0A',
          lv_hostname    TYPE string,
          lv_port_number TYPE string.

    lv_hostname = is_cubiscan_device-host_name.
    lv_port_number = is_cubiscan_device-port_number.

    " Specification of TCP frame
    lv_frame-frame_type = if_apc_tcp_frame_types=>co_frame_type_terminator. " Frames are terminated with specific bytes
    lv_frame-terminator = lv_frame_t. " Frame termination bytes
    go_client = cl_apc_tcp_client_manager=>create( i_host = lv_hostname
                                                   i_port = lv_port_number
                                                   i_frame = lv_frame
                                                   i_event_handler = me ).
  ENDMETHOD.


  METHOD IF_APC_WSP_EVENT_HANDLER~ON_CLOSE.
  ENDMETHOD.


  METHOD IF_APC_WSP_EVENT_HANDLER~ON_ERROR.
  ENDMETHOD.


  METHOD IF_APC_WSP_EVENT_HANDLER~ON_MESSAGE.
    DATA: ls_msg           TYPE bal_s_msg.

    " Message handling
    TRY.
        me->gv_message = i_message->get_text( ).
      CATCH cx_apc_error INTO DATA(lx_apc_error).
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '007'.
        ls_msg-msgv1 = gs_cubiscan_device-device_name.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg.

        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '008'.
        ls_msg-msgv1 = lx_apc_error->get_text( ).

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg.

        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_save_all     = abap_true
            i_t_log_handle = gt_log_handles.

        " Set the error message into the message variable to stop the polling.
        me->gv_message = lx_apc_error->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD IF_APC_WSP_EVENT_HANDLER~ON_OPEN.
  ENDMETHOD.


  METHOD zif_cubi_scan_handler~connect.
    DATA: lx_apc_exception TYPE REF TO cx_apc_error,
          ls_msg           TYPE bal_s_msg.

    TRY.
        " Initiate the connection setup, successful connect leads to execution of ON_OPEN
        go_client->connect( ).
      CATCH cx_apc_error INTO lx_apc_exception.
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '004'.
        ls_msg-msgv1 = gs_cubiscan_device-device_name.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg.

        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_save_all     = abap_true
            i_t_log_handle = gt_log_handles.

        " Send message back to the client.
        CALL METHOD me->send_outbound_message
          EXPORTING
            iv_warehouse_number = gs_cubiscan_device-warehouse_number
            iv_device_name      = gs_cubiscan_device-device_name
            iv_product          = me->gv_product
            iv_uom              = me->gv_uom
            iv_object           = gc_object
            iv_action           = gc_action_connect_error.

        RAISE EXCEPTION TYPE zcx_cubiscan_exception
          EXPORTING
            previous = lx_apc_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cubi_scan_handler~disconnect.
    DATA: lx_apc_exception TYPE REF TO cx_apc_error,
          ls_msg           TYPE bal_s_msg.

    TRY.
        " close connection
        go_client->close( i_reason = 'Application Closed Connection!' ).
      CATCH cx_apc_error INTO lx_apc_exception.
        " Log the message to the application log.
        ls_msg-msgty = 'E'.
        ls_msg-msgid = 'ZCUBI_MESSAGES'.
        ls_msg-msgno = '005'.
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
            previous = lx_apc_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cubi_scan_handler~start_polling.
    DATA: ls_msg               TYPE bal_s_msg,
          ls_gate_measure_data TYPE zcubi_gate_measure_data,
          lo_cubiscan_scan     TYPE REF TO zcl_cubiscan_scan.

    " Clear the message first.
    CLEAR me->gv_message.

    " Wait for the a message from peer
    WAIT FOR PUSH CHANNELS UNTIL me->gv_message IS NOT INITIAL UP TO gs_cubiscan_device-polling_period SECONDS.
    IF sy-subrc = 8 AND me->gv_message IS INITIAL.
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
      ls_msg-msgno = '006'.
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
      " Received a message, now parse it.
      CREATE OBJECT lo_cubiscan_scan
        EXPORTING
          iv_warehouse_number = gs_cubiscan_device-warehouse_number
          iv_product          = me->gv_product
          iv_uom              = me->gv_uom
          iv_gate_measure_str = me->gv_message.
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
    ENDIF.
  ENDMETHOD.
ENDCLASS.
