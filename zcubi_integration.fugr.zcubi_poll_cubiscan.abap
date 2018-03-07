FUNCTION zcubi_poll_cubiscan.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_WAREHOUSE_NUMBER) TYPE  /SCWM/LGNUM
*"     VALUE(IV_DEVICE_NAME) TYPE  CHAR40
*"     VALUE(IV_PRODUCT) TYPE  /SAPAPO/MATNR
*"     VALUE(IV_UOM) TYPE  /SAPAPO/MEINS
*"----------------------------------------------------------------------
  DATA: ls_device      TYPE zcubi_device,
        lo_instance    TYPE REF TO zif_cubi_scan_handler,
        ls_log         TYPE bal_s_log,
        lv_log_handle  TYPE balloghndl,
        lt_log_handles TYPE bal_t_logh,
        ls_msg         TYPE bal_s_msg,
        lv_msg         TYPE char80,
        lt_parameters  TYPE abap_parmbind_tab,
        ls_parameter   TYPE abap_parmbind.

  " Create the logging handle.
  ls_log-object = 'ZCUBISCAN'.
  ls_log-subobject = 'TCP_CLIENT'.
  ls_log-aluser = sy-uname.
  ls_log-alprog = sy-repid.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = lv_log_handle.
  APPEND lv_log_handle TO lt_log_handles.

  " Add a message stating the background process started.
  ls_msg-msgty = 'I'.
  ls_msg-msgid = 'ZCUBI_MESSAGES'.
  ls_msg-msgno = '017'.
  ls_msg-msgv1 = iv_warehouse_number.
  ls_msg-msgv2 = iv_device_name.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = lv_log_handle
      i_s_msg      = ls_msg.

  " Lookup the Cubiscan device values.
  SELECT SINGLE * FROM zcubi_devices
    INTO CORRESPONDING FIELDS OF ls_device
    WHERE warehouse_number EQ iv_warehouse_number AND device_name EQ iv_device_name.

  " Create the object, connect, start polling and then disconnect.
  TRY.
      ls_parameter-name = 'IS_CUBISCAN_DEVICE'.
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      GET REFERENCE OF ls_device INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE lt_parameters.

      ls_parameter-name = 'IV_PRODUCT'.
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      GET REFERENCE OF iv_product INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE lt_parameters.

      ls_parameter-name = 'IV_UOM'.
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      GET REFERENCE OF iv_uom INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE lt_parameters.

      CREATE OBJECT lo_instance TYPE (ls_device-handler_class_name)
        PARAMETER-TABLE lt_parameters.
      lo_instance->connect( ).
      lo_instance->start_polling( ).
      lo_instance->disconnect( ).
    CATCH cx_apc_error.
      ls_msg-msgty = 'E'.
      ls_msg-msgid = 'ZCUBI_MESSAGES'.
      ls_msg-msgno = '018'.
      ls_msg-msgv1 = iv_warehouse_number.
      ls_msg-msgv2 = iv_device_name.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle
          i_s_msg      = ls_msg.
    CATCH zcx_cubiscan_exception.
      ls_msg-msgty = 'E'.
      ls_msg-msgid = 'ZCUBI_MESSAGES'.
      ls_msg-msgno = '018'.
      ls_msg-msgv1 = iv_warehouse_number.
      ls_msg-msgv2 = iv_device_name.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle
          i_s_msg      = ls_msg.
  ENDTRY.

  " Save the log
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all     = abap_true
      i_t_log_handle = lt_log_handles.
ENDFUNCTION.
