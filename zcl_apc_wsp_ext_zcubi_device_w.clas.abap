class ZCL_APC_WSP_EXT_ZCUBI_DEVICE_W definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZCUBI_DEVICE_W IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_message.
    TYPES: BEGIN OF ltyp_message,
             warehouse_number TYPE /scwm/lgnum,
             device_name      TYPE char40,
             product          TYPE /sapapo/matnr,
             uom              TYPE /sapapo/meins,
           END OF ltyp_message.

    DATA: ls_message TYPE ltyp_message.

    TRY.
        " deserialize JSON string into the message structure
        /ui2/cl_json=>deserialize( EXPORTING json = i_message->get_text( ) pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = ls_message ).

        " Need to convert the UOM.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = ls_message-uom
          IMPORTING
            output = ls_message-uom.

        CALL FUNCTION 'ZCUBI_POLL_CUBISCAN' STARTING NEW TASK 'CUBISCAN_INTEGRATION'
          EXPORTING
            iv_warehouse_number = ls_message-warehouse_number
            iv_device_name      = ls_message-device_name
            iv_product          = ls_message-product
            iv_uom              = ls_message-uom.
      CATCH cx_apc_error INTO DATA(lx_apc_error).
        MESSAGE lx_apc_error->get_text( ) TYPE 'e'.
    ENDTRY.
  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_start.
    " Bind the WebSocket connection to the AMC channel
    TRY.
        DATA(lo_binding) = i_context->get_binding_manager( ).
        lo_binding->bind_amc_message_consumer( i_application_id = 'ZCUBI_MESSAGE_CHANNEL' i_channel_id = '/outbound' ).
      CATCH cx_apc_error INTO DATA(lx_apc_error).
        DATA(lv_message) = lx_apc_error->get_text( ).
        MESSAGE lx_apc_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
