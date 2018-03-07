class ZCL_CUBI_WRKLIST_PRE_GOLIVE definition
  public
  inheriting from ZCL_CUBI_WRKLIST_LOCAL
  final
  create public .

public section.

  methods ZIF_CUBI_WRKLST~QUERY_PRODUCTS
    redefinition .
  methods ZIF_CUBI_WRKLST~QUERY_UOMS
    redefinition .
  methods ZIF_CUBI_WRKLST~UPDATE_ITEM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUBI_WRKLIST_PRE_GOLIVE IMPLEMENTATION.


  METHOD zif_cubi_wrklst~query_products.

  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_uoms.
    DATA: lt_uom            TYPE STANDARD TABLE OF /sapapo/meins,
          ls_entity         LIKE LINE OF et_uoms,
          lt_configuration  TYPE zmscg_configuration_tt,
          lt_worklist_items TYPE STANDARD TABLE OF zcubi_worklist_item.

    FIELD-SYMBOLS: <lfs_config>        TYPE zmscg_configuration,
                   <lfs_uom>           TYPE /sapapo/meins,
                   <lfs_worklist_item> TYPE zcubi_worklist_item.

    " Read configuration.
    CALL METHOD zcl_mscg_common_utils=>get_configuration
      EXPORTING
        iv_config_application = 'CUBISCAN'
        iv_config_type        = 1 "Backend Configuration
      RECEIVING
        rt_configuration      = lt_configuration.

    " Read the config values and construct the select option table of all valid UOM's.
    READ TABLE lt_configuration ASSIGNING <lfs_config> WITH KEY config_key = 'SUPPORTED_UOMS'.
    IF <lfs_config> IS ASSIGNED.
      SPLIT <lfs_config>-config_value AT ',' INTO TABLE lt_uom.
    ENDIF.

    " Read the worklist and determine the UOM's that are currently in there.
    " They cannot be added to the worklist if they already exist so they need
    " to be filtered out.
    SELECT * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF TABLE lt_worklist_items
      WHERE warehouse EQ iv_warehouse_number AND product EQ iv_product_number.

    LOOP AT lt_uom ASSIGNING <lfs_uom>.
      READ TABLE lt_worklist_items ASSIGNING <lfs_worklist_item>
        WITH KEY product = iv_product_number uom = <lfs_uom>.
      IF <lfs_worklist_item> IS NOT ASSIGNED.
        ls_entity-product = iv_product_number.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = <lfs_uom>
          IMPORTING
            output         = ls_entity-uom_key
            short_text     = ls_entity-uom_text
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc IS INITIAL.
          APPEND ls_entity TO et_uoms.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT et_uoms BY product uom_key.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~update_item.
***********************************************************
*   Special implementation, no update to ECC in this case
*   if the status is 'U' as these entries will be exported
*   and possibly imported into the sytem.
***********************************************************
    DATA: lt_messages TYPE bapiret2_t,
          ls_message  TYPE bapiret2,
          ls_worklist TYPE zcubi_worklist.

    FIELD-SYMBOLS:  <lfs_message> TYPE bapiret2.

    " Now update the worklist.
    SELECT SINGLE * FROM zcubi_worklist
        INTO CORRESPONDING FIELDS OF ls_worklist
        WHERE warehouse EQ is_worklist_item-warehouse AND product EQ is_worklist_item-product
          AND uom EQ is_worklist_item-uom.
    MOVE-CORRESPONDING is_worklist_item TO ls_worklist.

    " If the status is a 'U', it means they are approving from the Approval screen.  However, we
    " need to really only update the status in this case to an 'A'.  Means Approved but not updated
    " to ECC.
    IF ls_worklist-status EQ 'U'.
      ls_worklist-status = 'A'.
    ENDIF.

    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist-changeutc TIME ZONE 'UTC'.
    ls_worklist-changeuser = sy-uname.
    UPDATE zcubi_worklist FROM ls_worklist.
    IF sy-subrc IS INITIAL.
      ls_message-type = 'S'.
      ls_message-id = 'ZCUBI_MESSAGES'.
      ls_message-number = '011'.
      ls_message-message_v1 = ls_worklist-product.
      ls_message-message_v2 = ls_worklist-uom.
      APPEND ls_message TO et_messages.

      ev_success = abap_true.
    ELSE.
      ls_message-type = 'E'.
      ls_message-id = 'ZCUBI_MESSAGES'.
      ls_message-number = '028'.
      ls_message-message_v1 = ls_worklist-product.
      ls_message-message_v2 = ls_worklist-uom.
      APPEND ls_message TO et_messages.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
