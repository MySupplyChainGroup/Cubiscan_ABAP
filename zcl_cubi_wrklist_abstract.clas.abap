class ZCL_CUBI_WRKLIST_ABSTRACT definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_CUBI_WRKLST
      abstract methods CREATE_ITEM
                       DELETE_ITEM
                       QUERY_HEADERS
                       QUERY_ITEMS
                       QUERY_PRODUCTS
                       QUERY_REVIEW_ITEMS
                       UPDATE_ITEM .

  methods CONSTRUCTOR .
protected section.

  data GT_CONFIGURATION type ZMSCG_CONFIGURATION_TT .

  methods GET_SUPPORTED_UOMS
    returning
      value(RT_UOM_SELECT_OPTIONS) type /IWBEP/T_COD_SELECT_OPTIONS .
  methods UPDATE_ECC
    importing
      !IS_WORKLIST_ITEM type ZCUBI_WORKLIST_ITEM
    exporting
      !EV_SUCCESS type BOOLEAN
      !ET_MESSAGES type BAPIRET2_T .
  methods GET_UOMS
    importing
      !IV_WAREHOUSE_NUMBER type /SCWM/LGNUM
      !IV_PRODUCT_NUMBER type /SAPAPO/MATNR
    exporting
      !ET_UOMS type ZCUBI_PRODUCT_UOM_TT .
  methods ALL_UOMS_ARE_MEASURED
    importing
      !IV_MATNR type /SAPAPO/MATNR
      !IV_MATID type /SAPAPO/MATID
    returning
      value(RV_MEASURED) type BOOLEAN .
private section.
ENDCLASS.



CLASS ZCL_CUBI_WRKLIST_ABSTRACT IMPLEMENTATION.


  METHOD all_uoms_are_measured.
    DATA: lt_uom_select_options       TYPE /iwbep/t_cod_select_options,
          lt_worklist_status          TYPE STANDARD TABLE OF zcubi_wrklist_st,
          lt_material_uoms            TYPE STANDARD TABLE OF /sapapo/meins,
          lv_uom                      TYPE /sapapo/meins,
          lt_matnr_uom_select_options TYPE /iwbep/t_cod_select_options,
          ls_select_option            LIKE LINE OF lt_matnr_uom_select_options.

    FIELD-SYMBOLS: <lfs_select_option> TYPE /iwbep/s_cod_select_option,
                   <lfs_uom>           TYPE /sapapo/meins.

    " Get the UOM select option values.
    lt_uom_select_options = me->get_supported_uoms( ).

    " Select all UOM's for the material.
    SELECT meinh FROM /sapapo/marm
      INTO TABLE lt_material_uoms
      WHERE matid = iv_matid.

    " Filter the UOM's for the material to only contain the supported UOM's.
    LOOP AT lt_uom_select_options ASSIGNING <lfs_select_option>.
      " Read the material UOM's and see if it exists, if it does then fill
      " it into a UOM select options table.
      READ TABLE lt_material_uoms ASSIGNING <lfs_uom> WITH KEY table_line = <lfs_select_option>-low.
      IF <lfs_uom> IS ASSIGNED.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <lfs_uom>.
        APPEND ls_select_option TO lt_matnr_uom_select_options.
      ENDIF.
      UNASSIGN <lfs_uom>.
    ENDLOOP.

    SELECT * FROM zcubi_wrklist_st
      INTO CORRESPONDING FIELDS OF TABLE lt_worklist_status
      WHERE product EQ iv_matnr
        AND uom IN lt_matnr_uom_select_options
        AND updated_ecc EQ abap_true.
    IF lines( lt_worklist_status ) EQ lines( lt_matnr_uom_select_options ).
      rv_measured = abap_true.
    ELSE.
      rv_measured = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    CALL METHOD zcl_mscg_common_utils=>get_configuration
      EXPORTING
        iv_config_application = 'CUBISCAN'
        iv_config_type        = 1
      RECEIVING
        rt_configuration      = gt_configuration.
  ENDMETHOD.


  METHOD get_supported_uoms.
    DATA: lt_uoms          TYPE STANDARD TABLE OF /sapapo/meins,
          ls_select_option TYPE /iwbep/s_cod_select_option.

    FIELD-SYMBOLS: <lfs_config> TYPE zmscg_configuration,
                   <lfs_uom>    TYPE /sapapo/meins.

    " Read the config values.
    READ TABLE gt_configuration ASSIGNING <lfs_config> WITH KEY config_key = 'SUPPORTED_UOMS'.
    IF <lfs_config> IS ASSIGNED.
      SPLIT <lfs_config>-config_value AT ',' INTO TABLE lt_uoms.
      LOOP AT lt_uoms ASSIGNING <lfs_uom>.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <lfs_uom>.
        APPEND ls_select_option TO rt_uom_select_options.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_uoms.
    DATA: lv_mat_id             TYPE /sapapo/matid,
          lt_uom                TYPE STANDARD TABLE OF meinh,
          ls_entity             LIKE LINE OF et_uoms,
          ls_select_option      TYPE /iwbep/s_cod_select_option,
          lt_uoms               TYPE STANDARD TABLE OF /sapapo/meins,
          lt_configuration      TYPE zmscg_configuration_tt,
          lt_uom_select_options TYPE /iwbep/t_cod_select_options,
          lt_worklist_items     TYPE STANDARD TABLE OF zcubi_worklist_item.

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
      SPLIT <lfs_config>-config_value AT ',' INTO TABLE lt_uoms.
      LOOP AT lt_uoms ASSIGNING <lfs_uom>.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <lfs_uom>.
        APPEND ls_select_option TO lt_uom_select_options.
      ENDLOOP.
    ENDIF.

    " Select the mat id first.
    SELECT SINGLE matid FROM /sapapo/matkey
      INTO lv_mat_id
      WHERE matnr EQ iv_product_number.

    SELECT meinh FROM /sapapo/marm
      INTO TABLE lt_uom
      WHERE matid EQ lv_mat_id
        AND meinh IN lt_uom_select_options.

    LOOP AT lt_uom ASSIGNING <lfs_uom>.
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
    ENDLOOP.

    SORT et_uoms BY product uom_key.
  ENDMETHOD.


  METHOD update_ecc.
    DATA: lv_rfc_dest        TYPE rfcdest,
          lt_messages        TYPE bapiret2_t,
          ls_return          TYPE bapiret2,
          ls_worklist_status TYPE zcubi_wrklist_st,
          lv_queue_name      TYPE trfcqnam,
          lv_product         TYPE char40.

    FIELD-SYMBOLS: <lfs_config> LIKE LINE OF gt_configuration.

    " Default ev_success to abap_true.
    ev_success = abap_true.

    " Lookup RFC Destination.
    READ TABLE gt_configuration ASSIGNING <lfs_config> WITH KEY config_key = 'ECC_RFC_DEST'.
    IF <lfs_config> IS ASSIGNED AND <lfs_config>-config_value IS NOT INITIAL.
      MOVE <lfs_config>-config_value TO lv_rfc_dest.
    ELSE.
      ls_return-id = 'ZCUBI_MESSAGES'.
      ls_return-type = 'E'.
      ls_return-number = '010'.
      APPEND ls_return TO et_messages.

      ev_success = abap_false.
      RETURN.
    ENDIF.

    " Set the outbound queue name
    CALL FUNCTION 'CONVERSION_EXIT_PRODU_OUTPUT'
      EXPORTING
        input  = is_worklist_item-product
      IMPORTING
        output = lv_product.
    CONCATENATE 'CUBI' lv_product is_worklist_item-uom INTO lv_queue_name.
    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname              = lv_queue_name
      EXCEPTIONS
        invalid_queue_name = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      ls_return-id = '00'.
      ls_return-type = 'E'.
      ls_return-number = '001'.
      ls_return-message_v1 = 'TRFC_SET_QUEUE_NAME error'.
      APPEND ls_return TO et_messages.

      ev_success = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ZMSCG_CUBISCAN_EWM_TO_ECC' IN BACKGROUND TASK AS SEPARATE UNIT DESTINATION lv_rfc_dest
      EXPORTING
        iv_matnr         = is_worklist_item-product
        iv_uom           = is_worklist_item-uom
        iv_length        = is_worklist_item-length
        iv_width         = is_worklist_item-width
        iv_height        = is_worklist_item-height
        iv_dimension_uom = is_worklist_item-meabm
        iv_gross_weight  = is_worklist_item-g_weight
        iv_net_weight    = is_worklist_item-n_weight
        iv_weight_uom    = is_worklist_item-gewei
        iv_volume        = is_worklist_item-volume
        iv_volume_uom    = is_worklist_item-voleh.

    " Now update the Cubiscan status table.
    SELECT SINGLE * FROM zcubi_wrklist_st
      INTO ls_worklist_status
      WHERE product EQ is_worklist_item-product
        AND uom EQ is_worklist_item-uom.
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING is_worklist_item TO ls_worklist_status.
      ls_worklist_status-created_by = sy-uname.
      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist_status-created_timestamp TIME ZONE 'UTC'.
    ENDIF.

    ls_worklist_status-updated_ecc = abap_true.
    ls_worklist_status-updated_by = sy-uname.
    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist_status-updated_timestamp TIME ZONE 'UTC'.
    MODIFY zcubi_wrklist_st FROM ls_worklist_status.
    COMMIT WORK AND WAIT.

    ls_return-id = 'ZCUBI_MESSAGES'.
    ls_return-type = 'S'.
    ls_return-number = '009'.
    APPEND ls_return TO et_messages.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_uoms.
    CALL METHOD me->get_uoms
      EXPORTING
        iv_warehouse_number = iv_warehouse_number
        iv_product_number   = iv_product_number
      IMPORTING
        et_uoms             = et_uoms.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_uoms_in_worklist.
    DATA: lt_uoms                     TYPE zcubi_product_uom_tt,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_status_select_options    TYPE /iwbep/t_cod_select_options,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          lt_worklist_items           TYPE zcubi_worklist_item_tt,
          lv_uom                      TYPE char3.

    FIELD-SYMBOLS: <lfs_uom>           LIKE LINE OF lt_uoms,
                   <lfs_worklist_item> LIKE LINE OF lt_worklist_items.

    CALL METHOD me->get_uoms
      EXPORTING
        iv_warehouse_number = iv_warehouse_number
        iv_product_number   = iv_product_number
      IMPORTING
        et_uoms             = lt_uoms.

    " Populate the status select options.
    " New Entries
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = 'N'.
    APPEND ls_select_option TO lt_status_select_options.

    " Re-measure Entries.
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = 'R'.
    APPEND ls_select_option TO lt_status_select_options.

    " Populate warehouse select options.
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = iv_warehouse_number.
    APPEND ls_select_option TO lt_warehouse_select_options.

    CALL METHOD me->zif_cubi_wrklst~query_items
      EXPORTING
        it_warehouse_select_options = lt_warehouse_select_options
        it_status_select_options    = lt_status_select_options
      RECEIVING
        rt_items                    = lt_worklist_items.

    " Read the worklist and determine the UOM's that are currently in there.
    " Only want to return the UOM's that exist in the worklist.
    LOOP AT lt_uoms ASSIGNING <lfs_uom>.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input  = <lfs_uom>-uom_key
        IMPORTING
          output = lv_uom.

      READ TABLE lt_worklist_items ASSIGNING <lfs_worklist_item> WITH KEY product = iv_product_number uom = lv_uom.
      IF <lfs_worklist_item> IS ASSIGNED.
        APPEND <lfs_uom> TO et_uoms.
      ENDIF.
      UNASSIGN <lfs_worklist_item>.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_uoms_not_in_worklist.
    DATA: lt_uoms                     TYPE zcubi_product_uom_tt,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_status_select_options    TYPE /iwbep/t_cod_select_options,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          lt_worklist_items           TYPE zcubi_worklist_item_tt.

    FIELD-SYMBOLS: <lfs_uom>           LIKE LINE OF lt_uoms,
                   <lfs_worklist_item> LIKE LINE OF lt_worklist_items.

    CALL METHOD me->get_uoms
      EXPORTING
        iv_warehouse_number = iv_warehouse_number
        iv_product_number   = iv_product_number
      IMPORTING
        et_uoms             = lt_uoms.

    " Populate the status select options.
    " New Entries
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = 'N'.
    APPEND ls_select_option TO lt_status_select_options.

    " Re-measure Entries.
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = 'R'.
    APPEND ls_select_option TO lt_status_select_options.

    " Populate warehouse select options.
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = iv_warehouse_number.
    APPEND ls_select_option TO lt_warehouse_select_options.

    CALL METHOD me->zif_cubi_wrklst~query_items
      EXPORTING
        it_warehouse_select_options = lt_warehouse_select_options
        it_status_select_options    = lt_status_select_options
      RECEIVING
        rt_items                    = lt_worklist_items.

    " Read the worklist and determine the UOM's that are currently in there.
    " They cannot be added to the worklist if they already exist so they need
    " to be filtered out.
    LOOP AT lt_uoms ASSIGNING <lfs_uom>.
      READ TABLE lt_worklist_items ASSIGNING <lfs_worklist_item>
        WITH KEY product = iv_product_number uom = <lfs_uom>-uom_key.
      IF <lfs_worklist_item> IS NOT ASSIGNED.
        APPEND <lfs_uom> TO et_uoms.
      ENDIF.
      UNASSIGN <lfs_worklist_item>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
