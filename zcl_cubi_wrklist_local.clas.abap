class ZCL_CUBI_WRKLIST_LOCAL definition
  public
  inheriting from ZCL_CUBI_WRKLIST_ABSTRACT
  create public .

public section.

  methods ZIF_CUBI_WRKLST~CREATE_ITEM
    redefinition .
  methods ZIF_CUBI_WRKLST~DELETE_ITEM
    redefinition .
  methods ZIF_CUBI_WRKLST~QUERY_HEADERS
    redefinition .
  methods ZIF_CUBI_WRKLST~QUERY_ITEMS
    redefinition .
  methods ZIF_CUBI_WRKLST~QUERY_PRODUCTS
    redefinition .
  methods ZIF_CUBI_WRKLST~QUERY_REVIEW_ITEMS
    redefinition .
  methods ZIF_CUBI_WRKLST~UPDATE_ITEM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUBI_WRKLIST_LOCAL IMPLEMENTATION.


  METHOD zif_cubi_wrklst~create_item.
    DATA: ls_worklist_item   TYPE zcubi_worklist,
          ls_worklist_status TYPE zcubi_wrklist_st.

    " Need to check and make sure the item does not already exist.
    SELECT SINGLE * FROM zcubi_worklist INTO ls_worklist_item
      WHERE warehouse EQ is_worklist_item-warehouse AND product EQ is_worklist_item-product
        AND uom EQ is_worklist_item-uom AND status NE 'U'.
    IF sy-subrc IS INITIAL.
      " Entry exists.
      rs_return-id = 'ZCUBI_MESSAGES'.
      rs_return-number = '024'.
      rs_return-type = 'E'.
      rs_return-message_v1 = ls_worklist_item-product.
      rs_return-message_v2 = ls_worklist_item-uom.

      RETURN.
    ENDIF.

    " Check to see if this product/UOM entry is completed in the worklist status table.
    " If it is, then we need to clear the entry otherwise the product/uom will not appear in the worklist.
    SELECT SINGLE * FROM zcubi_wrklist_st
      INTO ls_worklist_status
      WHERE product EQ is_worklist_item-product
        AND uom EQ is_worklist_item-uom.
    IF sy-subrc IS INITIAL.
      " Found it, we need to clear the ECC info.
      CLEAR ls_worklist_status-updated_ecc.
      CLEAR ls_worklist_status-updated_timestamp.
      CLEAR ls_worklist_status-updated_by.
    ELSE.
      " Didn't find it, need to create the entry for it.
      MOVE-CORRESPONDING is_worklist_item TO ls_worklist_status.
      ls_worklist_status-created_by = sy-uname.
      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist_status-created_timestamp TIME ZONE 'UTC'.
    ENDIF.

    MODIFY zcubi_wrklist_st FROM ls_worklist_status.
    IF sy-subrc IS INITIAL.
      " Successfully modified the status table.
      " Create the item in the worklist.
      MOVE-CORRESPONDING is_worklist_item TO ls_worklist_item.

      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist_item-createutc TIME ZONE 'UTC'.
      ls_worklist_item-createuser = sy-uname.

      MODIFY zcubi_worklist FROM ls_worklist_item.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.

        " Add success message.
        rs_return-type = 'S'.
        rs_return-id = 'ZCUBI_MESSAGES'.
        rs_return-number = '025'.
        rs_return-message_v1 = ls_worklist_item-product.
        rs_return-message_v2 = ls_worklist_item-uom.
      ELSE.
        ROLLBACK WORK.

        rs_return-id = 'ZCUBI_MESSAGES'.
        rs_return-number = '023'.
        rs_return-type = 'E'.
        rs_return-message_v1 = ls_worklist_item-product.
        rs_return-message_v2 = ls_worklist_item-uom.
      ENDIF.
    ELSE.
      ROLLBACK WORK.

      rs_return-id = 'ZCUBI_MESSAGES'.
      rs_return-number = '023'.
      rs_return-type = 'E'.
      rs_return-message_v1 = ls_worklist_item-product.
      rs_return-message_v2 = ls_worklist_item-uom.
    ENDIF.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~delete_item.
    DATA: lt_worklist_items TYPE STANDARD TABLE OF zcubi_worklist,
          ls_message        TYPE bapiret2,
          lv_product        TYPE /sapapo/matnr,
          lv_uom            TYPE /sapapo/meins.

    FIELD-SYMBOLS: <lfs_worklist_item> LIKE LINE OF lt_worklist_items.

    " Default EV_SUCCESS to true.
    ev_success = abap_true.

    " Need to replace the * wildcard character with the % wildcard character before querying.
    MOVE is_worklist_item-product TO lv_product.
    REPLACE ALL OCCURRENCES OF '*' IN lv_product WITH '%'.

    MOVE is_worklist_item-uom TO lv_uom.
    REPLACE ALL OCCURRENCES OF '*' IN lv_uom WITH '%'.

    " Select all entries that match and then delete.
    SELECT * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF TABLE lt_worklist_items
      WHERE warehouse EQ is_worklist_item-warehouse AND product LIKE lv_product
        AND uom LIKE lv_uom.
    IF sy-subrc IS INITIAL.
      " Delete from the worklist.
      LOOP AT lt_worklist_items ASSIGNING <lfs_worklist_item>.
        DELETE zcubi_worklist FROM <lfs_worklist_item>.
        IF sy-subrc IS INITIAL.
          ls_message-type = 'S'.
          ls_message-id = 'ZCUBI_MESSAGES'.
          ls_message-number = '030'.
          ls_message-message_v1 = <lfs_worklist_item>-product.
          ls_message-message_v2 = <lfs_worklist_item>-uom.
          APPEND ls_message TO et_messages.
        ELSE.
          ev_success = abap_false.

          ls_message-type = 'E'.
          ls_message-id = 'ZCUBI_MESSAGES'.
          ls_message-number = '029'.
          ls_message-message_v1 = <lfs_worklist_item>-product.
          ls_message-message_v2 = <lfs_worklist_item>-uom.
          APPEND ls_message TO et_messages.
        ENDIF.
      ENDLOOP.
    ELSE.
      ls_message-type = 'S'.
      ls_message-id = 'ZCUBI_MESSAGES'.
      ls_message-number = '031'.
      ls_message-message_v1 = is_worklist_item-product.
      ls_message-message_v2 = is_worklist_item-uom.
      APPEND ls_message TO et_messages.
    ENDIF.

    IF ev_success EQ abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_headers.
    DATA: lv_mat_id       TYPE /sapapo/matid,
          lo_badi_handler TYPE REF TO zbadi_mscg_cubiscan,
          lv_is_measured  TYPE boolean.

    FIELD-SYMBOLS: <fs_entity>           LIKE LINE OF rt_headers.

    SELECT DISTINCT warehouse product FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF TABLE rt_headers
      WHERE warehouse IN it_warehouse_select_options
        AND product IN it_product_select_options
      ORDER BY product.

    LOOP AT rt_headers ASSIGNING <fs_entity>.
      SELECT SINGLE matid FROM /sapapo/matkey
          INTO lv_mat_id
          WHERE matnr EQ <fs_entity>-product.

      " Check if all UOM's are measured already.  If they have been then we don't need
      " to add this to the list.
      lv_is_measured = me->all_uoms_are_measured( iv_matnr = <fs_entity>-product iv_matid = lv_mat_id ).
      IF lv_is_measured NE abap_true.
        IF sy-subrc IS INITIAL.
          " Select the product description.
          SELECT SINGLE maktx FROM /sapapo/mattxt
            INTO <fs_entity>-product_description
            WHERE matid EQ lv_mat_id AND langu EQ sy-langu.
        ELSE.
          <fs_entity>-product_description = 'N/A'.
        ENDIF.

        " Retrieve product image.
        GET BADI lo_badi_handler.
        CALL BADI lo_badi_handler->get_image_path
          EXPORTING
            iv_product    = <fs_entity>-product
          IMPORTING
            ev_image_path = <fs_entity>-product_image.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_items.
    DATA: lt_taba            TYPE STANDARD TABLE OF dd07v,
          lt_tabb            TYPE STANDARD TABLE OF dd07v,
          lt_worklist_status TYPE HASHED TABLE OF zcubi_wrklist_st WITH UNIQUE KEY product uom.

    FIELD-SYMBOLS: <fs_entry>            TYPE zcubi_worklist_item,
                   <fs_dom_value>        TYPE dd07v,
                   <lfs_worklist_status> TYPE zcubi_wrklist_st.

    " Get status domain values.
    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name = 'ZCUBI_STATUS'
        langu       = sy-langu
        withtext    = 'X'
      TABLES
        dd07v_tab_a = lt_taba
        dd07v_tab_n = lt_tabb.

    SELECT * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF TABLE rt_items
      WHERE warehouse IN it_warehouse_select_options
        AND product IN it_product_select_options
        AND status IN it_status_select_options.

    " Select all records from the worklist status table.
    SELECT * FROM zcubi_wrklist_st
      INTO CORRESPONDING FIELDS OF TABLE lt_worklist_status
      FOR ALL ENTRIES IN rt_items
      WHERE product EQ rt_items-product.

    LOOP AT rt_items ASSIGNING <fs_entry>.
      " Check if the item exists in the worklist status table.  If it does, then populate the
      " appropriate values.
      READ TABLE lt_worklist_status ASSIGNING <lfs_worklist_status>
        WITH KEY product = <fs_entry>-product uom = <fs_entry>-uom.
      IF <lfs_worklist_status> IS ASSIGNED.
        MOVE-CORRESPONDING <lfs_worklist_status> TO <fs_entry>.
      ENDIF.
      UNASSIGN <lfs_worklist_status>.

      READ TABLE lt_taba ASSIGNING <fs_dom_value> WITH KEY domvalue_l = <fs_entry>-status.
      IF <fs_dom_value> IS ASSIGNED.
        <fs_entry>-status_text = <fs_dom_value>-ddtext.
      ENDIF.
      UNASSIGN <fs_dom_value>.
    ENDLOOP.

    " Sort the results by UOM.
    SORT rt_items BY uom.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_products.
    TYPES: BEGIN OF ltyp_scu_lookup,
             warehouse TYPE /scwm/lgnum,
             scuguid   TYPE /scmb/mdl_scuguid,
           END OF ltyp_scu_lookup.

    TYPES: BEGIN OF ltyp_matid_matnr,
             matid TYPE /sapapo/matid,
             matnr TYPE /sapapo/matnr,
           END OF ltyp_matid_matnr.

    DATA: lt_mat_id                   TYPE STANDARD TABLE OF /sapapo/matid,
          lt_matid_matnr              TYPE STANDARD TABLE OF ltyp_matid_matnr,
          lt_uom_select_options       TYPE /iwbep/t_cod_select_options,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_scu                      TYPE STANDARD TABLE OF ltyp_scu_lookup,
          ls_product                  TYPE zcubi_product_lookup,
          lv_warehouse_number         TYPE /scwm/lgnum,
          lv_is_measured              TYPE boolean.

    FIELD-SYMBOLS: <lfs_matid_matnr> TYPE ltyp_matid_matnr,
                   <lfs_scu>         TYPE ltyp_scu_lookup.

    " Check if a warehouse number was supplied.
    IF lines( it_warehouse_select_options ) EQ 0.
      " We need to read the default warehouse number from the parameter.
      GET PARAMETER ID '/SCWM/LGN' FIELD lv_warehouse_number.

      ls_select_option-sign = 'I'.
      ls_select_option-option = 'EQ'.
      ls_select_option-low = lv_warehouse_number.
      APPEND ls_select_option TO lt_warehouse_select_options.
    ELSE.
      lt_warehouse_select_options[] = it_warehouse_select_options[].
    ENDIF.

    " Get the UOM select option values.
    lt_uom_select_options = me->get_supported_uoms( ).

    " Have to select the SCUGUID in order to select the values from the
    " /SAPAPO/MATLWH table next.
    SELECT lgnum scuguid FROM /scwm/t300_md
      INTO TABLE lt_scu
      WHERE lgnum IN it_warehouse_select_options.

    LOOP AT lt_scu ASSIGNING <lfs_scu>.
      CLEAR lt_mat_id[].
      CLEAR lt_matid_matnr[].

      " Grab the MATID of each product for the warehouse.  Need to take this list
      " and compare against the Checked flag.
      SELECT DISTINCT matid FROM /sapapo/matlwh
        INTO TABLE lt_mat_id
        WHERE scuguid EQ <lfs_scu>-scuguid.

      IF lines( lt_mat_id ) GT 0.
        SELECT mk~matid mk~matnr FROM /sapapo/matkey AS mk
          INNER JOIN /sapapo/marm AS m ON mk~matid = m~matid
          INTO TABLE lt_matid_matnr
          FOR ALL ENTRIES IN lt_mat_id
          WHERE mk~matid EQ lt_mat_id-table_line
            AND mk~matnr IN it_product_select_options
            AND m~meinh IN lt_uom_select_options.

        LOOP AT lt_matid_matnr ASSIGNING <lfs_matid_matnr>.
          " Check if all UOM's are measured already.  If they have been then we don't need
          " to add this to the list.
          lv_is_measured = me->all_uoms_are_measured( iv_matnr = <lfs_matid_matnr>-matnr iv_matid = <lfs_matid_matnr>-matid ).
          IF lv_is_measured NE abap_true.
            ls_product-warehouse = <lfs_scu>-warehouse.
            ls_product-product = <lfs_matid_matnr>-matnr.

            " Select the product description.
            SELECT SINGLE maktx FROM /sapapo/mattxt
              INTO ls_product-product_description
              WHERE matid EQ <lfs_matid_matnr>-matid AND langu EQ sy-langu.

            APPEND ls_product TO rt_products.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    SORT rt_products BY warehouse product.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~query_review_items.
    TYPES:  BEGIN OF ltyp_entityset.
        INCLUDE TYPE zcubi_worklist_review_item.
    TYPES: matid TYPE /sapapo/matid,
           END OF ltyp_entityset.

    DATA: ls_select_option         TYPE /iwbep/s_cod_select_option,
          lt_taba                  TYPE STANDARD TABLE OF dd07v,
          lt_tabb                  TYPE STANDARD TABLE OF dd07v,
          lv_matid                 TYPE /sapapo/matid,
          ls_marm_key              TYPE /sapapo/pr_sel_marm,
          lt_marm_key              TYPE /sapapo/pr_set_marm,
          lt_db_marm               TYPE /sapapo/marm_tab,
          lt_items                 TYPE STANDARD TABLE OF ltyp_entityset,
          ls_item                  TYPE zcubi_worklist_review_item,
          lv_count                 TYPE i,
          lt_status_select_options TYPE /iwbep/t_cod_select_options.

    FIELD-SYMBOLS: <fs_entry>     TYPE ltyp_entityset,
                   <fs_dom_value> TYPE dd07v,
                   <fs_marm>      TYPE /sapapo/marm.

    lt_status_select_options[] = it_status_select_options[].

    " Get status domain values.
    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name = 'ZCUBI_STATUS'
        langu       = sy-langu
        withtext    = 'X'
      TABLES
        dd07v_tab_a = lt_taba
        dd07v_tab_n = lt_tabb.

    " Hard code the status filter value since this is the review report list.
    ls_select_option-sign = 'I'.
    ls_select_option-option = 'EQ'.
    ls_select_option-low = 'S'.
    APPEND ls_select_option TO lt_status_select_options.

    SELECT * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF TABLE lt_items
      WHERE warehouse IN it_warehouse_select_options
        AND product IN it_product_select_options
        AND uom IN it_uom_select_options
        AND status IN lt_status_select_options.

    " Fill the MatID in the source table.
    LOOP AT lt_items ASSIGNING <fs_entry>.
      " Select the matid from MATKEY.
      SELECT SINGLE matid INTO <fs_entry>-matid
        FROM /sapapo/matkey
        WHERE matnr EQ <fs_entry>-product.
      IF sy-subrc IS INITIAL.
        CLEAR ls_marm_key.
        ls_marm_key-matid = lv_matid.
        ls_marm_key-meinh = <fs_entry>-uom.
        APPEND ls_marm_key TO lt_marm_key.
      ENDIF.
    ENDLOOP.

    " Read MARM
    IF lines( lt_marm_key ) GT 0.
      CALL FUNCTION '/SAPAPO/DM_MARM_GET'
        EXPORTING
          it_sel_marm = lt_marm_key
        IMPORTING
          et_db_marm  = lt_db_marm.
    ENDIF.

    " Loop at the result set and fill the remaining data.
    LOOP AT lt_items ASSIGNING <fs_entry>.
      " Get the status text value.
      READ TABLE lt_taba ASSIGNING <fs_dom_value> WITH KEY domvalue_l = <fs_entry>-status.
      IF <fs_dom_value> IS ASSIGNED.
        <fs_entry>-status_text = <fs_dom_value>-ddtext.
      ENDIF.
      UNASSIGN <fs_dom_value>.

      " Lookup the old marm values.
      IF lines( lt_db_marm ) GT 0.
        READ TABLE lt_db_marm ASSIGNING <fs_marm> WITH KEY matid = <fs_entry>-matid meinh = <fs_entry>-uom.
        IF <fs_marm> IS ASSIGNED.
          <fs_entry>-length_old = <fs_marm>-laeng.
          <fs_entry>-width_old = <fs_marm>-breit.
          <fs_entry>-height_old = <fs_marm>-hoehe.
          <fs_entry>-g_weight_old = <fs_marm>-brgew.
          <fs_entry>-volume_old = <fs_marm>-volum.
        ENDIF.
        UNASSIGN <fs_marm>.
      ENDIF.

      " Check if the entry has exceptions.
      SELECT COUNT(*) FROM zcubi_wrklst_exp
        INTO lv_count
        WHERE warehouse EQ <fs_entry>-warehouse AND product = <fs_entry>-product AND uom EQ <fs_entry>-uom.
      IF lv_count GT 0.
        <fs_entry>-has_exceptions = abap_true.
      ENDIF.

      MOVE-CORRESPONDING <fs_entry> TO ls_item.
      APPEND ls_item TO rt_items.
    ENDLOOP.

    " Sort the results by UOM.
    SORT rt_items BY product uom.
  ENDMETHOD.


  METHOD zif_cubi_wrklst~update_item.
    DATA: lt_messages     TYPE bapiret2_t,
          ls_message      TYPE bapiret2,
          lv_success      TYPE boolean VALUE abap_true,
          ls_worklist     TYPE zcubi_worklist,
          lo_badi_handler TYPE REF TO zbadi_mscg_cubiscan.

    FIELD-SYMBOLS:  <lfs_message> TYPE bapiret2.

    " Check if the status is an update, if it is then we need to process
    " the updates to ECC.
    IF is_worklist_item-status EQ 'U'.
      CALL METHOD me->update_ecc
        EXPORTING
          is_worklist_item = is_worklist_item
        IMPORTING
          ev_success       = lv_success
          et_messages      = lt_messages.

      " Call the after update method.
      GET BADI lo_badi_handler.
      CALL BADI lo_badi_handler->on_after_update
        EXPORTING
          is_worklist_item = is_worklist_item.
    ENDIF.

    " Success is defaulted to true above.  Would only be different if the approval in ECC failed.
    IF lv_success EQ abap_true.
      " Now update the worklist.
      SELECT SINGLE * FROM zcubi_worklist
        INTO CORRESPONDING FIELDS OF ls_worklist
        WHERE warehouse EQ is_worklist_item-warehouse AND product EQ is_worklist_item-product
          AND uom EQ is_worklist_item-uom.
      MOVE-CORRESPONDING is_worklist_item TO ls_worklist.

      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist-changeutc TIME ZONE 'UTC'.
      ls_worklist-changeuser = sy-uname.
      UPDATE zcubi_worklist FROM ls_worklist.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.

        ls_message-type = 'S'.
        ls_message-id = 'ZCUBI_MESSAGES'.
        ls_message-number = '011'.
        ls_message-message_v1 = ls_worklist-product.
        ls_message-message_v2 = ls_worklist-uom.
        APPEND ls_message TO et_messages.
      ELSE.
        ROLLBACK WORK.

        ls_message-type = 'E'.
        ls_message-id = 'ZCUBI_MESSAGES'.
        ls_message-number = '028'.
        ls_message-message_v1 = ls_worklist-product.
        ls_message-message_v2 = ls_worklist-uom.
        APPEND ls_message TO et_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
