class ZCL_ZCUBISCAN_SERVICES_DPC_EXT definition
  public
  inheriting from ZCL_ZCUBISCAN_SERVICES_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
protected section.

  methods APPEND_MESSAGES_HTTP_HEADER .

  methods CUBISCANDEVICESE_CREATE_ENTITY
    redefinition .
  methods CUBISCANDEVICESE_GET_ENTITY
    redefinition .
  methods CUBISCANDEVICESE_GET_ENTITYSET
    redefinition .
  methods CUBISCANDEVICESE_UPDATE_ENTITY
    redefinition .
  methods PRODUCTDATASET_GET_ENTITY
    redefinition .
  methods PRODUCTLOOKUPSET_GET_ENTITYSET
    redefinition .
  methods PRODUCTUOMSET_GET_ENTITYSET
    redefinition .
  methods WORKLISTHEADERSE_GET_ENTITY
    redefinition .
  methods WORKLISTHEADERSE_GET_ENTITYSET
    redefinition .
  methods WORKLISTITEMSET_CREATE_ENTITY
    redefinition .
  methods WORKLISTITEMSET_GET_ENTITY
    redefinition .
  methods WORKLISTITEMSET_GET_ENTITYSET
    redefinition .
  methods WORKLISTITEMSET_UPDATE_ENTITY
    redefinition .
  methods WORKLISTREVIEW01_GET_ENTITYSET
    redefinition .
  methods WORKLISTREVIEWIT_DELETE_ENTITY
    redefinition .
  methods WORKLISTREVIEWIT_GET_ENTITY
    redefinition .
  methods WORKLISTREVIEWIT_GET_ENTITYSET
    redefinition .
  methods WORKLISTREVIEWIT_UPDATE_ENTITY
    redefinition .
  methods WORKLISTITEMSET_DELETE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZCUBISCAN_SERVICES_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
*  EXPORTING
*    IT_OPERATION_INFO =
**  CHANGING
**    cv_defer_mode     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD append_messages_http_header.
    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
          lt_messages          TYPE /iwbep/t_message_container,
          lv_message           TYPE string,
          lv_message_json      TYPE string,
          lv_output_json       TYPE string,
          lv_message_type      TYPE string,
          ls_http_header       TYPE ihttpnvp.

    CONSTANTS: lc_error       TYPE string VALUE 'Error',
               lc_information TYPE string VALUE 'Information',
               lc_success     TYPE string VALUE 'Success',
               lc_warning     TYPE string VALUE 'Warning'.


    FIELD-SYMBOLS:  <fs_message> TYPE /iwbep/s_message_container.

    lo_message_container = mo_context->get_message_container( ).
    lt_messages = lo_message_container->get_messages( ).

    lv_output_json = '{"messages":['.

    LOOP AT lt_messages ASSIGNING <fs_message>.
      CLEAR lv_message_json.

      CASE <fs_message>-type.
        WHEN 'E'.
          lv_message_type = lc_error.
        WHEN 'I'.
          lv_message_type = lc_information.
        WHEN 'S'.
          lv_message_type = lc_success.
        WHEN 'W'.
          lv_message_type = lc_warning.
      ENDCASE.


      MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number INTO lv_message
        WITH <fs_message>-message_v1 <fs_message>-message_v2 <fs_message>-message_v3 <fs_message>-message_v4.

      CONCATENATE '{"type":"' lv_message_type '","title":"' lv_message '"}' INTO lv_message_json.

      IF sy-tabix GT 1.
        CONCATENATE lv_output_json ',' INTO lv_output_json.
      ENDIF.
      CONCATENATE lv_output_json lv_message_json INTO lv_output_json.
    ENDLOOP.

    CONCATENATE lv_output_json ']}' INTO lv_output_json.

    " Append to the Http header.
    ls_http_header-name = 'zmscg-messages'.
    ls_http_header-value = lv_output_json.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_http_header ).
  ENDMETHOD.


  METHOD cubiscandevicese_create_entity.
    DATA: ls_data              TYPE zcubi_device,
          ls_cubiscan_device   TYPE zcubi_devices,
          lv_timestamp         TYPE timestamp,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lv_msg_text          TYPE bapi_msg,
          lv_worst_msg_type    TYPE symsgty.

    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.
    lo_message_container = mo_context->get_message_container( ).

    " Retrieve the data from the data provider object.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Make sure all required fields are filled.
    " warehouse, deviceName, deviceDescription, hostName and portNumber are all required.
    IF ls_data-warehouse_number IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Warehouse Number'.
    ENDIF.
    IF ls_data-device_name IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Device Name'.
    ENDIF.
    IF ls_data-device_description IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Device Description'.
    ENDIF.
    IF ls_data-host_name IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Host Name/IP Address'.
    ENDIF.
    IF ls_data-port_number IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Port Number'.
    ENDIF.

    " Check for errors.
    lv_worst_msg_type = lo_message_container->get_worst_message_type( ).
    IF lv_worst_msg_type EQ 'E'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    " Set the fixed values.
    MOVE-CORRESPONDING ls_data TO ls_cubiscan_device.

    " Insert the record into the worklist.  If for some reason it fails, throw an exception.
    INSERT zcubi_devices FROM ls_cubiscan_device.
    IF sy-subrc IS INITIAL.
      er_entity = ls_data.
    ELSE.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'E' NUMBER '000' INTO lv_msg_text.
      CALL METHOD lo_message_container->add_message_text_only
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = lv_msg_text.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
  ENDMETHOD.


  METHOD cubiscandevicese_get_entity.
    DATA: lv_warehouse_number TYPE /scwm/lgnum,
          lv_device_name      TYPE char40.

    FIELD-SYMBOLS: <fs_key> TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        lv_warehouse_number = <fs_key>-value.
      ELSEIF <fs_key>-name EQ 'deviceName'.
        lv_device_name = <fs_key>-value.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE * FROM zcubi_devices
      INTO CORRESPONDING FIELDS OF er_entity
      WHERE warehouse_number EQ lv_warehouse_number
        AND device_name EQ lv_device_name.
  ENDMETHOD.


  METHOD cubiscandevicese_get_entityset.
    DATA: lo_message_container       TYPE REF TO /iwbep/if_message_container,
          ls_warehouse_number_filter TYPE /iwbep/s_mgw_select_option.

    FIELD-SYMBOLS: <fs_filter> TYPE /iwbep/s_mgw_select_option.

    lo_message_container = mo_context->get_message_container( ).

    " Need to make sure they pass the filter value of warehouseNumber.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_number_filter.

    SELECT warehouse_number device_name device_description host_name port_number FROM zcubi_devices
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      WHERE warehouse_number IN ls_warehouse_number_filter-select_options
      ORDER BY device_name.
  ENDMETHOD.


  METHOD cubiscandevicese_update_entity.
    DATA: ls_data              TYPE zcubi_device,
          ls_cubiscan_device   TYPE zcubi_devices,
          lv_timestamp         TYPE timestamp,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lv_msg_text          TYPE bapi_msg,
          lv_worst_msg_type    TYPE symsgty.

    FIELD-SYMBOLS: <fs_key> TYPE /iwbep/s_mgw_name_value_pair.

    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.
    lo_message_container = mo_context->get_message_container( ).

    " Retrieve the data from the data provider object.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Read key values from the request.
    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        ls_data-warehouse_number = <fs_key>-value.
      ELSEIF <fs_key>-name EQ 'deviceName'.
        ls_data-device_name = <fs_key>-value.
      ENDIF.
    ENDLOOP.

    " Make sure all required fields are filled.
    " deviceDescription, hostName, portNumber are required.
    IF ls_data-device_description IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Device Description'.
    ENDIF.
    IF ls_data-host_name IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Host Name'.
    ENDIF.
    IF ls_data-port_number IS INITIAL.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '001'
          iv_msg_v1     = 'Port Number'.
    ENDIF.

    " Check for errors.
    lv_worst_msg_type = lo_message_container->get_worst_message_type( ).
    IF lv_worst_msg_type EQ 'E'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    " Set the fixed values.
    MOVE-CORRESPONDING ls_data TO ls_cubiscan_device.

    " Insert the record into the worklist.  If for some reason it fails, throw an exception.
    MODIFY zcubi_devices FROM ls_cubiscan_device.
    IF sy-subrc IS INITIAL.
      er_entity = ls_data.
    ELSE.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'E' NUMBER '000' INTO lv_msg_text.
      CALL METHOD lo_message_container->add_message_text_only
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = lv_msg_text.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
  ENDMETHOD.


  METHOD productdataset_get_entity.
    DATA: lv_material          TYPE /sapapo/matnr,
          lv_uom               TYPE /sapapo/lrmei,
          lv_mat_id            TYPE /sapapo/matid,
          lv_material_group    TYPE /sapapo/matkl,
          ls_marm              TYPE /sapapo/marm,
          ls_tolerance_data    TYPE zcubi_tolerances,
          lv_msg_var1          TYPE symsgv,
          lv_msg_var2          TYPE symsgv,
          lo_message_container TYPE REF TO /iwbep/if_message_container.

    FIELD-SYMBOLS: <fs_key> TYPE /iwbep/s_mgw_name_value_pair.

    lo_message_container = mo_context->get_message_container( ).

    " Get the key values first.
    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'product'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = lv_material.
      ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = lv_uom.
      ENDIF.
    ENDLOOP.

    " Now select the mat id for the material number.
    SELECT SINGLE matid matkl FROM /sapapo/matkey
      INTO (lv_mat_id, lv_material_group)
      WHERE matnr EQ lv_material.

    SELECT SINGLE * FROM /sapapo/marm
      INTO CORRESPONDING FIELDS OF ls_marm
      WHERE matid = lv_mat_id
        AND meinh = lv_uom.

    IF sy-subrc IS INITIAL.
      er_entity-material = lv_material.
      er_entity-unit_of_measure = lv_uom.
      er_entity-length = ls_marm-laeng.
      er_entity-length_uom = ls_marm-meabm.
      er_entity-width = ls_marm-breit.
      er_entity-width_uom = ls_marm-meabm.
      er_entity-height = ls_marm-hoehe.
      er_entity-height_uom = ls_marm-meabm.
      er_entity-gross_weight = ls_marm-brgew.
      er_entity-gross_weight_uom = ls_marm-gewei.
      er_entity-net_weight = ls_marm-ntgew.
      er_entity-net_weight_uom = ls_marm-gewei.
      er_entity-volume = ls_marm-volum.
      er_entity-volume_uom = ls_marm-voleh.

      " Set some defaults if nothing defined as the Cubiscan will default to these anyway.
      IF er_entity-length_uom IS INITIAL.
        er_entity-length_uom = 'IN'.
        er_entity-width_uom = 'IN'.
        er_entity-height_uom = 'IN'.
      ENDIF.

      IF er_entity-gross_weight_uom IS INITIAL.
        er_entity-gross_weight_uom = 'LB'.
        er_entity-net_weight_uom = 'LB'.
      ENDIF.

      IF er_entity-volume_uom IS INITIAL.
        er_entity-volume_uom = 'FT3'.
      ENDIF.

      " Need to select the Case record and pull the numerator and denominator.
      " Hard coded.
      SELECT SINGLE * FROM /sapapo/marm
        INTO CORRESPONDING FIELDS OF ls_marm
        WHERE matid = lv_mat_id
          AND meinh = 'CS'.
      IF sy-subrc EQ 0.
        MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'S' NUMBER '009'
          WITH ls_marm-umren ls_marm-umrez
          INTO er_entity-uom_conversion_txt.
      ENDIF.

      " Need to retrieve and fill the tolerance data for the given material group.
      SELECT SINGLE * INTO ls_tolerance_data
        FROM zcubi_tolerances
        WHERE material_group EQ lv_material_group.
      IF sy-subrc IS INITIAL.
        er_entity-tolerance_length_upper = ls_tolerance_data-length_upper.
        er_entity-tolerance_length_lower = ls_tolerance_data-length_lower.
        er_entity-tolerance_width_upper = ls_tolerance_data-width_upper.
        er_entity-tolerance_width_lower = ls_tolerance_data-width_lower.
        er_entity-tolerance_height_upper = ls_tolerance_data-height_upper.
        er_entity-tolerance_height_lower = ls_tolerance_data-height_lower.
        er_entity-tolerance_volume_upper = ls_tolerance_data-volume_upper.
        er_entity-tolerance_volume_lower = ls_tolerance_data-volume_lower.
        er_entity-tolerance_gweight_upper = ls_tolerance_data-gweight_upper.
        er_entity-tolerance_gweight_lower = ls_tolerance_data-gweight_lower.
        er_entity-tolerance_nweight_upper = ls_tolerance_data-nweight_upper.
        er_entity-tolerance_nweight_lower = ls_tolerance_data-nweight_lower.
      ENDIF.
    ELSE.
      MOVE lv_material TO lv_msg_var1.
      MOVE lv_uom TO lv_msg_var2.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '002'
          iv_msg_v1     = lv_msg_var1
          iv_msg_v2     = lv_msg_var2.
    ENDIF.
  ENDMETHOD.


  METHOD productlookupset_get_entityset.
    DATA: ls_product_filter    TYPE /iwbep/s_mgw_select_option,
          ls_warehouse_filter  TYPE /iwbep/s_mgw_select_option,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lo_worklist          TYPE REF TO zif_cubi_wrklst,
          lt_data              TYPE zcubi_product_lookup_tt,
          lv_start             TYPE i VALUE 1,
          lv_end               TYPE i.

    FIELD-SYMBOLS: <lfs_data>          TYPE zcubi_product_lookup.

    lo_message_container = mo_context->get_message_container( ).

    " Read the warehouse filter value if it is passed.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_filter.

    " Read the product filter value if it is passed.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'product' INTO ls_product_filter.

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_products
      EXPORTING
        it_warehouse_select_options = ls_warehouse_filter-select_options
        it_product_select_options   = ls_product_filter-select_options
      RECEIVING
        rt_products                 = lt_data.

    " Handle paging of the data if necessary.
    IF is_paging-skip IS NOT INITIAL.
      lv_start = is_paging-skip + 1.
    ENDIF.

    IF is_paging-top IS NOT INITIAL.
      lv_end = is_paging-top + lv_start - 1.
    ELSE.
      lv_end = lines( lt_data ).
    ENDIF.

    LOOP AT lt_data ASSIGNING <lfs_data> FROM lv_start TO lv_end.
      APPEND <lfs_data> TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD productuomset_get_entityset.
    DATA: lv_mat_id             TYPE /sapapo/matid,
          lt_uom                TYPE STANDARD TABLE OF meinh,
          ls_entity             LIKE LINE OF et_entityset,
          lo_message_container  TYPE REF TO /iwbep/if_message_container,
          ls_select_option      TYPE /iwbep/s_cod_select_option,
          lt_uoms               TYPE STANDARD TABLE OF /sapapo/meins,
          lt_configuration      TYPE zmscg_configuration_tt,
          lt_uom_select_options TYPE /iwbep/t_cod_select_options,
          lv_product_number     TYPE /sapapo/matnr,
          lt_worklist_items     TYPE STANDARD TABLE OF zcubi_worklist_item,
          ls_warehouse_filter   TYPE /iwbep/s_mgw_select_option.

    FIELD-SYMBOLS: <lfs_filter>        TYPE /iwbep/s_mgw_select_option,
                   <lfs_option>        TYPE /iwbep/s_cod_select_option,
                   <lfs_config>        TYPE zmscg_configuration,
                   <lfs_uom>           TYPE /sapapo/meins,
                   <lfs_worklist_item> TYPE zcubi_worklist_item.

    lo_message_container = mo_context->get_message_container( ).

    " Read configuration.
    CALL METHOD zcl_mscg_common_utils=>get_configuration
      EXPORTING
        iv_config_application = 'CUBISCAN'
        iv_config_type        = 1 "Backend Configuration
      RECEIVING
        rt_configuration      = lt_configuration.

    " Read the filter value for warehouse.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_filter.

    " Read the filter value for product.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'product' ASSIGNING <lfs_filter>.

    IF <lfs_filter> IS ASSIGNED.
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

      LOOP AT <lfs_filter>-select_options ASSIGNING <lfs_option>.
        CLEAR lt_uom[].
        CLEAR lt_worklist_items[].

        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <lfs_option>-low
          IMPORTING
            output = lv_product_number.

        " Select the mat id first.
        SELECT SINGLE matid FROM /sapapo/matkey
          INTO lv_mat_id
          WHERE matnr EQ lv_product_number.

        SELECT meinh FROM /sapapo/marm
          INTO TABLE lt_uom
          WHERE matid EQ lv_mat_id
            AND meinh IN lt_uom_select_options.

        " Read the worklist and determine the UOM's that are currently in there.
        " They cannot be added to the worklist if they already exist so they need
        " to be filtered out.
        SELECT * FROM zcubi_worklist
          INTO CORRESPONDING FIELDS OF TABLE lt_worklist_items
          WHERE warehouse IN ls_warehouse_filter-select_options AND product EQ lv_product_number.

        LOOP AT lt_uom ASSIGNING <lfs_uom>.
          READ TABLE lt_worklist_items ASSIGNING <lfs_worklist_item>
            WITH KEY product = lv_product_number uom = <lfs_uom>.
          IF <lfs_worklist_item> IS NOT ASSIGNED.
            ls_entity-product = lv_product_number.

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
              APPEND ls_entity TO et_entityset.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      SORT et_entityset BY product uom_key.
    ENDIF.
  ENDMETHOD.


  METHOD worklistheaderse_get_entity.
    DATA: lo_message_container        TYPE REF TO /iwbep/if_message_container,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          lt_product_select_options   TYPE /iwbep/t_cod_select_options,
          lt_headers                  TYPE zcubi_worklist_header_tt,
          lo_worklist                 TYPE REF TO zif_cubi_wrklst.

    FIELD-SYMBOLS: <fs_key>            TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <fs_key>-value.
        APPEND ls_select_option TO lt_warehouse_select_options.
      ELSEIF <fs_key>-name EQ 'product'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <fs_key>-value.
        APPEND ls_select_option TO lt_product_select_options.
      ENDIF.
    ENDLOOP.

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_headers
      EXPORTING
        it_warehouse_select_options = lt_warehouse_select_options
        it_product_select_options   = lt_product_select_options
      RECEIVING
        rt_headers                  = lt_headers.

    READ TABLE lt_headers INDEX 1 INTO er_entity.
  ENDMETHOD.


  METHOD worklistheaderse_get_entityset.
    DATA: ls_product_filter    TYPE /iwbep/s_mgw_select_option,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lo_worklist          TYPE REF TO zif_cubi_wrklst,
          lt_data              TYPE zcubi_worklist_header_tt,
          lv_start             TYPE i VALUE 1,
          lv_end               TYPE i.

    FIELD-SYMBOLS: <lfs_warehouse_filter> TYPE /iwbep/s_mgw_select_option,
                   <lfs_data>             TYPE zcubi_worklist_header.

    lo_message_container = mo_context->get_message_container( ).

    " Need to make sure they pass the filter value of warehouseNumber, it is required.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' ASSIGNING <lfs_warehouse_filter>.
    IF <lfs_warehouse_filter> IS NOT ASSIGNED.
      " Not assigned, throw a message and exit.  Means the filter was not supplied.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZCUBI_MESSAGES'
          iv_msg_number = '003'
          iv_msg_v1     = 'warehouseNumber'.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    " Read the product filter value if it is passed.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'product' INTO ls_product_filter.

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_headers
      EXPORTING
        it_warehouse_select_options = <lfs_warehouse_filter>-select_options
        it_product_select_options   = ls_product_filter-select_options
      RECEIVING
        rt_headers                  = lt_data.

    " Handle paging of the data if necessary.
    IF is_paging-skip IS NOT INITIAL.
      lv_start = is_paging-skip + 1.
    ENDIF.

    IF is_paging-top IS NOT INITIAL.
      lv_end = is_paging-top + lv_start - 1.
    ELSE.
      lv_end = lines( lt_data ).
    ENDIF.

    LOOP AT lt_data ASSIGNING <lfs_data> FROM lv_start TO lv_end.
      APPEND <lfs_data> TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD worklistitemset_create_entity.
    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
          lo_worklist          TYPE REF TO zif_cubi_wrklst,
          ls_data              TYPE zcubi_worklist_item,
          lv_warehouse_number  TYPE /scwm/lgnum,
          lv_product           TYPE /sapapo/matnr,
          lv_uom               TYPE /sapapo/meins,
          ls_return            TYPE bapiret2.

    FIELD-SYMBOLS: <lfs_key>           TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <lfs_key>.
      IF <lfs_key>-name EQ 'warehouseNumber'.
        lv_warehouse_number = <lfs_key>-value.
      ELSEIF <lfs_key>-name EQ 'product'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_product.
      ELSEIF <lfs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_uom.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Retrieve the data from the data provider object.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->create_item
      EXPORTING
        is_worklist_item = ls_data
      RECEIVING
        rs_return        = ls_return.

    CALL METHOD lo_message_container->add_message_from_bapi
      EXPORTING
        is_bapi_message   = ls_return
        iv_message_target = 'Create'.

    me->append_messages_http_header( ).
  ENDMETHOD.


  METHOD worklistitemset_delete_entity.
    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
          ls_worklist_item     TYPE zcubi_worklist_item,
          lo_worklist          TYPE REF TO zif_cubi_wrklst,
          lv_success           TYPE boolean,
          lt_messages          TYPE bapiret2_t,
          lv_update_key        TYPE char80,
          ls_http_header       TYPE ihttpnvp.

    FIELD-SYMBOLS: <fs_key>            TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        ls_worklist_item-warehouse = <fs_key>-value.
      ELSEIF <fs_key>-name EQ 'product'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = ls_worklist_item-product.
      ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = ls_worklist_item-uom.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Add Http header for the delete key.
    CONCATENATE '{"warehouseNumber":"' ls_worklist_item-warehouse '","product":"' ls_worklist_item-product '","unitOfMeasure":"' ls_worklist_item-uom '"}' INTO lv_update_key.
    ls_http_header-name = 'zmscg-update-key'.
    ls_http_header-value = lv_update_key.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_http_header ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->delete_item
      EXPORTING
        is_worklist_item = ls_worklist_item
      IMPORTING
        ev_success       = lv_success
        et_messages      = lt_messages.

    CALL METHOD lo_message_container->add_messages_from_bapi
      EXPORTING
        it_bapi_messages = lt_messages.

    me->append_messages_http_header( ).
  ENDMETHOD.


  METHOD worklistitemset_get_entity.
    DATA: lo_message_container        TYPE REF TO /iwbep/if_message_container,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          lt_product_select_options   TYPE /iwbep/t_cod_select_options,
          lt_uom_select_options       TYPE /iwbep/t_cod_select_options,
          lt_items                    TYPE zcubi_worklist_item_tt,
          lo_worklist                 TYPE REF TO zif_cubi_wrklst.

    FIELD-SYMBOLS: <fs_key>            TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <fs_key>-value.
        APPEND ls_select_option TO lt_warehouse_select_options.
      ELSEIF <fs_key>-name EQ 'product'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = ls_select_option-low.
        APPEND ls_select_option TO lt_product_select_options.
      ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = ls_select_option-low.
        APPEND ls_select_option TO lt_uom_select_options.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_items
      EXPORTING
        it_warehouse_select_options = lt_warehouse_select_options
        it_product_select_options   = lt_product_select_options
        it_uom_select_options       = lt_uom_select_options
      RECEIVING
        rt_items                    = lt_items.

    READ TABLE lt_items INDEX 1 INTO er_entity.
  ENDMETHOD.


  METHOD worklistitemset_get_entityset.
    DATA: ls_warehouse_filter  TYPE /iwbep/s_mgw_select_option,
          ls_product_filter    TYPE /iwbep/s_mgw_select_option,
          ls_status_filter     TYPE /iwbep/s_mgw_select_option,
          ls_select_option     TYPE /iwbep/s_cod_select_option,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lo_worklist          TYPE REF TO zif_cubi_wrklst,
          lt_data              TYPE zcubi_worklist_item_tt,
          lv_start             TYPE i VALUE 1,
          lv_end               TYPE i.

    FIELD-SYMBOLS: <fs_key>   TYPE /iwbep/s_mgw_name_value_pair,
                   <lfs_data> LIKE LINE OF lt_data.

    lo_message_container = mo_context->get_message_container( ).

    " If the source name is the header, then the key values will come across in IT_KEY_TAB rather than in
    " IT_FILTER_SELECT_OPTIONS.
    IF iv_source_name EQ 'WorklistHeader'.
      LOOP AT it_key_tab ASSIGNING <fs_key>.
        IF <fs_key>-name EQ 'warehouseNumber'.
          ls_select_option-sign = 'I'.
          ls_select_option-option = 'EQ'.
          ls_select_option-low = <fs_key>-value.
          APPEND ls_select_option TO ls_warehouse_filter-select_options.
        ELSEIF <fs_key>-name EQ 'product'.
          ls_select_option-sign = 'I'.
          ls_select_option-option = 'EQ'.
          CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
            EXPORTING
              input  = <fs_key>-value
            IMPORTING
              output = ls_select_option-low.
          APPEND ls_select_option TO ls_product_filter-select_options.
        ENDIF.
      ENDLOOP.
    ELSE.
      " Read the warehouse number filter value if it is passed.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_filter.

      " Read the product filter value if it is passed.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'product' INTO ls_product_filter.

      " Read the status filter value if it is passed.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'status' INTO ls_status_filter.
    ENDIF.

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_items
      EXPORTING
        it_warehouse_select_options = ls_warehouse_filter-select_options
        it_product_select_options   = ls_product_filter-select_options
        it_status_select_options    = ls_status_filter-select_options
      RECEIVING
        rt_items                    = lt_data.

    " Handle paging of the data if necessary.
    IF is_paging-skip IS NOT INITIAL.
      lv_start = is_paging-skip + 1.
    ENDIF.

    IF is_paging-top IS NOT INITIAL.
      lv_end = is_paging-top + lv_start - 1.
    ELSE.
      lv_end = lines( lt_data ).
    ENDIF.

    LOOP AT lt_data ASSIGNING <lfs_data> FROM lv_start TO lv_end.
      APPEND <lfs_data> TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD worklistitemset_update_entity.
    DATA: lv_warehouse_number  TYPE /scwm/lgnum,
          lv_product           TYPE /sapapo/matnr,
          lv_uom               TYPE /sapapo/meins,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          ls_data              TYPE zcubi_worklist_item,
          ls_worklist          TYPE zcubi_worklist_item,
          lv_success           TYPE boolean,
          lt_messages          TYPE bapiret2_t,
          lv_update_key        TYPE char80,
          ls_http_header       TYPE ihttpnvp,
          lo_worklist          TYPE REF TO zif_cubi_wrklst.

    FIELD-SYMBOLS: <lfs_key>           TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <lfs_key>.
      IF <lfs_key>-name EQ 'warehouseNumber'.
        lv_warehouse_number = <lfs_key>-value.
      ELSEIF <lfs_key>-name EQ 'product'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_product.
      ELSEIF <lfs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_uom.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Retrieve the data from the data provider object.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Read the record from the table and map the values.
    SELECT SINGLE * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF ls_worklist
      WHERE warehouse EQ lv_warehouse_number
        AND product EQ lv_product
        AND uom EQ lv_uom.

    " Move the data to the worklist structure for updating of the table.
    MOVE-CORRESPONDING ls_data TO ls_worklist.

    " Add Http header for the update key.
    CONCATENATE '{"warehouseNumber":"' ls_data-warehouse '","product":"' ls_data-product '","unitOfMeasure":"' ls_data-uom '"}' INTO lv_update_key.
    ls_http_header-name = 'zmscg-update-key'.
    ls_http_header-value = lv_update_key.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_http_header ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->update_item
      EXPORTING
        is_worklist_item = ls_worklist
      IMPORTING
        ev_success       = lv_success
        et_messages      = lt_messages.

    CALL METHOD lo_message_container->add_messages_from_bapi
      EXPORTING
        it_bapi_messages = lt_messages.

    me->append_messages_http_header( ).
  ENDMETHOD.


  METHOD worklistreview01_get_entityset.
    DATA: ls_warehouse_filter  TYPE /iwbep/s_mgw_select_option,
          ls_product_filter    TYPE /iwbep/s_mgw_select_option,
          ls_uom_filter        TYPE /iwbep/s_mgw_select_option,
          ls_select_option     TYPE /iwbep/s_cod_select_option,
          lo_message_container TYPE REF TO /iwbep/if_message_container.

    FIELD-SYMBOLS: <fs_key>       TYPE /iwbep/s_mgw_name_value_pair.

    lo_message_container = mo_context->get_message_container( ).

    " If the source name is the WorklistReviewItem, then the key values will come across in IT_KEY_TAB rather than in
    " IT_FILTER_SELECT_OPTIONS.
    IF iv_source_name EQ 'WorklistReviewItem'.
      LOOP AT it_key_tab ASSIGNING <fs_key>.
        IF <fs_key>-name EQ 'warehouseNumber'.
          ls_select_option-sign = 'I'.
          ls_select_option-option = 'EQ'.
          ls_select_option-low = <fs_key>-value.
          APPEND ls_select_option TO ls_warehouse_filter-select_options.
        ELSEIF <fs_key>-name EQ 'product'.
          ls_select_option-sign = 'I'.
          ls_select_option-option = 'EQ'.
          CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
            EXPORTING
              input  = <fs_key>-value
            IMPORTING
              output = ls_select_option-low.
          APPEND ls_select_option TO ls_product_filter-select_options.
        ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
          ls_select_option-sign = 'I'.
          ls_select_option-option = 'EQ'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input  = <fs_key>-value
            IMPORTING
              output = ls_select_option-low.
          APPEND ls_select_option TO ls_uom_filter-select_options.
        ENDIF.
      ENDLOOP.
    ELSE.
      " Need to make sure they pass the filter value of warehouseNumber, it is required.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_filter.
      IF sy-subrc IS NOT INITIAL.
        " Not assigned, throw a message and exit.  Means the filter was not supplied.
        CALL METHOD lo_message_container->add_message
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = 'ZCUBI_MESSAGES'
            iv_msg_number = '003'
            iv_msg_v1     = 'warehouseNumber'.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ENDIF.

      " Need to make sure they pass the filter value of product, it is required.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'product' INTO ls_product_filter.

      " Need to make sure they pass the filter value of unitOfMeasure, it is required.
      READ TABLE it_filter_select_options WITH TABLE KEY property = 'unitOfMeasure' INTO ls_uom_filter.
    ENDIF.

    SELECT * FROM zcubi_wrklst_exp
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      WHERE warehouse IN ls_warehouse_filter-select_options
        AND product IN ls_product_filter-select_options
        AND uom IN ls_uom_filter-select_options.

    " Sort the results by warehouse, product, uom.
    SORT et_entityset BY warehouse product uom.
  ENDMETHOD.


  METHOD worklistreviewit_delete_entity.
    DATA: lv_warehouse_number  TYPE /scwm/lgnum,
          lv_product           TYPE /sapapo/matnr,
          lv_uom               TYPE /sapapo/meins,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          ls_data              TYPE zcubi_worklist_review_item,
          ls_worklist          TYPE zcubi_worklist,
          lv_success           TYPE boolean,
          lt_messages          TYPE bapiret2_t,
          ls_message           TYPE bapiret2,
          lv_worst_msg_type    TYPE symsgty,
          lv_msg_var           TYPE symsgv,
          lv_update_key        TYPE char80,
          ls_http_header       TYPE ihttpnvp,
          lv_change_timestamp  TYPE timestamp.

    FIELD-SYMBOLS: <fs_key>     TYPE /iwbep/s_mgw_name_value_pair,
                   <fs_message> TYPE bapiret2.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        lv_warehouse_number = <fs_key>-value.
      ELSEIF <fs_key>-name EQ 'product'.
        lv_product = <fs_key>-value.
      ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = lv_uom.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Add Http header for the update key.
    CONCATENATE '{"warehouseNumber":"' lv_warehouse_number '","product":"' lv_product '","unitOfMeasure":"' lv_uom '"}' INTO lv_update_key.
    ls_http_header-name = 'zmscg-update-key'.
    ls_http_header-value = lv_update_key.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_http_header ).

    " Delete the entry from the worklist.
    DELETE FROM zcubi_worklist WHERE warehouse EQ lv_warehouse_number AND product EQ lv_product AND uom EQ lv_uom.

    " Add success message.
    ls_message-type = 'S'.
    ls_message-id = 'ZCUBI_MESSAGES'.
    ls_message-number = '012'.
    ls_message-message_v1 = ls_worklist-product.
    ls_message-message_v2 = ls_worklist-uom.

    CALL METHOD lo_message_container->add_message_from_bapi
      EXPORTING
        is_bapi_message   = ls_message
        iv_message_target = 'Reject'.

    me->append_messages_http_header( ).
  ENDMETHOD.


  METHOD worklistreviewit_get_entity.
    DATA: lo_message_container        TYPE REF TO /iwbep/if_message_container,
          ls_select_option            TYPE /iwbep/s_cod_select_option,
          lt_warehouse_select_options TYPE /iwbep/t_cod_select_options,
          lt_product_select_options   TYPE /iwbep/t_cod_select_options,
          lt_uom_select_options       TYPE /iwbep/t_cod_select_options,
          lt_items                    TYPE zcubi_worklist_review_item_tt,
          lo_worklist                 TYPE REF TO zif_cubi_wrklst.

    FIELD-SYMBOLS: <fs_key>            TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <fs_key>.
      IF <fs_key>-name EQ 'warehouseNumber'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <fs_key>-value.
        APPEND ls_select_option TO lt_warehouse_select_options.
      ELSEIF <fs_key>-name EQ 'product'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        ls_select_option-low = <fs_key>-value.
        APPEND ls_select_option TO lt_product_select_options.
      ELSEIF <fs_key>-name EQ 'unitOfMeasure'.
        ls_select_option-sign = 'I'.
        ls_select_option-option = 'EQ'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <fs_key>-value
          IMPORTING
            output = ls_select_option-low.
        APPEND ls_select_option TO lt_uom_select_options.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_review_items
      EXPORTING
        it_warehouse_select_options = lt_warehouse_select_options
        it_product_select_options   = lt_product_select_options
        it_uom_select_options       = lt_uom_select_options
      RECEIVING
        rt_items                    = lt_items.

    READ TABLE lt_items INDEX 1 INTO er_entity.
  ENDMETHOD.


  METHOD worklistreviewit_get_entityset.
    DATA: ls_warehouse_filter  TYPE /iwbep/s_mgw_select_option,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          lo_worklist          TYPE REF TO zif_cubi_wrklst.

    lo_message_container = mo_context->get_message_container( ).

    " Read filter value for warehouse number.
    READ TABLE it_filter_select_options WITH TABLE KEY property = 'warehouseNumber' INTO ls_warehouse_filter.

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->query_review_items
      EXPORTING
        it_warehouse_select_options = ls_warehouse_filter-select_options
      RECEIVING
        rt_items                    = et_entityset.
  ENDMETHOD.


  METHOD worklistreviewit_update_entity.
    DATA: lv_warehouse_number  TYPE /scwm/lgnum,
          lv_product           TYPE /sapapo/matnr,
          lv_uom               TYPE /sapapo/meins,
          lo_message_container TYPE REF TO /iwbep/if_message_container,
          ls_data              TYPE zcubi_worklist_review_item,
          ls_worklist          TYPE zcubi_worklist_item,
          lv_success           TYPE boolean,
          lt_messages          TYPE bapiret2_t,
          lv_update_key        TYPE char80,
          ls_http_header       TYPE ihttpnvp,
          lo_worklist          TYPE REF TO zif_cubi_wrklst.

    FIELD-SYMBOLS: <lfs_key>           TYPE /iwbep/s_mgw_name_value_pair.

    LOOP AT it_key_tab ASSIGNING <lfs_key>.
      IF <lfs_key>-name EQ 'warehouseNumber'.
        lv_warehouse_number = <lfs_key>-value.
      ELSEIF <lfs_key>-name EQ 'product'.
        CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_product.
      ELSEIF <lfs_key>-name EQ 'unitOfMeasure'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input  = <lfs_key>-value
          IMPORTING
            output = lv_uom.
      ENDIF.
    ENDLOOP.

    lo_message_container = mo_context->get_message_container( ).

    " Retrieve the data from the data provider object.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Read the record from the table.
    SELECT SINGLE * FROM zcubi_worklist
      INTO CORRESPONDING FIELDS OF ls_worklist
      WHERE warehouse EQ lv_warehouse_number
        AND product EQ lv_product
        AND uom EQ lv_uom.

    " Move the data to the worklist structure for updating of the table.
    MOVE-CORRESPONDING ls_data TO ls_worklist.

    " Add Http header for the update key.
    CONCATENATE '{"warehouseNumber":"' ls_data-warehouse '","product":"' ls_data-product '","unitOfMeasure":"' ls_data-uom '"}' INTO lv_update_key.
    ls_http_header-name = 'zmscg-update-key'.
    ls_http_header-value = lv_update_key.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_http_header ).

    " Instantiate the proper worklist implementation, left to the factory.
    lo_worklist = zcl_cubi_wrklist_factory=>create( ).
    CALL METHOD lo_worklist->update_item
      EXPORTING
        is_worklist_item = ls_worklist
      IMPORTING
        ev_success       = lv_success
        et_messages      = lt_messages.

    CALL METHOD lo_message_container->add_messages_from_bapi
      EXPORTING
        it_bapi_messages = lt_messages.

    me->append_messages_http_header( ).
  ENDMETHOD.
ENDCLASS.
