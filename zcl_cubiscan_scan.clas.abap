class ZCL_CUBISCAN_SCAN definition
  public
  final
  create public .

public section.

  constants GC_STATUS_OK type I value 1 ##NO_TEXT.
  constants GC_STATUS_CAUTION type I value 2 ##NO_TEXT.
  constants GC_STATUS_ERROR type I value 3 ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_WAREHOUSE_NUMBER type /SCWM/LGNUM
      !IV_PRODUCT type /SAPAPO/MATNR
      !IV_UOM type /SAPAPO/MEINS
      !IV_GATE_MEASURE_STR type STRING .
  methods GET_DATA
    returning
      value(RV_DATA) type ZCUBI_GATE_MEASURE_DATA .
protected section.

  data GS_GATE_MEASURE_DATA type ZCUBI_GATE_MEASURE_DATA .
  data GV_PRODUCT type /SAPAPO/MATNR .
  data GV_UOM type /SAPAPO/MEINS .
  data GV_WAREHOUSE_NUMBER type /SCWM/LGNUM .
  data GT_CONFIGURATION type ZMSCG_CONFIGURATION_TT .

  methods VALIDATE_RULES .
  methods IS_WITHIN_TOLERANCES
    importing
      !IV_CUBISCAN_VALUE type BASMN
      !IV_PRODUCT_MASTER_VALUE type BASMN
      !IV_LOWER_LIMIT_PERCENT type ZCUBI_TOLERANCE
      !IV_UPPER_LIMIT_PERCENT type ZCUBI_TOLERANCE
    exporting
      !EV_VALID type BOOLEAN
      !EV_EXCEPTION_MESSAGE type ZCUBI_EXCEPTION_MESSAGE .
  methods VALIDATE_VALUE
    importing
      !IV_CUBISCAN_VALUE type BASMN
      !IV_PRODUCT_MASTER_VALUE type BASMN
      !IV_LOWER_LIMIT_PERCENT type ZCUBI_TOLERANCE
      !IV_UPPER_LIMIT_PERCENT type ZCUBI_TOLERANCE
    exporting
      !EV_STATUS type ZCUBI_TOLERANCE_STATUS
      !EV_EXCEPTION_MESSAGE type ZCUBI_EXCEPTION_MESSAGE .
  methods VALIDATE_UOM
    importing
      !IV_CUBISCAN_UOM type MEINS
      !IV_PRODUCT_MASTER_UOM type MEINS
    exporting
      !EV_STATUS type ZCUBI_TOLERANCE_STATUS
      !EV_EXCEPTION_MESSAGE type ZCUBI_EXCEPTION_MESSAGE .
private section.
ENDCLASS.



CLASS ZCL_CUBISCAN_SCAN IMPLEMENTATION.


  METHOD constructor.
    DATA: ls_worklist TYPE zcubi_worklist.

    gv_warehouse_number = iv_warehouse_number.
    CALL FUNCTION 'CONVERSION_EXIT_PRODU_INPUT'
      EXPORTING
        input  = iv_product
      IMPORTING
        output = gv_product.
    gv_uom = iv_uom.

    CALL METHOD zcl_mscg_common_utils=>get_configuration
      EXPORTING
        iv_config_application = 'CUBISCAN'
        iv_config_type        = 1
      RECEIVING
        rt_configuration      = gt_configuration.

    " Parse out the length, width and height.
    gs_gate_measure_data-length = iv_gate_measure_str+12(5).
    gs_gate_measure_data-width = iv_gate_measure_str+19(5).
    gs_gate_measure_data-height = iv_gate_measure_str+26(5).
    gs_gate_measure_data-dimension_uom = iv_gate_measure_str+31(2).
    TRANSLATE gs_gate_measure_data-dimension_uom TO UPPER CASE.

    " Parse out the gross and dimensional weight.
    gs_gate_measure_data-gross_weight = iv_gate_measure_str+35(6).
    gs_gate_measure_data-dim_weight = iv_gate_measure_str+43(6).
    gs_gate_measure_data-weight_uom = iv_gate_measure_str+49(2).
    TRANSLATE gs_gate_measure_data-weight_uom TO UPPER CASE.

    " Calculate the volume.
    gs_gate_measure_data-volume = ( gs_gate_measure_data-length * gs_gate_measure_data-width * gs_gate_measure_data-height ).
    IF gs_gate_measure_data-dimension_uom EQ 'IN'.
      gs_gate_measure_data-volume_uom = 'IN3'.
    ELSE.
      gs_gate_measure_data-volume_uom = 'CCM'.
    ENDIF.

    " Delete any records from the exceptions table.
    DELETE FROM zcubi_wrklst_exp
      WHERE warehouse = gv_warehouse_number AND product = gv_product AND uom = gv_uom.

    " Read the current worklist record.
    SELECT SINGLE * FROM zcubi_worklist INTO ls_worklist
      WHERE warehouse EQ gv_warehouse_number AND product EQ gv_product AND uom EQ gv_uom.

    " This code is used when the worklist record was retrieved from somewhere other than the local worklist.
    " For instance MATLWH.
    IF sy-subrc IS NOT INITIAL.
      ls_worklist-warehouse = gv_warehouse_number.
      ls_worklist-product = gv_product.
      ls_worklist-uom = gv_uom.
      ls_worklist-createuser = sy-uname.
      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist-createutc TIME ZONE 'UTC'.
      ls_worklist-status = 'N'.
    ENDIF.

    " Update the record now.
    ls_worklist-length = gs_gate_measure_data-length.
    ls_worklist-width = gs_gate_measure_data-width.
    ls_worklist-height = gs_gate_measure_data-height.
    ls_worklist-meabm = gs_gate_measure_data-dimension_uom.
    ls_worklist-g_weight = gs_gate_measure_data-gross_weight.
    ls_worklist-gewei = gs_gate_measure_data-weight_uom.
    ls_worklist-volume = gs_gate_measure_data-volume.
    ls_worklist-voleh = gs_gate_measure_data-volume_uom.
    ls_worklist-changeuser = sy-uname.
    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP ls_worklist-changeutc TIME ZONE 'UTC'.
    MODIFY zcubi_worklist FROM ls_worklist.

    " Validate all rules/tolerance data.
    CALL METHOD me->validate_rules.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD get_data.
    rv_data = gs_gate_measure_data.
  ENDMETHOD.


  METHOD is_within_tolerances.
    DATA: lv_upper_value    TYPE basmn,
          lv_lower_value    TYPE basmn,
          lv_cubiscan_value TYPE basmn.

    MOVE iv_cubiscan_value TO lv_cubiscan_value.

    " Calculate the upper and lower value based on the product master value.
    lv_lower_value = iv_product_master_value - ( ( iv_lower_limit_percent / 100 ) * iv_product_master_value ).
    lv_upper_value = iv_product_master_value + ( ( iv_upper_limit_percent / 100 ) * iv_product_master_value ).
    IF lv_cubiscan_value GT lv_upper_value.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '020' INTO ev_exception_message.
      ev_valid = abap_false.
    ELSEIF lv_cubiscan_value LT lv_lower_value.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '019' INTO ev_exception_message.
      ev_valid = abap_false.
    ELSE.
      ev_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD validate_rules.
    DATA: lv_mat_id            TYPE /sapapo/matid,
          lv_material_group    TYPE /sapapo/matkl,
          ls_marm              TYPE /sapapo/marm,
          ls_tolerance_data    TYPE zcubi_tolerances,
          lt_exceptions        TYPE STANDARD TABLE OF zcubi_wrklst_exp,
          ls_exception         LIKE LINE OF lt_exceptions,
          lv_valid             TYPE boolean,
          lv_exception_message TYPE zcubi_exception_message,
          lv_uom_status        TYPE zcubi_tolerance_status.

    FIELD-SYMBOLS:  <lfs_config> LIKE LINE OF gt_configuration.

    " Select the matid, material group first.
    SELECT SINGLE matid matkl FROM /sapapo/matkey
      INTO (lv_mat_id, lv_material_group)
      WHERE matnr EQ gv_product.

    " Select master data values.
    SELECT SINGLE * FROM /sapapo/marm
      INTO CORRESPONDING FIELDS OF ls_marm
      WHERE matid = lv_mat_id
        AND meinh = gv_uom.

    " Select the tolerance data.
    SELECT SINGLE * INTO ls_tolerance_data
      FROM zcubi_tolerances
      WHERE material_group EQ lv_material_group.

    " Validate each of the values.
    " Set defaults for exception structure.
    ls_exception-warehouse = gv_warehouse_number.
    ls_exception-product = gv_product.
    ls_exception-uom = gv_uom.

    " Check if we should auto-convert the UOM's to match MARM.
    READ TABLE gt_configuration ASSIGNING <lfs_config> WITH KEY config_key = 'PERFORM_AUTO_UOM_CONVERSION'.
    IF <lfs_config> IS ASSIGNED.
      IF <lfs_config>-config_value EQ 'true'.

        " Convert dimensions.
        IF ls_marm-meabm IS NOT INITIAL AND ls_marm-meabm NE gs_gate_measure_data-dimension_uom.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input    = gs_gate_measure_data-length
              unit_in  = gs_gate_measure_data-dimension_uom
              unit_out = ls_marm-meabm
            IMPORTING
              output   = gs_gate_measure_data-length.

          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input    = gs_gate_measure_data-width
              unit_in  = gs_gate_measure_data-dimension_uom
              unit_out = ls_marm-meabm
            IMPORTING
              output   = gs_gate_measure_data-width.

          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input    = gs_gate_measure_data-height
              unit_in  = gs_gate_measure_data-dimension_uom
              unit_out = ls_marm-meabm
            IMPORTING
              output   = gs_gate_measure_data-height.
        ENDIF.

        " Convert weight.
        IF ls_marm-gewei IS NOT INITIAL AND ls_marm-gewei NE gs_gate_measure_data-weight_uom.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input    = gs_gate_measure_data-gross_weight
              unit_in  = gs_gate_measure_data-weight_uom
              unit_out = ls_marm-gewei
            IMPORTING
              output   = gs_gate_measure_data-gross_weight.
        ENDIF.

        " Convert volume.
        IF ls_marm-voleh IS NOT INITIAL AND ls_marm-voleh NE gs_gate_measure_data-volume_uom.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input    = gs_gate_measure_data-volume
              unit_in  = gs_gate_measure_data-volume_uom
              unit_out = ls_marm-voleh
            IMPORTING
              output   = gs_gate_measure_data-volume.
        ENDIF.
      ENDIF.
    ENDIF.

*******************************************************************************
* Validate UOM's.  If the UOM's don't match, we can't hardly validate
* tolerances.
*******************************************************************************
    " Validate Dimension UOM
    CALL METHOD me->validate_uom
      EXPORTING
        iv_cubiscan_uom       = gs_gate_measure_data-dimension_uom
        iv_product_master_uom = ls_marm-meabm
      IMPORTING
        ev_status             = lv_uom_status
        ev_exception_message  = lv_exception_message.
    " Check if there is an exception message.
    IF lv_uom_status EQ gc_status_error.
      " Yes, an exception exists.
      ls_exception-item_exception = 'Dimensions UOM'.
      ls_exception-exception_message = lv_exception_message.
      APPEND ls_exception TO lt_exceptions.

      " Must set all the dimension status' to the UOM status now.
      gs_gate_measure_data-length_status = lv_uom_status.
      gs_gate_measure_data-width_status = lv_uom_status.
      gs_gate_measure_data-height_status = lv_uom_status.
    ENDIF.

    " Validate Weight UOM
    CALL METHOD me->validate_uom
      EXPORTING
        iv_cubiscan_uom       = gs_gate_measure_data-weight_uom
        iv_product_master_uom = ls_marm-gewei
      IMPORTING
        ev_status             = lv_uom_status
        ev_exception_message  = lv_exception_message.
    " Check if there is an exception message.
    IF lv_uom_status EQ gc_status_error.
      " Yes, an exception exists.
      ls_exception-item_exception = 'Weight UOM'.
      ls_exception-exception_message = lv_exception_message.
      APPEND ls_exception TO lt_exceptions.

      " Must set all the weight status' to the UOM status now.
      gs_gate_measure_data-gross_weight_status = lv_uom_status.
      gs_gate_measure_data-dim_weight_status = lv_uom_status.
    ENDIF.

    " Validate Volume UOM
    CALL METHOD me->validate_uom
      EXPORTING
        iv_cubiscan_uom       = gs_gate_measure_data-volume_uom
        iv_product_master_uom = ls_marm-voleh
      IMPORTING
        ev_status             = lv_uom_status
        ev_exception_message  = lv_exception_message.
    " Check if there is an exception message.
    IF lv_uom_status EQ gc_status_error.
      " Yes, an exception exists.
      ls_exception-item_exception = 'Volume UOM'.
      ls_exception-exception_message = lv_exception_message.
      APPEND ls_exception TO lt_exceptions.

      " Must set all the volume status' to the UOM status now.
      gs_gate_measure_data-volume_status = lv_uom_status.
    ENDIF.

*******************************************************************************
* Now validate all the tolerance data, if the UOM's match.
*******************************************************************************
    IF ls_marm-meabm EQ gs_gate_measure_data-dimension_uom.
      " Validate Length
      CALL METHOD me->validate_value
        EXPORTING
          iv_cubiscan_value       = gs_gate_measure_data-length
          iv_product_master_value = ls_marm-laeng
          iv_lower_limit_percent  = ls_tolerance_data-length_lower
          iv_upper_limit_percent  = ls_tolerance_data-length_upper
        IMPORTING
          ev_status               = gs_gate_measure_data-length_status
          ev_exception_message    = lv_exception_message.
      " Check if there is an exception message.
      IF gs_gate_measure_data-length_status EQ gc_status_error.
        " Yes, an exception exists.
        ls_exception-item_exception = 'Length'.
        ls_exception-exception_message = lv_exception_message.
        APPEND ls_exception TO lt_exceptions.
      ENDIF.

      " Validate Width
      CALL METHOD me->validate_value
        EXPORTING
          iv_cubiscan_value       = gs_gate_measure_data-width
          iv_product_master_value = ls_marm-breit
          iv_lower_limit_percent  = ls_tolerance_data-width_lower
          iv_upper_limit_percent  = ls_tolerance_data-width_upper
        IMPORTING
          ev_status               = gs_gate_measure_data-width_status
          ev_exception_message    = lv_exception_message.
      " Check if there is an exception message.
      IF gs_gate_measure_data-width_status EQ gc_status_error.
        " Yes, an exception exists.
        ls_exception-item_exception = 'Width'.
        ls_exception-exception_message = lv_exception_message.
        APPEND ls_exception TO lt_exceptions.
      ENDIF.

      " Validate Height
      CALL METHOD me->validate_value
        EXPORTING
          iv_cubiscan_value       = gs_gate_measure_data-height
          iv_product_master_value = ls_marm-hoehe
          iv_lower_limit_percent  = ls_tolerance_data-height_lower
          iv_upper_limit_percent  = ls_tolerance_data-height_upper
        IMPORTING
          ev_status               = gs_gate_measure_data-height_status
          ev_exception_message    = lv_exception_message.
      " Check if there is an exception message.
      IF gs_gate_measure_data-height_status EQ gc_status_error.
        " Yes, an exception exists.
        ls_exception-item_exception = 'Height'.
        ls_exception-exception_message = lv_exception_message.
        APPEND ls_exception TO lt_exceptions.
      ENDIF.
    ENDIF.

    IF ls_marm-gewei EQ gs_gate_measure_data-weight_uom.
      " Validate Gross Weight
      CALL METHOD me->validate_value
        EXPORTING
          iv_cubiscan_value       = gs_gate_measure_data-gross_weight
          iv_product_master_value = ls_marm-brgew
          iv_lower_limit_percent  = ls_tolerance_data-gweight_lower
          iv_upper_limit_percent  = ls_tolerance_data-gweight_upper
        IMPORTING
          ev_status               = gs_gate_measure_data-gross_weight_status
          ev_exception_message    = lv_exception_message.
      " Check if there is an exception message.
      IF gs_gate_measure_data-gross_weight_status EQ gc_status_error.
        " Yes, an exception exists.
        ls_exception-item_exception = 'Gross Weight'.
        ls_exception-exception_message = lv_exception_message.
        APPEND ls_exception TO lt_exceptions.
      ENDIF.
    ENDIF.

    IF ls_marm-voleh EQ gs_gate_measure_data-volume_uom.
      " Validate Volume
      CALL METHOD me->validate_value
        EXPORTING
          iv_cubiscan_value       = gs_gate_measure_data-volume
          iv_product_master_value = ls_marm-volum
          iv_lower_limit_percent  = ls_tolerance_data-volume_lower
          iv_upper_limit_percent  = ls_tolerance_data-volume_upper
        IMPORTING
          ev_status               = gs_gate_measure_data-volume_status
          ev_exception_message    = lv_exception_message.
      " Check if there is an exception message.
      IF gs_gate_measure_data-volume_status EQ gc_status_error.
        " Yes, an exception exists.
        ls_exception-item_exception = 'Volume'.
        ls_exception-exception_message = lv_exception_message.
        APPEND ls_exception TO lt_exceptions.
      ENDIF.
    ENDIF.

    " If exceptions exist, write them to the table.
    IF lines( lt_exceptions ) GT 0.
      INSERT zcubi_wrklst_exp FROM TABLE lt_exceptions.
    ENDIF.
  ENDMETHOD.


  METHOD validate_uom.
    IF iv_product_master_uom IS INITIAL.
      ev_status = gc_status_error.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '027' INTO ev_exception_message.
    ELSEIF iv_cubiscan_uom NE iv_product_master_uom.
      ev_status = gc_status_error.
      MESSAGE ID 'ZCUBI_MESSAGES' TYPE 'I' NUMBER '026' INTO ev_exception_message.
    ELSE.
      ev_status = me->gc_status_ok.
    ENDIF.
  ENDMETHOD.


  METHOD validate_value.
    DATA: lv_valid TYPE boolean.

    IF iv_lower_limit_percent IS INITIAL OR iv_upper_limit_percent IS INITIAL.
      ev_status = gc_status_ok.
    ELSE.
      " Tolerances are supplied
      IF iv_cubiscan_value IS INITIAL OR iv_product_master_value IS INITIAL.
        " Either no value in Cubiscan field or no value in product master field so default to caution.
        ev_status = gc_status_caution.
      ELSE.
        " Validate that it fits within tolerances.
        CALL METHOD me->is_within_tolerances
          EXPORTING
            iv_cubiscan_value       = iv_cubiscan_value
            iv_product_master_value = iv_product_master_value
            iv_lower_limit_percent  = iv_lower_limit_percent
            iv_upper_limit_percent  = iv_upper_limit_percent
          IMPORTING
            ev_valid                = lv_valid
            ev_exception_message    = ev_exception_message.
        IF lv_valid EQ abap_true.
          ev_status = gc_status_ok.
        ELSE.
          ev_status = gc_status_error.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
