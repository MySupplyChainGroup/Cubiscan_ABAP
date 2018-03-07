interface ZIF_CUBI_WRKLST
  public .


  methods QUERY_ITEMS
    importing
      !IT_WAREHOUSE_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_PRODUCT_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_UOM_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_STATUS_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
    returning
      value(RT_ITEMS) type ZCUBI_WORKLIST_ITEM_TT .
  methods QUERY_HEADERS
    importing
      !IT_WAREHOUSE_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS
      !IT_PRODUCT_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
    returning
      value(RT_HEADERS) type ZCUBI_WORKLIST_HEADER_TT .
  methods QUERY_REVIEW_ITEMS
    importing
      !IT_WAREHOUSE_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_PRODUCT_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_UOM_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !IT_STATUS_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS optional
    returning
      value(RT_ITEMS) type ZCUBI_WORKLIST_REVIEW_ITEM_TT .
  methods QUERY_PRODUCTS
    importing
      !IT_WAREHOUSE_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS
      !IT_PRODUCT_SELECT_OPTIONS type /IWBEP/T_COD_SELECT_OPTIONS
    returning
      value(RT_PRODUCTS) type ZCUBI_PRODUCT_LOOKUP_TT .
  methods CREATE_ITEM
    importing
      !IS_WORKLIST_ITEM type ZCUBI_WORKLIST_ITEM
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods UPDATE_ITEM
    importing
      !IS_WORKLIST_ITEM type ZCUBI_WORKLIST_ITEM
    exporting
      !EV_SUCCESS type BOOLEAN
      !ET_MESSAGES type BAPIRET2_T .
  methods DELETE_ITEM
    importing
      !IS_WORKLIST_ITEM type ZCUBI_WORKLIST_ITEM
    exporting
      !EV_SUCCESS type BOOLEAN
      !ET_MESSAGES type BAPIRET2_T .
  methods QUERY_UOMS
    importing
      !IV_WAREHOUSE_NUMBER type /SCWM/LGNUM
      !IV_PRODUCT_NUMBER type /SAPAPO/MATNR
    exporting
      !ET_UOMS type ZCUBI_PRODUCT_UOM_TT .
  methods QUERY_UOMS_IN_WORKLIST
    importing
      !IV_WAREHOUSE_NUMBER type /SCWM/LGNUM
      !IV_PRODUCT_NUMBER type /SAPAPO/MATNR
    exporting
      !ET_UOMS type ZCUBI_PRODUCT_UOM_TT .
  methods QUERY_UOMS_NOT_IN_WORKLIST
    importing
      !IV_WAREHOUSE_NUMBER type /SCWM/LGNUM
      !IV_PRODUCT_NUMBER type /SAPAPO/MATNR
    exporting
      !ET_UOMS type ZCUBI_PRODUCT_UOM_TT .
endinterface.
