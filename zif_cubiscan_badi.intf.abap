interface ZIF_CUBISCAN_BADI
  public .


  interfaces IF_BADI_INTERFACE .

  methods GET_IMAGE_PATH
    importing
      !IV_PRODUCT type /SAPAPO/MATNR
    exporting
      !EV_IMAGE_PATH type CHAR255 .
  methods ON_AFTER_UPDATE
    importing
      !IS_WORKLIST_ITEM type ZCUBI_WORKLIST_ITEM .
endinterface.
