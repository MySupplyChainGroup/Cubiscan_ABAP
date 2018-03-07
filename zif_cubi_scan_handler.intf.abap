interface ZIF_CUBI_SCAN_HANDLER
  public .


  methods CONNECT
    raising
      ZCX_CUBISCAN_EXCEPTION .
  methods DISCONNECT
    raising
      ZCX_CUBISCAN_EXCEPTION .
  methods START_POLLING
    raising
      ZCX_CUBISCAN_EXCEPTION .
  methods GET_GATE_MEASURE_DATA
    returning
      value(RV_GATE_MEASURE_DATA) type ZCUBI_GATE_MEASURE_DATA .
endinterface.
