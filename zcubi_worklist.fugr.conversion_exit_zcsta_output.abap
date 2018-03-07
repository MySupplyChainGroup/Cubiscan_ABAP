FUNCTION CONVERSION_EXIT_ZCSTA_OUTPUT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------
  DATA: lt_values TYPE STANDARD TABLE OF dd07v.

  FIELD-SYMBOLS: <fs_value> TYPE dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZCUBI_STATUS'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_values
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  LOOP AT lt_values ASSIGNING <fs_value>.
    IF <fs_value>-domvalue_l EQ input.
      output = <fs_value>-ddtext.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
