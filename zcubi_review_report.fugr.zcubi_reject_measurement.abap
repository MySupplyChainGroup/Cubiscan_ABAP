FUNCTION zcubi_reject_measurement.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  DATA: lo_worklist        TYPE REF TO zif_cubi_wrklst,
        ls_worklist_item   TYPE zcubi_worklist_item,
        lv_success         TYPE boolean,
        lt_messages        TYPE bapiret2_t,
        lt_all_messages    TYPE bapiret2_t,
        lo_log             TYPE REF TO /scwm/cl_log,
        ls_display_profile TYPE bal_s_prof,
        ls_log             TYPE bal_s_log,
        lv_loghandle       TYPE balloghndl.

  FIELD-SYMBOLS:  <lfs_item> TYPE zcubi_worklist_review_item.

  " If no records were selected, just exit.
  IF lines( it_data ) EQ 0.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log.

  CALL METHOD zcl_cubi_wrklist_factory=>create
    RECEIVING
      ro_worklist = lo_worklist.

  LOOP AT it_data ASSIGNING <lfs_item>.
    CLEAR ls_worklist_item.
    MOVE-CORRESPONDING <lfs_item> TO ls_worklist_item.

    " Update status.
    ls_worklist_item-status = 'R'.

    CALL METHOD lo_worklist->update_item
      EXPORTING
        is_worklist_item = ls_worklist_item
      IMPORTING
        ev_success       = lv_success
        et_messages      = lt_messages.

    APPEND LINES OF lt_messages TO lt_all_messages.
  ENDLOOP.

  IF NOT lt_all_messages[] IS INITIAL.
    " Handle messages that occurred during function processing
    lo_log->add_log( it_prot = lt_all_messages ).
    ls_log-extnumber = 1.
    ls_log-object = wmegc_apl_object_wme.
    ls_log-subobject = wmegc_apl_subob_gen.

    lo_log->create_log(
      EXPORTING
        is_log = ls_log
        IMPORTING
          ev_loghandle = lv_loghandle ).
    lo_log->convert_bapiret2applog( ).

    " Get profile for popup application log
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile.
    ls_display_profile-use_grid = 'X'.
    TRY.
        lo_log->display_log(
          EXPORTING
            iv_loghandle = lv_loghandle
            is_display_profile = ls_display_profile ).
      CATCH /scwm/cx_basics.                            "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
ENDFUNCTION.
