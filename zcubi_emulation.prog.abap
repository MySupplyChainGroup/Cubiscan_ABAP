*&---------------------------------------------------------------------*
*& Report ZCUBI_EMULATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcubi_emulation.

SELECTION-SCREEN COMMENT /1(80) TEXT-001.
SELECTION-SCREEN COMMENT /1(80) TEXT-002.
SELECTION-SCREEN COMMENT /1(80) TEXT-003.
SELECTION-SCREEN ULINE.

SELECTION-SCREEN SKIP 1.
PARAMETERS: message TYPE string DEFAULT '#mAC      ,L12.05,W 5.05,H 3.60in,K  1.50,D  1.32lb,F0166,D###'.

WRITE: / 'This program will write the dimension string into an inbound ABAP message channel and then will be picked up by the waiting Cubiscan scan handler and processed'.

DATA: lo_producer_text TYPE REF TO if_amc_message_producer_text.
DATA: lx_amc_error       TYPE REF TO cx_amc_error.

TRY.
    lo_producer_text ?= cl_amc_channel_manager=>create_message_producer( i_application_id = 'ZCUBI_MESSAGE_CHANNEL' i_channel_id = '/inbound' ).

    " Send message to the amc channel
    lo_producer_text->send( i_message = message ).
  CATCH cx_amc_error INTO lx_amc_error.
    MESSAGE lx_amc_error->get_text( ) TYPE 'E'.
ENDTRY.
