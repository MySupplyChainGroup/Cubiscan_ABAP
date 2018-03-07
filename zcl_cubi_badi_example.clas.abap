class ZCL_CUBI_BADI_EXAMPLE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZIF_CUBISCAN_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUBI_BADI_EXAMPLE IMPLEMENTATION.


  METHOD zif_cubiscan_badi~get_image_path.
    ev_image_path = 'https://www.certifyme.net/wp-content/themes/certifyme2014/images/forklift-osha-certified.jpg'.
  ENDMETHOD.


  METHOD zif_cubiscan_badi~on_after_update.
  ENDMETHOD.
ENDCLASS.
