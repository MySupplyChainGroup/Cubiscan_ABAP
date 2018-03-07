class ZCL_CUBI_WRKLIST_FACTORY definition
  public
  final
  create public .

public section.

  class-methods CREATE
    returning
      value(RO_WORKLIST) type ref to ZIF_CUBI_WRKLST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUBI_WRKLIST_FACTORY IMPLEMENTATION.


  METHOD create.
    DATA: lt_configuration  TYPE zmscg_configuration_tt.

    FIELD-SYMBOLS:  <lfs_configuration> LIKE LINE OF lt_configuration.

    " Read configuration and instantiage the proper worklist implementation.
    CALL METHOD zcl_mscg_common_utils=>get_configuration
      EXPORTING
        iv_config_application = 'CUBISCAN'
        iv_config_type        = 1 "Backend Configuration
      RECEIVING
        rt_configuration      = lt_configuration.
    READ TABLE lt_configuration ASSIGNING <lfs_configuration> WITH KEY config_key = 'WORKLIST_IMPL_CLASS'.

    CREATE OBJECT ro_worklist TYPE (<lfs_configuration>-config_value).
  ENDMETHOD.
ENDCLASS.
