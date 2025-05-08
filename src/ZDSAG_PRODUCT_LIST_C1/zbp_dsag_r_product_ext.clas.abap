class ZBP_DSAG_R_PRODUCT_EXT  DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zdsag_r_product.

public section.
    CLASS-METHODS bapi_objcl_create
      IMPORTING classnumnew       TYPE klasse_d
                classtypenew      TYPE klassenart
                keydate           TYPE bapi_keydate DEFAULT sy-datum
                objectkeynew      TYPE objnum OPTIONAL
                objectkeynew_long TYPE cuobn90 OPTIONAL
                objecttablenew    TYPE tabelle OPTIONAL
      EXPORTING classif_status    TYPE clstatus
      CHANGING  return            TYPE zcl_dsag_bapi_objcl=>t_bapiret2.
protected section.
private section.
ENDCLASS.



CLASS ZBP_DSAG_R_PRODUCT_EXT IMPLEMENTATION.
  METHOD bapi_objcl_create.
    AUTHORITY-CHECK DISABLE BEGIN CONTEXT zdsag_r_product~nocheckwhensaving.
      zcl_dsag_bapi_objcl=>create( EXPORTING  classnumnew    = classnumnew
                                              classtypenew   = classtypenew
                                              keydate        = keydate
                                              objectkeynew   = objectkeynew
                                              objecttablenew = objecttablenew
                                    CHANGING  return         = return ).

    AUTHORITY-CHECK DISABLE END.
  ENDMETHOD.
ENDCLASS.
