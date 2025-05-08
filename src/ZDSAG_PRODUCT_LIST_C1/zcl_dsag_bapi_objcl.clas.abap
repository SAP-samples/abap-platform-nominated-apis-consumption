CLASS zcl_dsag_bapi_objcl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES t_bapiret2 type standard table of bapiret2.
    CLASS-METHODS create
      IMPORTING classnumnew       TYPE klasse_d
                classtypenew      TYPE klassenart
                keydate           TYPE bapi_keydate DEFAULT sy-datum
                objectkeynew      TYPE objnum OPTIONAL
                objectkeynew_long TYPE cuobn90 OPTIONAL
                objecttablenew    TYPE tabelle OPTIONAL
      EXPORTING classif_status    TYPE clstatus
      CHANGING  return            TYPE t_bapiret2.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dsag_bapi_objcl IMPLEMENTATION.
  METHOD create.

    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        classnumnew       = classnumnew
        classtypenew      = classtypenew
        keydate           = keydate
        objectkeynew      = objectkeynew
        objectkeynew_long = objectkeynew_long
        objecttablenew    = objecttablenew
      IMPORTING
        classif_status    = classif_status
      TABLES
        return            = return.

  ENDMETHOD.

ENDCLASS.
