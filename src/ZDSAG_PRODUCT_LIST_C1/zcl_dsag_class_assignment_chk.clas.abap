CLASS zcl_dsag_class_assignment_chk DEFINITION
PUBLIC
FINAL
CREATE PRIVATE .
  PUBLIC SECTION.
    TYPES:
    t_bapiret2 type standard table of bapiret2.
    CLASS-METHODS does_obj_classification_exist
        IMPORTING classnum          TYPE klasse_d
                  classtype         TYPE klassenart
                  keydate           TYPE bapi_keydate DEFAULT sy-datum
                  objectkey         TYPE objnum       OPTIONAL
                  objectkey_long    TYPE cuobn90      OPTIONAL
                  objecttable       TYPE tabelle
        EXPORTING assignment_exists TYPE abap_bool
        CHANGING  return            TYPE t_bapiret2 OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    .
    .
ENDCLASS.

CLASS zcl_dsag_class_assignment_chk IMPLEMENTATION.
  METHOD does_obj_classification_exist.
    CALL FUNCTION 'BAPI_OBJCL_EXISTENCECHECK'
      EXPORTING
        classnum       = classnum
        classtype      = classtype
        keydate        = keydate
        objectkey      = objectkey
        objectkey_long = objectkey_long
        objecttable    = objecttable
      TABLES
        return         = return.

    ASSIGN return[ type   = 'S'
                   id     = 'CL'
                   number = 731 ] TO FIELD-SYMBOL(<fs_return>).
    IF sy-subrc = 0.
      <fs_return>-type = 'E'.
      assignment_exists = abap_true.
    ELSE.
      assignment_exists = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
