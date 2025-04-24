CLASS zdsag_bapi_objcl_create DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES _bapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.

    CLASS-METHODS map_4_bapi_objcl_create
      IMPORTING classnumnew       TYPE klasse_d
                classtypenew      TYPE klassenart
                keydate           TYPE bapi_keydate DEFAULT sy-datum
                objectkeynew      TYPE objnum       OPTIONAL
                objectkeynew_long TYPE cuobn90      OPTIONAL
                objecttablenew    TYPE tabelle      OPTIONAL
      CHANGING  !return           TYPE _bapiret2.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS bapi_objcl_create
      IMPORTING classnumnew       TYPE klasse_d
                classtypenew      TYPE klassenart
                keydate           TYPE bapi_keydate DEFAULT sy-datum
                objectkeynew      TYPE objnum       OPTIONAL
                objectkeynew_long TYPE cuobn90      OPTIONAL
                objecttablenew    TYPE tabelle
      CHANGING  !return           TYPE _bapiret2.

ENDCLASS.

CLASS zdsag_bapi_objcl_create IMPLEMENTATION.
  METHOD bapi_objcl_create.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        classnumnew           = classnumnew
        classtypenew          = classtypenew
        keydate               = keydate
        objectkeynew          = objectkeynew
        objectkeynew_long     = objectkeynew_long
        objecttablenew        = objecttablenew
      TABLES
        return                = return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.

  METHOD map_4_bapi_objcl_create.
    DATA: lv_object      TYPE objnum.
"VD unable to activate ABAP ATC check
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = objectkeynew
      IMPORTING
        output       = lv_object
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    bapi_objcl_create( EXPORTING
                             classnumnew = classnumnew
                             classtypenew = classtypenew
                             keydate = sy-datum
                             objectkeynew = lv_object
                             objectkeynew_long = CONV #( lv_object )
                             objecttablenew = 'MARA'
                           CHANGING
                              return = return
                           ).

  ENDMETHOD.

ENDCLASS.
