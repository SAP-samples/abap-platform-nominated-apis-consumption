CLASS zbp_dsag_r_classification_exte DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zdsag_r_classification.
  PUBLIC SECTION.
    TYPES _bapiret2                   TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.
    TYPES _t_zdsag_r_classdescription TYPE STANDARD TABLE OF zdsag_r_classdescription.
    TYPES _t_zdsag_r_classcharacters  TYPE STANDARD TABLE OF zdsag_r_classcharacters.

    CLASS-METHODS bapi_class_create
      IMPORTING iv_classification  TYPE zdsag_r_classification
                it_descriptions    TYPE _t_zdsag_r_classdescription
                it_characteristics TYPE _t_zdsag_r_classcharacters
                test_run           TYPE abap_bool OPTIONAL
      EXPORTING ev_classinternalid TYPE clint
      CHANGING  !return            TYPE _bapiret2.

    CLASS-METHODS bapi_class_change
      IMPORTING iv_classification  TYPE zdsag_r_Classification
                it_descriptions    TYPE _t_zdsag_r_classdescription
                it_characteristics TYPE _t_zdsag_r_classcharacters
                test_run           TYPE abap_bool OPTIONAL

      CHANGING  !return            TYPE _bapiret2.

    CLASS-METHODS bapi_objcl_create
      IMPORTING classnumnew  TYPE klasse_d
                classtypenew TYPE klassenart
                objectkeynew TYPE objnum OPTIONAL
      CHANGING  !return      TYPE _bapiret2.

ENDCLASS.


CLASS zbp_dsag_r_classification_exte IMPLEMENTATION.
  METHOD bapi_class_create.
    AUTHORITY-CHECK DISABLE BEGIN CONTEXT zdsag_r_classification~NoCheckWhenSaving.

      zdsag_bapi_class_create=>map_4_bapi_class_create( EXPORTING iv_classification  = iv_classification
                                                                  it_descriptions    = it_descriptions
                                                                  it_characteristics = it_characteristics
                                                                  test_run           = test_run
                                                        IMPORTING ev_classinternalid = ev_classinternalid
                                                        CHANGING  return             = return ).
    AUTHORITY-CHECK DISABLE END.
  ENDMETHOD.

  METHOD bapi_class_change.
    AUTHORITY-CHECK DISABLE BEGIN CONTEXT zdsag_r_classification~nocheckwhensaving.

      zdsag_bapi_class_change=>map_4_bapi_class_change( EXPORTING iv_classification  = iv_classification
                                                                  it_descriptions    = it_descriptions
                                                                  it_characteristics = it_characteristics
                                                                  test_run           = test_run

                                                        CHANGING  return             = return ).
    AUTHORITY-CHECK DISABLE END.
  ENDMETHOD.

  METHOD bapi_objcl_create.
    AUTHORITY-CHECK DISABLE BEGIN CONTEXT zdsag_r_classification~nocheckwhensaving.
      zdsag_bapi_objcl_create=>map_4_bapi_objcl_create( EXPORTING classnumnew  = classnumnew
                                                                  classtypenew = classtypenew
                                                                  objectkeynew = objectkeynew
                                                        CHANGING  return       = return ).

    AUTHORITY-CHECK DISABLE END.
  ENDMETHOD.
ENDCLASS.
