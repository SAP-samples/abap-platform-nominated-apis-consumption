CLASS zdsag_bapi_class_create DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES _bapi1003_charact           TYPE STANDARD TABLE OF bapi1003_charact WITH DEFAULT KEY ##TYPSHADOW.
    TYPES _bapi1003_catch             TYPE STANDARD TABLE OF bapi1003_catch WITH DEFAULT KEY ##TYPSHADOW.
    TYPES _bapiret2                   TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY ##TYPSHADOW.
    TYPES _t_zdsag_r_classdescription TYPE STANDARD TABLE OF zdsag_r_classdescription.
    TYPES _t_zdsag_r_classcharacters  TYPE STANDARD TABLE OF zdsag_r_classcharacters.

    CLASS-METHODS map_4_bapi_class_create
      IMPORTING iv_classification  TYPE zdsag_r_Classification
                it_descriptions    TYPE _t_zdsag_r_classdescription
                it_characteristics TYPE _t_zdsag_r_classcharacters
                test_run           TYPE abap_bool OPTIONAL
      EXPORTING ev_classinternalid TYPE clint
      CHANGING  !return            TYPE _bapiret2.

  PRIVATE SECTION.
    CLASS-METHODS bapi_class_create
      IMPORTING classbasicdata       TYPE bapi1003_basic
                classnumnew          TYPE klasse_d
                classtypenew         TYPE klassenart
                testrun              TYPE testrun           DEFAULT abap_false
      CHANGING  classcharacteristics TYPE _bapi1003_charact OPTIONAL
                classdescriptions    TYPE _bapi1003_catch
                !return              TYPE _bapiret2.

ENDCLASS.


CLASS zdsag_bapi_class_create IMPLEMENTATION.
  METHOD bapi_class_create.
    CALL FUNCTION 'BAPI_CLASS_CREATE'
      EXPORTING
        classbasicdata        = classbasicdata
        classnumnew           = classnumnew
        classtypenew          = classtypenew
        testrun               = testrun
      TABLES
        classcharacteristics  = classcharacteristics
        classdescriptions     = classdescriptions
        return                = return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF testrun = abap_true.
      " Required to clean the buffer manually
      CALL FUNCTION 'BUFFER_REFRESH_ALL'.  " Tier 3 required else will get warning message
    ENDIF.
  ENDMETHOD.

  METHOD map_4_bapi_class_create.
    " 1. Handle conversion for BAPIs
    " 2. Mapping of CDS entity properties to BAPI structures
    " 3. Special handling for retrieving "Classification ID" created using BAPI
    TYPES: BEGIN OF t_data,
             sign   TYPE char1,
             option TYPE char2,
             low    TYPE char18,
             high   TYPE char18,
           END OF t_data.
    DATA ls_basic_data      TYPE bapi1003_basic.

    DATA lt_descriptions    TYPE _bapi1003_catch.
    DATA ls_description           TYPE bapi1003_catch.

    DATA ls_characteristic  TYPE bapi1003_charact.
    DATA lt_characteristics TYPE _bapi1003_charact.

    DATA lt_klah            TYPE STANDARD TABLE OF klah.

    DATA ls_range TYPE t_data.
    DATA lt_range TYPE STANDARD TABLE OF t_data.

    " Basic Data from Root entity 'ZDSAG_R_CLASSFICATION'---------------------
    ls_basic_data-valid_from = iv_classification-ValidityStartDate.
    ls_basic_data-valid_to   = iv_classification-ValidityEndDate.
    ls_basic_data-status     = iv_classification-ClassStatus.
    "
    " Description  from entity 'ZDSAG_R_CLASSDESCRIPTION'---------------------
    LOOP AT it_descriptions ASSIGNING FIELD-SYMBOL(<fs_description>).

      ls_description-langu = <fs_description>-LanguageEdit.
      CALL FUNCTION 'LANGUAGE_CODE_SAP_TO_ISO'
        EXPORTING
          sap_code = ls_description-langu
        IMPORTING
          iso_code = ls_description-langu_iso.

      ls_description-catchword = <fs_description>-ClassDescription.

      APPEND ls_description TO lt_descriptions.
      CLEAR ls_description.
    ENDLOOP.


    " Characteristics  from entity 'ZDSAG_R_CLASSCHARACTERS'---------------------
    SELECT CharcInternalID,
           Characteristic
       FROM I_ClfnCharcBasic WITH PRIVILEGED ACCESS
      FOR ALL ENTRIES IN @it_characteristics
      WHERE CharcInternalID = @it_characteristics-CharcInternalID
      INTO TABLE @DATA(lt_cabn_details).
*    SELECT *
*       FROM zdsag_r_cabn
*      FOR ALL ENTRIES IN @it_characteristics
*      WHERE CharcInternalID = @it_characteristics-CharcInternalID
*      INTO TABLE @DATA(lt_cabn_details).

    LOOP AT it_characteristics ASSIGNING FIELD-SYMBOL(<fs_characteristic>).
      READ TABLE lt_cabn_details INTO DATA(ls_cabn_detail) WITH KEY CharcInternalID = <fs_characteristic>-CharcInternalID.
      ls_characteristic-name_char = ls_cabn_detail-Characteristic.
      APPEND ls_characteristic TO lt_characteristics.
      CLEAR ls_characteristic.
    ENDLOOP.

    bapi_class_create( EXPORTING classtypenew         = iv_classification-ClassType
                                 classnumnew          = iv_classification-Class
                                 classbasicdata       = ls_basic_data
                                 testrun              = test_run
                       CHANGING  classdescriptions    = lt_descriptions
                                 classcharacteristics = lt_characteristics
                                 return               = return ).

    IF test_run = abap_false.
      " BAPI does not explicitly return the "Classification ID -CLINT", so to access that information from buffer
      ls_range-sign   = 'I'.
      ls_range-option = 'EQ'.
      ls_range-low    = iv_classification-Class.
      APPEND ls_range TO lt_range.
      " Nominated API to retrieve classInternal ID from Buffer
      CALL FUNCTION 'CLSE_SELECT_KLAH_VIA_NAMERANGE'
        TABLES
          imp_exp_klah   = lt_klah
          namerange      = lt_range
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.

      IF sy-subrc = 0.
        DATA(ls_class) = VALUE #( lt_klah[ klart = iv_classification-ClassType
                                           class = iv_classification-Class ] OPTIONAL ).
        IF ls_class IS NOT INITIAL.
          ev_classinternalid = ls_class-clint.
        ENDIF.
      ELSE.
        ASSERT 1 = 2.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
