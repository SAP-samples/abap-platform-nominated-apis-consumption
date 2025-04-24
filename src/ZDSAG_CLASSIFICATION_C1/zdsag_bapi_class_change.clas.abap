CLASS zdsag_bapi_class_change DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES _bapi1003_catch_new         TYPE STANDARD TABLE OF bapi1003_catch WITH DEFAULT KEY .
    TYPES _bapi1003_catch             TYPE STANDARD TABLE OF bapi1003_catch WITH DEFAULT KEY .
    TYPES _bapi1003_charact           TYPE STANDARD TABLE OF bapi1003_charact WITH DEFAULT KEY.
    TYPES _bapi1003_charact_new       TYPE STANDARD TABLE OF bapi1003_charact_new WITH DEFAULT KEY.
    TYPES _bapiret2                   TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .
    TYPES _t_zdsag_r_classdescription TYPE STANDARD TABLE OF zdsag_r_classdescription.
    TYPES _t_zdsag_r_classcharacters  TYPE STANDARD TABLE OF zdsag_r_classcharacters.

    CLASS-METHODS map_4_bapi_class_change
      IMPORTING iv_classification  TYPE zdsag_r_Classification
                it_descriptions    TYPE _t_zdsag_r_classdescription
                it_characteristics TYPE _t_zdsag_r_classcharacters
                test_run           TYPE abap_bool OPTIONAL

      CHANGING  !return            TYPE _bapiret2.

  PRIVATE SECTION.
    CLASS-METHODS bapi_class_change
      IMPORTING classbasicdata          TYPE bapi1003_basic        OPTIONAL
                classbasicdatanew       TYPE bapi1003_basic        OPTIONAL
                classnum                TYPE klasse_d              OPTIONAL
                classtype               TYPE klassenart            OPTIONAL
                keydate                 TYPE bapi_keydate          DEFAULT sy-datum
                testrun                 TYPE testrun               DEFAULT abap_false
      CHANGING  classdescriptions       TYPE _bapi1003_catch       OPTIONAL
                classdescriptionsnew    TYPE _bapi1003_catch_new   OPTIONAL
                classcharacteristics    TYPE _bapi1003_charact     OPTIONAL
                classcharacteristicsnew TYPE _bapi1003_charact OPTIONAL
                !return                 TYPE _bapiret2.

ENDCLASS.


CLASS zdsag_bapi_class_change IMPLEMENTATION.
  METHOD bapi_class_change.
    CALL FUNCTION 'BAPI_CLASS_CHANGE'
      EXPORTING
        classbasicdata          = classbasicdata
        classbasicdatanew       = classbasicdatanew
        classnum                = classnum
        classtype               = classtype
        keydate                 = keydate
        testrun                 = testrun
      TABLES
        classdescriptions       = classdescriptions
        classdescriptionsnew    = classdescriptionsnew
        classcharacteristics    = classcharacteristics
        classcharacteristicsnew = classcharacteristicsnew
        return                  = return
      EXCEPTIONS
        communication_failure   = 1
        system_failure          = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.

  METHOD map_4_bapi_class_change.
    "We also need to pass both current data and new data for CHANGE operation

    " For Classifications
    DATA ls_basic_data_current      TYPE bapi1003_basic.
    DATA ls_basic_data_new          TYPE bapi1003_basic.

    " For descriptions
    DATA lt_descriptions_current    TYPE STANDARD TABLE OF bapi1003_catch_new WITH DEFAULT KEY.
    DATA lt_descriptions_new        TYPE STANDARD TABLE OF bapi1003_catch_new WITH DEFAULT KEY.
    FIELD-SYMBOLS <fs_description_new> TYPE bapi1003_catch.

    " For characteristics
    DATA lt_characteristics_current TYPE STANDARD TABLE OF bapi1003_charact_new WITH DEFAULT KEY.
    DATA lt_characteristics_new     TYPE STANDARD TABLE OF bapi1003_charact_new WITH DEFAULT KEY.

    TRY.
        zdsag_get_class_detail=>get_classification_details(
          EXPORTING classtype            = iv_classification-ClassType
                    classnum             = iv_classification-Class
          IMPORTING classbasicdata       = ls_basic_data_current
          CHANGING  classdescriptions    = lt_descriptions_current
                    classcharacteristics = lt_characteristics_current  ).
      CATCH cx_root .
        ASSERT 1 = 2.
        "Or handle exception
    ENDTRY.

    " Data  from entity 'ZDSAG_R_CLASSIFICATION'---------------------

    " We need existing data , along with Modified data, so move as a first step
    ls_basic_data_new = ls_basic_data_current.
    IF iv_classification-ValidityStartDate IS NOT INITIAL.
      ls_basic_data_new-valid_from = iv_classification-ValidityStartDate.
    ENDIF.
    IF iv_classification-ValidityEndDate IS NOT INITIAL.
      ls_basic_data_new-valid_to = iv_classification-ValidityEndDate.
    ENDIF.
    IF iv_classification-ClassStatus IS NOT INITIAL.
      ls_basic_data_new-status = iv_classification-ClassStatus.
    ENDIF.

    " Data from entity 'ZDSAG_R_CLASSDESCRIPTION'---------------------
    LOOP AT it_descriptions ASSIGNING FIELD-SYMBOL(<fs_description_from_ui>).

      ASSIGN lt_descriptions_new[ langu = <fs_description_from_ui>-LanguageEdit ] TO <fs_description_new>.
      IF sy-subrc = 0.
        <fs_description_new>-catchword = <fs_description_from_ui>-ClassDescription.
      ELSE.
        APPEND INITIAL LINE TO lt_descriptions_new ASSIGNING <fs_description_new>.
        CALL FUNCTION 'LANGUAGE_CODE_SAP_TO_ISO'
          EXPORTING
            sap_code = <fs_description_from_ui>-LanguageEdit
          IMPORTING
            iso_code = <fs_description_new>-langu_iso.

        <fs_description_new>-langu = <fs_description_from_ui>-LanguageEdit.
        <fs_description_new>-catchword = <fs_description_from_ui>-ClassDescription.

      ENDIF.

    ENDLOOP.

    " Data from entity 'ZDSAG_R_CLASSCHARACTERISTICS'---------------------
    SELECT CharcInternalID,
           Characteristic
       FROM I_ClfnCharcBasic WITH PRIVILEGED ACCESS
      FOR ALL ENTRIES IN @it_characteristics
      WHERE CharcInternalID = @it_characteristics-CharcInternalID
      INTO TABLE @DATA(lt_cabn_details).
    " Characteristics  from entity 'ZDSAG_R_CLASSCHARACTERS'---------------------

    LOOP AT it_characteristics ASSIGNING FIELD-SYMBOL(<fs_characteristic_from_ui>).

      ASSIGN lt_cabn_details[ CharcInternalID = <fs_characteristic_from_ui>-CharcInternalID ] TO FIELD-SYMBOL(<ls_cabn_detail>).
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_characteristics_new ASSIGNING FIELD-SYMBOL(<fs_characteristic_new>).
        <fs_characteristic_new>-name_char = <ls_cabn_detail>-Characteristic.
      ENDIF.
    ENDLOOP.

    bapi_class_change( EXPORTING classtype               = iv_classification-ClassType
                                 classnum                = iv_classification-Class
                                 classbasicdata          = ls_basic_data_current
                                 classbasicdatanew       = ls_basic_data_new
                                 testrun                 = test_run

                       CHANGING  classdescriptions       = lt_descriptions_current
                                 classdescriptionsnew    = lt_descriptions_new
                                 classcharacteristics    = lt_characteristics_current
                                 classcharacteristicsnew = lt_characteristics_new
                                 return                  = return ).
  ENDMETHOD.
ENDCLASS.
