CLASS zdsag_get_class_detail DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES _bapi1003_catch     TYPE STANDARD TABLE OF bapi1003_catch_new WITH DEFAULT KEY.
    TYPES _bapi1003_charact_r TYPE STANDARD TABLE OF bapi1003_charact_new WITH DEFAULT KEY.

    CLASS-METHODS get_classification_details
      IMPORTING classnum             TYPE klasse_d
                classtype            TYPE klassenart
      EXPORTING classbasicdata       TYPE bapi1003_basic
      CHANGING  classdescriptions    TYPE _bapi1003_catch     OPTIONAL
                classcharacteristics TYPE _bapi1003_charact_r OPTIONAL.

ENDCLASS.


CLASS zdsag_get_class_detail IMPLEMENTATION.
  METHOD get_classification_details.
    DATA characteristics TYPE STANDARD TABLE OF bapi_char.
    DATA char_values TYPE STANDARD TABLE OF bapi_char_values .

    " Basic Data
    SELECT SINGLE classInternalID,
                  validityStartDate,
                  validityEndDate,
                  classStatus
    FROM i_clfnclass
    INTO (@DATA(lv_classinternalid),
          @classbasicdata-valid_from,
          @classbasicdata-valid_to,
          @classbasicdata-status)
    WHERE ClassType = @classtype
    AND class = @classnum.

    " Description
    SELECT Language,
           ClassDescription
    FROM I_ClfnClassDescription
    INTO TABLE @DATA(lt_classification_description)
    WHERE ClassInternalID = @lv_classinternalid.

    LOOP AT lt_classification_description ASSIGNING FIELD-SYMBOL(<ls_cds_classification_des>).
      APPEND INITIAL LINE TO classdescriptions ASSIGNING FIELD-SYMBOL(<ls_bapi_classification_des>).
      <ls_bapi_classification_des>-langu = <ls_cds_classification_des>-Language.
      CALL FUNCTION 'LANGUAGE_CODE_SAP_TO_ISO'
        EXPORTING
          sap_code = <ls_bapi_classification_des>-langu
        IMPORTING
          iso_code = <ls_bapi_classification_des>-langu_iso.

      <ls_bapi_classification_des>-catchword = <ls_cds_classification_des>-ClassDescription.
    ENDLOOP.

    " Characteristics- I_ClfnClassCharacteristicDEX even though C1 released gives only 803- again have to map via ZDSAG_R_CABN
    CALL FUNCTION 'BAPI_CLASS_GET_CHARACTERISTICS'
      EXPORTING
        classnum        = classnum
        classtype       = classtype
        with_values     = abap_false
      TABLES
        characteristics = characteristics
        char_values     = char_values.
    IF sy-subrc = 0.

    ENDIF.
    LOOP AT characteristics ASSIGNING FIELD-SYMBOL(<ls_characteristic>).
      APPEND INITIAL LINE TO classcharacteristics ASSIGNING FIELD-SYMBOL(<ls_bapi_classification_char>).
      <ls_bapi_classification_char>-name_char = <ls_characteristic>-name_char.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
