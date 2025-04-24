CLASS lhc_Classification DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES ty_header_tky TYPE STRUCTURE FOR PERMISSIONS KEY zdsag_r_Classification\\Classification.
    TYPES ty_bapiret2   TYPE STANDARD TABLE OF bapiret2.
    TYPES ty_reported   TYPE RESPONSE FOR REPORTED LATE zdsag_r_classification.
    TYPES ty_failed     TYPE RESPONSE FOR FAILED LATE zdsag_r_Classification.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Classification RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Classification RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Classification.

    METHODS assign_class_to_material FOR MODIFY
      IMPORTING keys FOR ACTION Classification~assign_class_to_material RESULT result.

    METHODS setDefaultValues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Classification~setDefaultValues.

    METHODS check_if_classtype_exists FOR VALIDATE ON SAVE
      IMPORTING keys FOR Classification~check_if_classtype_exists.

    METHODS simulate_create FOR VALIDATE ON SAVE
      IMPORTING keys FOR Classification~simulate_create.

    METHODS handle_bapi_return
      IMPORTING header_tky TYPE ty_header_tky
                !return    TYPE ty_bapiret2
      EXPORTING !reported  TYPE ty_reported
                !failed    TYPE ty_failed.

ENDCLASS.


CLASS lhc_Classification IMPLEMENTATION.
  METHOD get_instance_features.
    READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
         ENTITY Classification
         FIELDS ( ClassType ) WITH CORRESPONDING #(  keys )

         RESULT DATA(Classifications)
         FAILED failed
         REPORTED reported.

    ASSIGN classifications[ 1 ] TO FIELD-SYMBOL(<fs_classification>).

    result = VALUE #(
        FOR key IN keys
        ( %tky                             = key-%tky
          %action-assign_class_to_material = COND #( WHEN key-ClassInternalID IS INITIAL OR <fs_classification>-ClassType <> '001'
                                                     THEN if_abap_behv=>fc-o-disabled
                                                     ELSE if_abap_behv=>fc-o-enabled ) ) ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD lock.
    READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
         ENTITY Classification
         FIELDS ( ClassType Class ) WITH CORRESPONDING #(  keys )
         RESULT DATA(Classifications)
         FAILED failed
         REPORTED reported.

    LOOP AT Classifications ASSIGNING FIELD-SYMBOL(<fs_classification>).
      zdsag_classification_utility=>enqueue_classification( EXPORTING classtype  = <fs_classification>-ClassType
                                                                      class      = <fs_classification>-class
                                                            IMPORTING lock_error = DATA(lock_error) ).

      IF lock_error = abap_true.
        APPEND VALUE #( %tky = <fs_classification>-%tky  ) TO failed-classification ASSIGNING FIELD-SYMBOL(<failed>).
        <failed>-%fail-cause = if_abap_behv=>cause-locked.

        APPEND VALUE #( %tky = <fs_classification>-%tky  ) TO reported-classification ASSIGNING FIELD-SYMBOL(<reported>).
        <reported>-%msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                 text     = |{ 'Instance Locked By User :' } { sy-msgv1 }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  " ----------------------------------METHOD assign_class_to_material----------------------------
  "   1. The assumption is we assign existing "Material/MM03" to existing "Classification/CL03".
  "   2. So both Classification and Material must exists.
  "   3. from Tier 1 object : We are Accessing Classic API: BAPI_OBJCL_CREATE wrapper
  "       C1 released.
  "   4. Lock Product & Classification(Taken care by Lock Unmanaged )
  "   5. Messages from BAPI are passed to UI layer
  " ---------------------------------------------------------------------------------------------

  METHOD assign_class_to_material.
    READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
         ENTITY Classification
         FIELDS ( Class ClassInternalID ClassType )
         WITH VALUE #( FOR key IN keys
                                              ( %tky             = key-%tky
                                                ClassInternalID  = key-classinternalid  )
                                          )
         RESULT DATA(lt_classifications).
    LOOP AT lt_classifications ASSIGNING FIELD-SYMBOL(<ls_classification>).
      ASSIGN keys[ KEY entity
                   ClassInternalID = <ls_classification>-ClassInternalID ] TO FIELD-SYMBOL(<ls_key>).

      " Try to lock Product or Products before taking for assignment of classification to Product/Products(1:N)
      zdsag_product_utility=>enqueue_product( EXPORTING i_matnr    = <ls_key>-%param-object
                                              IMPORTING lock_error = DATA(lock_error) ).

      IF lock_error = abap_true.
        APPEND VALUE #( %tky = <ls_classification>-%tky  ) TO failed-classification.

        APPEND VALUE #( %tky = <ls_classification>-%tky  ) TO reported-classification ASSIGNING FIELD-SYMBOL(<reported>).
        <reported>-%msg = new_message( id       = zbp_dsag_r_classification=>lc_msg_class
                                       number   = 002
                                       severity = if_abap_behv_message=>severity-error
                                       v1       = <ls_key>-%param-object
                                       v2       = sy-msgv1 ).

       CONTINUE.
      ENDIF.

      " For later processing of Assignment Action in late SAVE_MODIFIED phase
      zbp_dsag_r_classification=>ls_assignment_detail-classinternalid = <ls_classification>-Class.
      zbp_dsag_r_classification=>ls_assignment_detail-classtype       = <ls_classification>-ClassType.
      zbp_dsag_r_classification=>ls_assignment_detail-material_number = <ls_key>-%param-object.

      APPEND zbp_dsag_r_classification=>ls_assignment_detail TO zbp_dsag_r_classification=>it_assignment_details.
    ENDLOOP.

    " Read changed data for action result

    result = VALUE #( FOR Classification IN lt_Classifications
                      ( ClassInternalID        = Classification-ClassInternalID
                        %param-ClassInternalID = Classification-ClassInternalID ) ).
  ENDMETHOD.

  METHOD setDefaultValues.
    MODIFY ENTITIES OF zdsag_r_Classification IN LOCAL MODE
           ENTITY Classification
           UPDATE FIELDS ( ClassStatus ValidityStartDate ValidityEndDate )
           WITH VALUE #( FOR key IN keys
                         ( %tky              = key-%tky
                           ClassStatus       = '1'
                           ValidityStartDate = cl_abap_context_info=>get_system_date( )
                           ValidityEndDate   = '99991231' ) )
           CREATE BY \_ClassificationDescription
           FIELDS ( Language LanguageEdit )
           WITH VALUE #( FOR key IN keys INDEX INTO i
                         ( %tky    = key-%tky
                           %target = VALUE #( ( %cid      = i
                                                %is_draft = key-%is_draft
                                                Language = sy-langu
                                                LanguageEdit  = sy-langu ) ) ) ).
  ENDMETHOD.

  " ----------------------------------METHOD check_if_classtype_exists------------------------------------------------------------
  "      1. To Check if "Classification" created is using only existing "Classification Types" Example: Material Class(001)
  " ------------------------------------------------------------------------------------------------------------------------------

  METHOD check_if_classtype_exists.
    " Still needed,For other consumer scenarios like EML, REST etc..
    READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
         ENTITY Classification
         ALL FIELDS
         WITH VALUE #( FOR key IN keys
                          ( %tky             = key-%tky
                            ClassInternalID  = key-classinternalid  )
                      )
         RESULT DATA(Classifications).

    LOOP AT classifications ASSIGNING FIELD-SYMBOL(<ls_classification>).

      SELECT COUNT(*) FROM I_ClfnClassTypeBasic WHERE ClassType = @<ls_classification>-classtype.

      IF sy-subrc = 0.
        " OK as we are using only existing Classification Type Eg: Material Class(001)
      ELSE.
        " Not OK -Usage of no existing classification type is not permitted
        APPEND VALUE #( %key = <ls_classification>-%key
                        %Pid = <ls_classification>-%Pid )

               TO failed-classification.

        APPEND VALUE #( %key = <ls_classification>-%key
                        %msg = new_message( id       = zbp_dsag_r_classification=>lc_msg_class
                                            number   = 001
                                            severity = if_abap_behv_message=>severity-error
                                            v1       = <ls_classification>-classtype ) )
               TO reported-classification.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD simulate_create.
    " 1. Leveraging BAPI test run to report erros in UI
    DATA mapped_characteristics TYPE STANDARD TABLE OF zdsag_r_classcharacters.
    DATA mapped_descriptions    TYPE STANDARD TABLE OF zdsag_r_classdescription.
    DATA lt_return              TYPE STANDARD TABLE OF bapiret2.

    READ ENTITIES OF zdsag_r_classification IN LOCAL MODE
         ENTITY Classification
         ALL FIELDS WITH VALUE #( FOR key IN keys
                                              ( %tky             = key-%tky
                                                ClassInternalID  = key-classinternalid  )
                                          )
         RESULT DATA(classifications)
*Classification Description 'ZDSAG_R_CLASSDESCRIPTION'
         ENTITY Classification BY \_ClassificationDescription
         ALL FIELDS WITH VALUE #( FOR key IN keys
                                              ( %tky             = key-%tky
                                                ClassInternalID  = key-classinternalid  )
                                          )
         LINK DATA(links_descriptions)
         RESULT DATA(descriptions)
*Classification Characteristic 'ZDSAG_R_CLASSCHARACTERS'
         ENTITY Classification BY \_ClassificationCharacteristic
         ALL FIELDS WITH VALUE #( FOR key IN keys
                                              ( %tky             = key-%tky
                                                ClassInternalID  = key-classinternalid  )
                                          )
         LINK DATA(links_characteristics)
         RESULT DATA(Characteristics).

    " Simulate create
    LOOP AT classifications ASSIGNING FIELD-SYMBOL(<fs_classification>) WHERE ClassInternalID IS INITIAL
         GROUP BY <fs_classification>-%tky.

      LOOP AT links_descriptions ASSIGNING FIELD-SYMBOL(<link_description>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        DATA(description) = descriptions[ KEY id
                                          %tky = <link_description>-target-%tky ].
        APPEND CORRESPONDING #( description ) TO mapped_descriptions ASSIGNING FIELD-SYMBOL(<mapper_description>).
      ENDLOOP.

      LOOP AT links_characteristics ASSIGNING FIELD-SYMBOL(<links_characteristic>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        DATA(characteristic) = Characteristics[ KEY id
                                                %tky = <links_characteristic>-target-%tky ].
        APPEND CORRESPONDING #( characteristic ) TO mapped_characteristics.
      ENDLOOP.
      " Calling C1 released API from Classic ABAP package.
      TRY.
          zbp_dsag_r_classification_exte=>bapi_class_create(
            EXPORTING iv_classification  = CORRESPONDING #( <fs_classification> )
                      it_descriptions    = mapped_descriptions
                      it_characteristics = mapped_characteristics
                      test_run           = abap_true
            IMPORTING ev_classinternalid = DATA(rs_classinternalid)
            CHANGING  return             = lt_return ).
        CATCH cx_root.
          " handle exception
      ENDTRY.

      " 'BAPI returned messages are used to display in UI --------
      handle_bapi_return( EXPORTING header_tky = VALUE #( %is_draft = <fs_classification>-%is_draft
                                                          %pid      = <fs_classification>-%pid
                                                          ClassInternalID = rs_classinternalid )
                                         return     = lt_return
                               IMPORTING reported   = reported
                                         failed     = failed  ).
      "
    ENDLOOP.

    " Simulate change
    LOOP AT classifications ASSIGNING <fs_classification> WHERE ClassInternalID IS NOT INITIAL
         GROUP BY <fs_classification>-%tky.

      LOOP AT links_descriptions ASSIGNING <link_description> USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        description = descriptions[ KEY id
                                    %tky = <link_description>-target-%tky ].
        APPEND CORRESPONDING #( description ) TO mapped_descriptions ASSIGNING <mapper_description>.
      ENDLOOP.

      LOOP AT links_characteristics ASSIGNING <links_characteristic> USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        characteristic = Characteristics[ KEY id
                                          %tky = <links_characteristic>-target-%tky ].
        APPEND CORRESPONDING #( characteristic ) TO mapped_characteristics.
      ENDLOOP.

      IF <fs_classification>-%is_draft = if_abap_behv=>mk-on.
        zdsag_classification_utility=>get_dsp_facade( )->get_enqueue( 'ZDSAG_R_CLASSIFICATION' )->get_enqueue_context(
            is_pkey = <fs_classification>-%pky  )->attach( ).
      ENDIF.

      " Calling C1 released API.
      TRY.
          zbp_dsag_r_classification_exte=>bapi_class_change(
            EXPORTING iv_classification  = CORRESPONDING #( <fs_classification> )
                      it_descriptions    = mapped_descriptions
                      it_characteristics = mapped_characteristics
                      test_run           = abap_true
            CHANGING  return             = lt_return ).
        CATCH cx_root.
          " handle exception
      ENDTRY.
      IF <fs_classification>-%is_draft = if_abap_behv=>mk-on.
        zdsag_classification_utility=>get_dsp_facade( )->get_enqueue( 'ZDSAG_R_CLASSIFICATION' )->get_enqueue_context(
            is_pkey = <fs_classification>-%pky  )->detach( ).
      ENDIF.

      " 'BAPI returned messages are used to display in UI --------
      handle_bapi_return( EXPORTING header_tky = VALUE #( %is_draft = <fs_classification>-%is_draft
                                                          %pid      = <fs_classification>-%pid
                                                          ClassInternalID = <fs_classification>-ClassInternalID )
                                         return     = lt_return
                               IMPORTING reported   = reported
                                         failed     = failed  ).
      "
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_bapi_return.
    APPEND VALUE #( %tky        = header_tky-%tky
                    %state_area = 'ERROR' )
           TO reported-classification.
    LOOP AT return INTO DATA(ret).
      CASE ret-type.
        WHEN 'E' OR 'W'.
          IF ret-type = 'E'.
            APPEND VALUE #( %tky = header_tky-%tky ) TO failed-classification.
          ENDIF.
          APPEND VALUE #(
              %tky                       = header_tky-%tky
              %state_area                = 'ERROR'
              %msg                       = new_message(
                  id       = ret-id
                  number   = ret-number
                  severity = COND #(
                                WHEN ret-type = 'E' THEN if_abap_behv_message=>severity-error
                                WHEN ret-type = 'W' THEN if_abap_behv_message=>severity-warning )
                  v1       = ret-message_v1
                  v2       = ret-message_v2
                  v3       = ret-message_v3
                  v4       = ret-message_v4 )
              %element-ValidityEndDate   = COND #(
                                                WHEN ret-type = 'E' AND ret-id = 'CL' AND ret-number = '011'
                                                THEN if_abap_behv=>mk-on )
              %element-Class             = COND #(
                                                WHEN ret-type = 'E' AND ret-id = 'CL' AND ret-number = '002'
                                                THEN if_abap_behv=>mk-on )
              %element-ValidityStartDate = COND #(
                                                WHEN ret-type = 'E' AND ret-id = 'CL' AND ret-number = '010'
                                                THEN if_abap_behv=>mk-on ) )

              TO reported-classification.

        WHEN OTHERS.
          " No handling for success or information messages

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_Description DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Description RESULT result.
    METHODS setdefaultvalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR description~setdefaultvalues.

ENDCLASS.


CLASS lhc_Description IMPLEMENTATION.
  METHOD get_instance_features.
  ENDMETHOD.
  METHOD setDefaultValues.

    MODIFY ENTITIES OF zdsag_r_Classification IN LOCAL MODE
           ENTITY Description
           UPDATE FIELDS (  LanguageEdit )
           WITH VALUE #(
                         ( ClassInternalID = keys[ 1 ]-ClassInternalID
                           Language = keys[ 1 ]-language
                           %is_draft = keys[ 1 ]-%is_draft
                           %pid = keys[ 1 ]-%pid
                           %control-LanguageEdit     = if_abap_behv=>mk-on
                           LanguageEdit  = keys[ 1 ]-language ) )
    FAILED DATA(failed).


  ENDMETHOD.

ENDCLASS.


CLASS lhc_Characteristics DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Characteristics RESULT result.

ENDCLASS.


CLASS lhc_Characteristics IMPLEMENTATION.
  METHOD get_instance_features.
  ENDMETHOD.
ENDCLASS.


CLASS lsc_ZDSAG_R_CLASSIFICATION DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    TYPES ty_header_tky TYPE STRUCTURE FOR PERMISSIONS KEY zdsag_r_Classification\\Classification.
    TYPES ty_bapiret2   TYPE STANDARD TABLE OF bapiret2.
    TYPES ty_reported   TYPE RESPONSE FOR REPORTED LATE zdsag_r_Classification.

    METHODS adjust_numbers   REDEFINITION.

    METHODS save_modified    REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

    METHODS handle_bapi_return
      IMPORTING header_tky TYPE ty_header_tky
                !return    TYPE ty_bapiret2
      EXPORTING !reported  TYPE ty_reported.

  PRIVATE SECTION.
    CLASS-DATA mapped_characteristics TYPE STANDARD TABLE OF zdsag_r_classcharacters.
    CLASS-DATA mapped_descriptions    TYPE STANDARD TABLE OF zdsag_r_classdescription.

ENDCLASS.


CLASS lsc_ZDSAG_R_CLASSIFICATION IMPLEMENTATION.
*----------------------------------METHOD adjust_numbers----------------------------------------
  " 1.The assumption is we are creating new "Classification/CL01" via Tier 2 API.
  "       "Accessing C1 released Nominated APIs
  "       "Messages from BAPI are passed to UI layer.
  " 2. WHY ?? "Adjust Numbers" method instead of "Save Modified"
  "           1.BAPI returns "Classification ID " for new classification instance
  "           2."MAP" key field to RAP Bo instance
  " -------------------------------------------------------------------------------------------------

  METHOD adjust_numbers.
    CHECK mapped IS NOT INITIAL.
    DATA lt_return              TYPE ty_bapiret2.
    DATA mapped_characteristics TYPE STANDARD TABLE OF zdsag_r_classcharacters.
    DATA mapped_descriptions    TYPE STANDARD TABLE OF zdsag_r_classdescription.
    READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
         ENTITY Classification
         ALL FIELDS WITH CORRESPONDING #( mapped-classification )
         RESULT DATA(classifications)
*Classification Description 'ZDSAG_R_CLASSDESCRIPTION'
         ENTITY Classification BY \_ClassificationDescription
         ALL FIELDS WITH CORRESPONDING #( mapped-classification )
         LINK DATA(links_descriptions)
         RESULT DATA(descriptions)
*Classification Characteristic 'ZDSAG_R_CLASSCHARACTERS'
         ENTITY Classification BY \_ClassificationCharacteristic
         ALL FIELDS WITH CORRESPONDING #( mapped-classification )
         LINK DATA(links_characteristics)
         RESULT DATA(Characteristics).

    LOOP AT classifications ASSIGNING FIELD-SYMBOL(<fs_classification>) GROUP BY <fs_classification>-%tky.

      LOOP AT links_descriptions ASSIGNING FIELD-SYMBOL(<link_description>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        DATA(description) = descriptions[ KEY id
                                          %tky = <link_description>-target-%tky ].
        APPEND CORRESPONDING #( description ) TO mapped_descriptions ASSIGNING FIELD-SYMBOL(<mapper_description>).
      ENDLOOP.

      LOOP AT links_characteristics ASSIGNING FIELD-SYMBOL(<links_characteristic>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
        DATA(characteristic) = Characteristics[ KEY id
                                                %tky = <links_characteristic>-target-%tky ].
        APPEND CORRESPONDING #( characteristic ) TO mapped_characteristics.
      ENDLOOP.
      TRY.
          zbp_dsag_r_classification_exte=>bapi_class_create(
            EXPORTING iv_classification  = CORRESPONDING #( <fs_classification> )
                      it_descriptions    = mapped_descriptions
                      it_characteristics = mapped_characteristics
                      test_run           = abap_false
            IMPORTING ev_classinternalid = DATA(rs_classinternalid)
            CHANGING  return             = lt_return ).
        CATCH cx_root.
          " handle exception
      ENDTRY.

      " 'BAPI returned messages are used to display in UI --------

      handle_bapi_return( EXPORTING header_tky = VALUE #( %is_draft = <fs_classification>-%is_draft
                                                          %pid      = <fs_classification>-%pid
                                                          ClassInternalID = rs_classinternalid )
                                    return     = lt_return
                          IMPORTING reported   = reported  ).

      " Here we pass the "Classification Internal ID"  returned from BAPI
      " to Mapping parameters so key for this BO instance is mapped
      mapped-classification[ %pid = <fs_classification>-%pid ]-ClassInternalID = rs_classinternalid.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_modified.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.

    " For Update
    IF update-classification IS NOT INITIAL.
      READ ENTITIES OF zdsag_r_Classification IN LOCAL MODE
           ENTITY Classification
           ALL FIELDS WITH CORRESPONDING #( update-classification )
           RESULT DATA(classifications)
" Classification Description 'ZDSAG_R_CLASSDESCRIPTION'
           ENTITY Classification BY \_ClassificationDescription
           ALL FIELDS WITH CORRESPONDING #( update-classification )
           LINK DATA(links_descriptions)
           RESULT DATA(descriptions)
" Classification Characteristic 'ZDSAG_R_CLASSCHARACTERS'
           ENTITY Classification BY \_ClassificationCharacteristic
           ALL FIELDS WITH CORRESPONDING #( update-classification )
           LINK DATA(links_characteristics)
           RESULT DATA(Characteristics).

      LOOP AT classifications ASSIGNING FIELD-SYMBOL(<fs_classification>) GROUP BY <fs_classification>-%tky.

        LOOP AT links_descriptions ASSIGNING FIELD-SYMBOL(<link_description>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
          DATA(description) = descriptions[ KEY id
                                            %tky = <link_description>-target-%tky ].
          APPEND CORRESPONDING #( description ) TO mapped_descriptions.
        ENDLOOP.
        LOOP AT create-description ASSIGNING FIELD-SYMBOL(<fs_new_description>) GROUP BY <fs_classification>-%tky.
          APPEND CORRESPONDING #( <fs_new_description> ) TO mapped_descriptions.
        ENDLOOP.
        LOOP AT links_characteristics ASSIGNING FIELD-SYMBOL(<links_characteristic>) USING KEY id WHERE source-%tky = <fs_classification>-%tky.
          DATA(characteristic) = Characteristics[ KEY id
                                                  %tky = <links_characteristic>-target-%tky ].
          APPEND CORRESPONDING #( characteristic ) TO mapped_characteristics.
        ENDLOOP.
        LOOP AT create-characteristics ASSIGNING FIELD-SYMBOL(<fs_new_characteristics>) GROUP BY <fs_classification>-%tky.
          APPEND CORRESPONDING #( <fs_new_characteristics> ) TO mapped_characteristics.
        ENDLOOP.

        TRY.
            zbp_dsag_r_classification_exte=>bapi_class_change(
              EXPORTING iv_classification  = CORRESPONDING #( <fs_classification> )
                        it_descriptions    = mapped_descriptions
                        it_characteristics = mapped_characteristics
                        test_run           = abap_false
              CHANGING  return             = lt_return ).
          CATCH cx_root.
            " handle exception
        ENDTRY.

        " 'BAPI returned messages are used to display in UI --------
        handle_bapi_return( EXPORTING header_tky = VALUE #( %is_draft = <fs_classification>-%is_draft
                                                          %pid      = <fs_classification>-%pid
                                                          ClassInternalID = <fs_classification>-ClassInternalID )
                                      return     = lt_return
                            IMPORTING reported   = reported  ).

      ENDLOOP.
    ENDIF.
    " For Action
    LOOP AT zbp_dsag_r_classification=>it_assignment_details ASSIGNING FIELD-SYMBOL(<fs_assignment>).

      TRY.
          zbp_dsag_r_classification_exte=>bapi_objcl_create(
            EXPORTING classnumnew  = <fs_assignment>-classinternalid
                      classtypenew = <fs_assignment>-ClassType
                      objectkeynew = CONV #( <fs_assignment>-material_number )
            CHANGING  return       = lt_return ).
        CATCH cx_root.
      ENDTRY.
      handle_bapi_return( EXPORTING header_tky = VALUE #( ClassInternalID = <fs_assignment>-ClassInternalID )
                                    return     = lt_return
                          IMPORTING reported   = reported  ).

    ENDLOOP.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.
  " ----------------------------------METHOD handle_bpi_return----------------------------------------
  "   1. Handling of BAPI messages
  " -------------------------------------------------------------------------------------------------

  METHOD handle_bapi_return.
    LOOP AT return INTO DATA(ret).
      IF ( ret-id = 'CL' AND ret-number = '802' ) .
        " Skip some default messages from classic world for example :Start of class processing: ..
      ELSE.
        CASE ret-type.
          WHEN 'S' OR 'I' OR 'E' OR 'W'.
            APPEND VALUE #(
                %tky = header_tky-%tky
                %msg = new_message(
                           id       = ret-id
                           number   = ret-number
                           severity = COND #( WHEN ret-type = 'S' THEN if_abap_behv_message=>severity-success
                                              WHEN ret-type = 'I' THEN if_abap_behv_message=>severity-information
                                              WHEN ret-type = 'E' THEN if_abap_behv_message=>severity-error
                                              WHEN ret-type = 'W' THEN if_abap_behv_message=>severity-warning
                                              ELSE                     if_abap_behv_message=>severity-none )
                           v1       = ret-message_v1
                           v2       = ret-message_v2
                           v3       = ret-message_v3
                           v4       = ret-message_v4 ) )
                   TO reported-classification.

          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
