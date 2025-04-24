CLASS zdsag_bor_event_handler DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES bi_event_handler_static.

  PRIVATE SECTION.
    CLASS-METHODS trigger_email
      IMPORTING i_event_container TYPE REF TO if_swf_ifs_parameter_container
      RETURNING VALUE(rv_evt_obj) TYPE matnr.
ENDCLASS.


CLASS zdsag_bor_event_handler IMPLEMENTATION.
  METHOD bi_event_handler_static~on_event.
    " ------------------------- Classic E-Mail  -------------------------------
    " Trigger Classic E-Mail
    " --------------------------------------------------------------------------

    DATA lv_evt_obj TYPE matnr.

    lv_evt_obj = trigger_email( event_container ).

    " ------------------------- EML modification -------------------------
    " Trigger EML Modif
    " --------------------------------------------------------------------------

    MODIFY ENTITIES OF zdsag_r_classification
           ENTITY Classification
           UPDATE FIELDS ( validityenddate ) WITH VALUE #( ( ClassInternalID = 0000000068
                                                             ValidityEndDate = '99991231' ) ).

    COMMIT ENTITIES.

    " ------------------------- RAP Business Event -------------------------
    " Trigger RAP event -to connect to external event
    " --------------------------------------------------------------------------

    DATA lt_events TYPE zbp_dsag_r_classification=>tt_events.

    SELECT * FROM zdsag_r_classification
      INTO CORRESPONDING FIELDS OF TABLE @lt_events
      WHERE ClassInternalID = 0000000068. " ClassInternal ID

    zbp_dsag_r_classification=>raise_event( lt_events  ).

    COMMIT ENTITIES.
  ENDMETHOD.

  METHOD trigger_email.
    TRY.
        i_event_container->get( EXPORTING name  = '_EVT_OBJKEY'
                                IMPORTING value = rv_evt_obj ).

      CATCH cx_swf_cnt_elem_not_found
            cx_swf_cnt_elem_type_conflict
            cx_swf_cnt_unit_type_conflict
            cx_swf_cnt_container.
        " handle exception
    ENDTRY.

    " Event created by

    DATA lv_created_by TYPE string.

    TRY.
        i_event_container->get( EXPORTING name  = '_EVT_CREATOR'
                                IMPORTING value = lv_created_by ).

      CATCH cx_swf_cnt_elem_not_found
            cx_swf_cnt_elem_type_conflict
            cx_swf_cnt_unit_type_conflict
            cx_swf_cnt_container.
        " handle exception
    ENDTRY.

    DATA(ls_line) = VALUE soli( line = `Usage of BOR Events in Tier 2 API` ).
    DATA(lv_length) = strlen( lv_created_by ).

    lv_length -= 2.

    ls_line-line = | Material: { rv_evt_obj } has been assigned with new view by user { lv_created_by+2(lv_length) }.  |.
    "       -------------
    DATA wa_objcont TYPE solisti1.
    DATA it_objcont TYPE STANDARD TABLE OF solisti1.
    DATA reclist    TYPE somlreci1.
    DATA it_reclist TYPE STANDARD TABLE OF somlreci1.
    DATA doc_chng   TYPE sodocchgi1.

    " Email subject line
    doc_chng-obj_descr = 'Usage of BOR Events in Tier 2 wrapper'.

    " Email body
    wa_objcont = ls_line-line.
    APPEND wa_objcont TO it_objcont.

    " Receiver list
    reclist-receiver = sy-uname.
    APPEND reclist TO it_reclist.
    " Send the document
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING  document_data              = doc_chng
                 commit_work                = 'X'
      TABLES     object_content             = it_objcont
                 receivers                  = it_reclist
      EXCEPTIONS too_many_receivers         = 1
                 document_not_sent          = 2
                 operation_no_authorization = 4
                 OTHERS                     = 99.
  ENDMETHOD.
ENDCLASS.
