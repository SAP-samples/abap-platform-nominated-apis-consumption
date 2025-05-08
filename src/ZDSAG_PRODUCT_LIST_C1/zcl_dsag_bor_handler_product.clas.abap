CLASS zcl_dsag_bor_handler_product DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES bi_event_handler_static .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS trigger_email
      IMPORTING
        i_event_container TYPE REF TO if_swf_ifs_parameter_container
      RETURNING
        VALUE(rv_evt_obj) TYPE matnr.
    CLASS-METHODS trigger_flp_notification
      IMPORTING
        iv_evt_obj TYPE matnr.
ENDCLASS.



CLASS zcl_dsag_bor_handler_product IMPLEMENTATION.
  METHOD bi_event_handler_static~on_event.

    " ------------------------- Maintenance Status -----------------------------
    " Verify Maintenance Status
    " --------------------------------------------------------------------------
    DATA maintenancestatus TYPE statm.

    TRY.
        event_container->get( EXPORTING name  = 'MAINTENANCESTATUS'
                              IMPORTING value = maintenancestatus ).

      CATCH cx_swf_cnt_elem_not_found cx_swf_cnt_elem_type_conflict cx_swf_cnt_unit_type_conflict cx_swf_cnt_container.
        " handle exception
    ENDTRY.

    " Only process classification (C) changes.
    CHECK maintenancestatus EQ 'C'.


    " ------------------------- Classic E-Mail  -------------------------------
    " Trigger Classic E-Mail
    " --------------------------------------------------------------------------

    DATA lv_evt_obj TYPE matnr.

    lv_evt_obj = trigger_email( event_container ).
    " ------------------------- FLP Notification ------------------------------
    " Trigger FLP Notiifcation
    " --------------------------------------------------------------------------

    trigger_flp_notification( lv_evt_obj ).

    " ------------------------- EML modification -------------------------
    " Trigger EML Modif
    " --------------------------------------------------------------------------

    cl_abap_tx=>modify( ).

    MODIFY ENTITIES OF zrm_r_classification_m
           ENTITY Classification
           UPDATE FIELDS ( validityenddate ) WITH VALUE #( ( ClassInternalID = 0000000068
                                                             ValidityEndDate = '99991231' ) ).
    TRY.
        cl_abap_tx=>save( ).
      CATCH cx_abap_behv_save_failed INTO DATA(lx_ABAP_BEHV_SAVE_FAILED).
        "Perform Error Handling in case of Failed keys

    ENDTRY.

    COMMIT ENTITIES.

    " ------------------------- RAP Business Event -------------------------
    " Trigger RAP event -to connect to external event
    " --------------------------------------------------------------------------

    DATA lt_events TYPE zbp_dsag_r_product=>tt_events.

    SELECT * FROM zrm_r_classification_m
      INTO CORRESPONDING FIELDS OF TABLE @lt_events
      WHERE ClassInternalID = 0000000139. " ClassInternal ID

    zbp_dsag_r_product=>raise_event( lt_events  ).

    COMMIT ENTITIES.
  ENDMETHOD.


  METHOD trigger_email.

    TRY.
        i_event_container->get( EXPORTING name  = '_EVT_OBJKEY'
                                IMPORTING value = rv_evt_obj ).

      CATCH cx_swf_cnt_elem_not_found cx_swf_cnt_elem_type_conflict cx_swf_cnt_unit_type_conflict cx_swf_cnt_container.
        " handle exception
    ENDTRY.

    " Event created by

    DATA lv_created_by TYPE string.

    TRY.
        i_event_container->get( EXPORTING name  = '_EVT_CREATOR'
                                IMPORTING value = lv_created_by ).

      CATCH cx_swf_cnt_elem_not_found cx_swf_cnt_elem_type_conflict cx_swf_cnt_unit_type_conflict cx_swf_cnt_container.
        " handle exception
    ENDTRY.

    DATA(ls_line) = VALUE soli( line = `Usage of BOR Events in Tier 2 wrapper` ).
    DATA(lv_length) = strlen( lv_created_by ).

    lv_length -= 2.

    ls_line-line = | Material: { rv_evt_obj } has been assigned with classification by user { lv_created_by+2(lv_length) }.  |.
*      -------------
    DATA: wa_objcont TYPE solisti1,
          it_objcont TYPE STANDARD TABLE OF solisti1,
          reclist    TYPE somlreci1,
          it_reclist TYPE STANDARD TABLE OF somlreci1.
    DATA: doc_chng TYPE  sodocchgi1.

* Email subject line
    doc_chng-obj_descr = 'Usage of BOR Events in Tier 2 wrapper'.

* Email body
    wa_objcont = ls_line-line.
    APPEND wa_objcont TO it_objcont.

* Receiver list
    reclist-receiver = 'DREES'.
    APPEND reclist TO it_reclist.
    reclist-receiver = 'RAJASEKARANM'.
    APPEND reclist TO it_reclist.
    reclist-receiver = 'SUBRAMANIVIG'.
    APPEND reclist TO it_reclist.

* Send the document
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        commit_work                = 'X'
      TABLES
        object_content             = it_objcont
        receivers                  = it_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.


  ENDMETHOD.


  METHOD trigger_flp_notification.

    DATA: lt_notif TYPE /iwngw/if_notif_provider=>ty_t_notification,
          ls_notif TYPE /iwngw/if_notif_provider=>ty_s_notification.


    TRY.
        ls_notif-id =  cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
      CATCH cx_uuid_error.
    ENDTRY.

    ls_notif-priority = /iwngw/if_notif_provider=>gcs_priorities-high.
    ls_notif-type_key = 'DevtoberfestApiConsumption'.
    ls_notif-type_version = '1'.
    ls_notif-actor_id = sy-uname.
    ls_notif-recipients = VALUE #( ( id = 'DREES' ) ( id ='RAJASEKARANM' ) ( id = 'SUBRAMANIVIG' ) ).
    ls_notif-actor_display_text = |Notification from BOR handler was created successfully |.
    ls_notif-parameters = VALUE #( (  language = sy-langu
                                   parameters = VALUE #( (  name = 'product_num'
                                                         type =  /iwngw/if_notif_provider=>gcs_parameter_types-type_string
                                                         value = iv_evt_obj )
                                                         (  name = 'class_num'
                                                         type =  /iwngw/if_notif_provider=>gcs_parameter_types-type_string
                                                         value = '001') ) ) ).
    APPEND ls_notif TO lt_notif.

    TRY.
        /iwngw/cl_notification_api=>create_notifications(
          EXPORTING
            iv_provider_id  = 'Z_DEVTOBERFEST_NOTIF'

            it_notification = lt_notif ).
      CATCH /iwngw/cx_notification_api .
    ENDTRY.

    COMMIT WORK.

  ENDMETHOD.

ENDCLASS.
