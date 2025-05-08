CLASS lhc_ProductList DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ProductList RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ProductList RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK ProductList.

    METHODS assign_class_to_product FOR MODIFY
      IMPORTING keys FOR ACTION ProductList~assign_class_to_product.

ENDCLASS.

CLASS lhc_ProductList IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD lock.
    READ ENTITIES OF zdsag_r_product IN LOCAL MODE
      ENTITY ProductList
      ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(Products)
    FAILED failed
    REPORTED reported.

    LOOP AT Products INTO DATA(product).
      zcl_dsag_product_utility=>enqueue_product(
        EXPORTING
          i_matnr    = product-Product
        IMPORTING
          lock_error = DATA(lv_lock_error) ).
      IF lv_lock_error EQ abap_true.
        APPEND CORRESPONDING #( product ) TO failed-productlist ASSIGNING FIELD-SYMBOL(<failed>).
        <failed>-%fail-cause = if_abap_behv=>cause-locked.
        APPEND CORRESPONDING #( product ) TO reported-productlist ASSIGNING FIELD-SYMBOL(<reported>).
        <reported>-%msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = |Instance locked by User { sy-msgv1 }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD assign_class_to_product.
    DATA ls_return TYPE bapiret2.
    DATA lt_return TYPE zcl_dsag_class_assignment_chk=>t_bapiret2.

    READ ENTITIES OF zdsag_r_product IN LOCAL MODE
         ENTITY ProductList
         ALL FIELDS
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_products).

    LOOP AT keys INTO DATA(ls_key).
      IF line_exists( lt_products[ KEY id Product = ls_key-Product ] ). " Material Found

        " Check if Assignment is already made between Material and Classification
        zcl_dsag_class_assignment_chk=>does_obj_classification_exist(
          EXPORTING classtype         = zbp_dsag_r_product=>lc_classification_type
                    classnum          = ls_key-%param-class
                    keydate           = cl_abap_context_info=>get_system_date( )
                    objectkey         = CONV #( ls_key-product )
                    objecttable       = zbp_dsag_r_product=>lc_material_master
          IMPORTING assignment_exists = DATA(assignment_exists) ).

        IF assignment_exists = abap_true.
          ls_return-id         = zbp_dsag_r_product=>lc_msg_class.
          ls_return-number     = 007.
          ls_return-type       = zbp_dsag_r_product=>lc_error.
          ls_return-message_v1 = ls_key-product.
          ls_return-message_v2 = sy-msgv1.
          APPEND ls_return TO lt_return.
          CONTINUE.
        ENDIF.
        zdsag_classification_helper=>enqueue_classification(
          EXPORTING classtype  = zbp_dsag_r_product=>lc_classification_type
                    class      = ls_key-%param-class
          IMPORTING lock_error = DATA(lv_lock_error) ).

        IF lv_lock_error = abap_true.
          ls_return-id         = zbp_dsag_r_product=>lc_msg_class.
          ls_return-number     = 003.
          ls_return-type       = zbp_dsag_r_product=>lc_error.
          ls_return-message_v1 = ls_key-%param-class.
          ls_return-message_v2 = sy-msgv1.
          APPEND ls_return TO lt_return.
          CONTINUE.
        ENDIF.
      ELSE. " Material Not Found
        ls_return-id         = zbp_dsag_r_product=>lc_msg_class.
        ls_return-number     = 005.
        ls_return-type       = zbp_dsag_r_product=>lc_error.
        ls_return-message_v1 = ls_key-product.
        APPEND ls_return TO lt_return.
        CONTINUE.
      ENDIF.

      zbp_dsag_r_product=>ls_assignment_detail-class   = ls_key-%param-class.
      zbp_dsag_r_product=>ls_assignment_detail-product = ls_key-Product.

      APPEND zbp_dsag_r_product=>ls_assignment_detail TO zbp_dsag_r_product=>it_assignment_details.
      CLEAR ls_return.
    ENDLOOP.

    LOOP AT lt_return INTO ls_return WHERE type = zbp_dsag_r_product=>lc_error.
      APPEND VALUE #( product = ls_return-message_v1
                      %msg    = new_message( id       = ls_return-id
                                             number   = ls_return-number
                                             severity = if_abap_behv_message=>severity-error
                                             v1       = ls_return-message_v1
                                             v2       = ls_return-message_v2 ) )
             TO reported-productlist.

      IF ls_return-type = zbp_dsag_r_product=>lc_error.
        APPEND VALUE #( %key = CORRESPONDING #( ls_key ) )
               TO failed-productlist.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZDSAG_R_PRODUCT DEFINITION INHERITING FROM cl_abap_behavior_saver_failed.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZDSAG_R_PRODUCT IMPLEMENTATION.

  METHOD save_modified.
    DATA ls_return TYPE bapiret2.
    DATA lt_return TYPE zcl_dsag_bapi_objcl=>t_bapiret2.

    LOOP AT zbp_dsag_r_product=>it_assignment_details ASSIGNING FIELD-SYMBOL(<fs_assignment>).
      zbp_dsag_r_product_ext=>bapi_objcl_create( EXPORTING  classnumnew    = <fs_assignment>-class
                                                            classtypenew   = zbp_dsag_r_product=>lc_classification_type
                                                            keydate        = cl_abap_context_info=>get_system_date( )
                                                            objectkeynew   = CONV #( <fs_assignment>-product )
                                                            objecttablenew = zbp_dsag_r_product=>lc_material_master
                                                  CHANGING  return         = lt_return ).
      LOOP AT lt_return INTO ls_return.
        IF NOT ( ls_return-type = 'I' AND ls_return-id = 'CL' AND ls_return-number = '732' ).
          APPEND VALUE #(
              product = <fs_assignment>-product
              %msg = new_message( id       = ls_return-id
                                  number   = ls_return-number
                                  severity = SWITCH #( ls_return-type
                                                       WHEN 'S' THEN if_abap_behv_message=>severity-success
                                                       WHEN 'E' THEN if_abap_behv_message=>severity-error
                                                       WHEN 'W' THEN if_abap_behv_message=>severity-warning
                                                       WHEN 'I' THEN if_abap_behv_message=>severity-information
                                                       WHEN 'A' THEN if_abap_behv_message=>severity-error )
                                  v1       = ls_return-message_v1
                                  v2       = ls_return-message_v2
                                  v3       = ls_return-message_v3
                                  v4       = ls_return-message_v4 ) )
                 TO reported-productlist.
        ENDIF.

        IF ls_return-type = zbp_dsag_r_product=>lc_error.
          APPEND VALUE #( product = <fs_assignment>-product )
                 TO failed-productlist.
        ELSE.
          DATA(atleast_one_success_assignment) = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF failed-productlist IS NOT INITIAL AND atleast_one_success_assignment = abap_true.

      CLEAR reported-productlist.

      APPEND VALUE #(
          product = <fs_assignment>-product
          %msg = new_message( id       = zbp_dsag_r_product=>lc_msg_class
                              number   = 006
                              severity = if_abap_behv_message=>severity-error
                             ) )
             TO reported-productlist.

    ENDIF.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
