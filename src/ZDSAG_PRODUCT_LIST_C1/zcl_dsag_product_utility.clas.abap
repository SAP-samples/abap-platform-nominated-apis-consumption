CLASS zcl_dsag_product_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS enqueue_product
      IMPORTING
        i_matnr    TYPE char40
      EXPORTING
        lock_error TYPE abap_boolean.
    CLASS-METHODS dequeue_product
      IMPORTING
        i_matnr TYPE char40.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dsag_product_utility IMPLEMENTATION.
  METHOD enqueue_product.
    CALL FUNCTION 'ENQUEUE_EMMARAE'
      EXPORTING
        mode_mara      = 'E'
        mandt          = sy-mandt
        matnr          = i_matnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      lock_error = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD dequeue_product.
    CALL FUNCTION 'DEQUEUE_EMMARAE'
      EXPORTING
        mode_mara = 'E'
        mandt     = sy-mandt
        matnr     = i_matnr.
  ENDMETHOD.

ENDCLASS.
