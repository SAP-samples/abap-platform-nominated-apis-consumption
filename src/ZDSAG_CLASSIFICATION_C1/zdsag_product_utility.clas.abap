CLASS zdsag_product_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS enqueue_product
      IMPORTING
        i_matnr    TYPE matnr
      EXPORTING
        lock_error TYPE abap_boolean.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zdsag_product_utility IMPLEMENTATION.
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
ENDCLASS.
