CLASS zdsag_classification_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      enqueue_classification
        IMPORTING
          classType  TYPE klassenart
          class      TYPE klasse_d
        EXPORTING
          lock_error TYPE abap_boolean.
    CLASS-METHODS:
      dequeue_classification
        IMPORTING
          classType TYPE klassenart
          class     TYPE klasse_d .

    CLASS-METHODS:
      get_dsp_facade
        RETURNING VALUE(ro_dsp_facade) TYPE REF TO if_dsp_facade.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zdsag_classification_helper IMPLEMENTATION.
  METHOD enqueue_classification.
    CALL FUNCTION 'ENQUEUE_ECKSSKXE'
      EXPORTING
        klart          = classType
        class          = class
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 3.

    IF sy-subrc <> 0.
      lock_error = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD dequeue_classification.
    CALL FUNCTION 'DEQUEUE_ECKSSKXE'
      EXPORTING
        klart = classType
        class = class.

  ENDMETHOD.

  METHOD get_dsp_facade.
    ro_dsp_facade = cl_dsp_facade=>get( ).
  ENDMETHOD.

ENDCLASS.
