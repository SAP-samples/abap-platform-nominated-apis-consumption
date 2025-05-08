CLASS zbp_dsag_r_product DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zdsag_r_product.
  PUBLIC SECTION.
    TYPES tt_events TYPE TABLE FOR EVENT zdsag_r_product~ClassAssigned_2_MATNR.
    TYPES klasse_d          TYPE c LENGTH 000018 .
    TYPES klassenart        TYPE c LENGTH 000003.

    TYPES :BEGIN OF t_action_assignment,
             classtype TYPE zbp_dsag_r_product=>klassenart,
             class     TYPE zbp_dsag_r_product=>klasse_d,
             product   TYPE matnr,
           END OF t_action_assignment.

    CLASS-DATA:
      it_assignment_details TYPE STANDARD TABLE OF t_action_assignment,
      ls_assignment_detail  TYPE t_action_assignment.
    CONSTANTS:
      lc_material_master     TYPE c LENGTH 000030 VALUE 'MARA',
      lc_error               TYPE bapi_mtype VALUE 'E',
      lc_classification_type TYPE klassenart VALUE '001',
      lc_msg_class           TYPE symsgid VALUE 'ZDSAG_CLASS_ASSIGN'.
    CLASS-METHODS
      raise_event
        IMPORTING it_events TYPE tt_events.
ENDCLASS.

CLASS zbp_dsag_r_product IMPLEMENTATION.
  METHOD raise_event.
    RAISE ENTITY EVENT zdsag_r_product~ClassAssigned_2_MATNR
    FROM VALUE #( FOR <s_classification> IN  it_events ( Product = <s_classification>-Product  ) ).
  ENDMETHOD.
ENDCLASS.
