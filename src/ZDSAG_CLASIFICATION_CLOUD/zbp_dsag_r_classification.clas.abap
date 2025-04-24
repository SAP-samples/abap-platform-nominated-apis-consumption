CLASS zbp_dsag_r_classification DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zdsag_r_classification.

  TYPES tt_events  TYPE TABLE FOR EVENT zdsag_r_classification~ClassificationCreated.
  TYPES klasse_d   TYPE c LENGTH 000018.
  TYPES klassenart TYPE c LENGTH 000003.

  CLASS-METHODS
    raise_event
      IMPORTING it_events TYPE tt_events.

  TYPES : BEGIN OF t_action_assignment,
            classtype       TYPE zbp_dsag_r_classification=>klassenart,
            ClassInternalID TYPE zbp_dsag_r_classification=>klasse_d,
            material_number TYPE matnr,
          END OF t_action_assignment.

  CLASS-DATA it_assignment_details TYPE STANDARD TABLE OF t_action_assignment.
  CLASS-DATA ls_assignment_detail  TYPE t_action_assignment.

  CONSTANTS lc_msg_class TYPE symsgid VALUE 'ZDSAG_CLASSIFICATION'.
ENDCLASS.


CLASS zbp_dsag_r_classification IMPLEMENTATION.
  METHOD raise_event.
    RAISE ENTITY EVENT zdsag_r_classification~classificationcreated
          FROM VALUE #( FOR <s_classification> IN  it_events
                        ( ClassInternalID = <s_classification>-ClassInternalID  ) ).
  ENDMETHOD.
ENDCLASS.
