@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Creation R view'
define root view entity ZDSAG_R_CLASSIFICATION
  as select from I_ClfnClassDEX
  composition [0..*] of ZDSAG_R_CLASSDESCRIPTION as _ClassificationDescription
  composition [0..*] of ZDSAG_R_CLASSCHARACTERS  as _ClassificationCharacteristic
  association [0..*] to I_ClfnClassTypeText      as _ClassType        on  _ClassType.ClassType = $projection.ClassType
  association [0..*] to I_ClfnClassDescription   as _ClassDescription on  _ClassDescription.ClassInternalID = $projection.ClassInternalID
  association [0..1] to I_ClfnClassStatusText    as _ClassStatus      on  $projection.ClassStatus = _ClassStatus.ClassStatus
                                                                      and $projection.ClassType   = _ClassStatus.ClassType
                                                                      and _ClassStatus.Language   = $session.system_language
  association [0..*] to ZDSAG_PRODUCT_HELPER   as _ProductHelper on  $projection.ClassInternalID = _ProductHelper.ClassInternalID
                                                                     and $projection.ClassType = _ProductHelper.ClassType
{
  key ClassInternalID,
      ClassType,
      Class,
      ClassStatus,
      @Semantics.user.createdBy: true
      CreatedByUser,
      CreationDate,
      @Semantics.user.localInstanceLastChangedBy: true
      LastChangedByUser,
      LastChangeDate,
      ValidityStartDate,
      ValidityEndDate,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @Semantics.systemDateTime.lastChangedAt: true
      ClassLastChangedDateTime,
      /* Associations */
      _ClassCharacteristic,
      _ClassDescription,
      _ClassStatus,
      _ClassType,
      _ClassificationDescription,
      _ClassificationCharacteristic,
      _ProductHelper
}
