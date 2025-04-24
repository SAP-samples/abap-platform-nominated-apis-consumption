@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Characteristics'
define view entity ZDSAG_R_CLASSCHARACTERS
  as select from I_ClfnClassCharacteristicDEX
  association        to parent ZDSAG_R_CLASSIFICATION as _Classfications       on $projection.ClassInternalID = _Classfications.ClassInternalID

  association [0..1] to ZDSAG_R_CABN                    as _CharacteristicsName  on _CharacteristicsName.CharcInternalID = $projection.CharcInternalID
  association [0..*] to I_ClfnCharcDesc               as _ClassCharDescription on _ClassCharDescription.CharcInternalID = $projection.CharcInternalID

{
  key ClassInternalID,
  key CharcPositionNumber,
      CharcInternalID,
      /* Associations */
      _Classfications,
      _CharacteristicsName,
      _ClassCharDescription
}
