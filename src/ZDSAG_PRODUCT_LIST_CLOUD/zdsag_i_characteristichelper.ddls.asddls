@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Characteristic Helper'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZDSAG_I_CharacteristicHelper
  as select from I_ClfnClassCharacteristicDEX as ClassCharacteristic
    inner join   I_ClfnCharcDesc              as CharcDesc on ClassCharacteristic.CharcInternalID = CharcDesc.CharcInternalID
{
      @UI.facet: [{ type:     #IDENTIFICATION_REFERENCE,
                    label:    'Characteristics',
                    position: 10 }]

  key ClassCharacteristic.ClassInternalID,
  key ClassCharacteristic.CharcPositionNumber,
  key ClassCharacteristic.ValidityEndDate,
      ClassCharacteristic.CharcInternalID,
      @UI: { identification: [ { position:10 } ],
             lineItem: [ { position:10 } ] }
      CharcDesc.CharcDescription
}
