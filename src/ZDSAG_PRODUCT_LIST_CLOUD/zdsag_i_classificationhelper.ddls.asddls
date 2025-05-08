@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Assignment Helper'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZDSAG_I_ClassificationHelper
  as select from I_ClfnObjectClassDEX as Object
    inner join   I_ClfnClass          as Class on Class.ClassInternalID = Object.ClassInternalID
  association [0..*] to ZDSAG_I_CharacteristicHelper as _CharacteristicHelper on $projection.ClassInternalID = _CharacteristicHelper.ClassInternalID
{
      @UI.facet: [{ type:     #IDENTIFICATION_REFERENCE,
                     label:    'Assignments',
                     position: 10 },
                   { type:     #LINEITEM_REFERENCE,
                     label:    'Characteristics',
                     position: 20,
                     targetElement: '_CharacteristicHelper' }]

  key Object.ClfnObjectID,
  key Object.ClassInternalID,
      @UI: { identification: [ { position:30 } ],
             lineItem: [ { position:30 } ] }
  key Object.ClassType,
  key Object.ClfnObjectType,
  key Object.TimeIntervalNumber,

      Object.ClfnObjectTable,
      Object.ClfnStatus,
      @UI: { identification: [ { position:40 } ],
             lineItem: [ { position:40 } ] }
      Object.ClassPositionNumber,

      @UI: { identification: [ { position:10 } ],
             lineItem: [ { position:10 } ] }
      Class.Class,
      @UI: { identification: [ { position:20 } ],
             lineItem: [ { position:20 } ] }
      Class._ClassDescription[1:Language = $session.system_language].ClassDescription,
      Class.ClassStatus,

      _CharacteristicHelper
}
where
  Object.ClfnObjectTable = 'MARA'
