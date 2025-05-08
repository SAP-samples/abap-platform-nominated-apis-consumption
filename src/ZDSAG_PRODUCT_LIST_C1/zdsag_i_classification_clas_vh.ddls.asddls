@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification class value help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.representativeKey: 'Class'
@Search.searchable: true
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.usageType.sizeCategory: #M
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #MASTER
@Consumption.ranked: true
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZDSAG_I_Classification_Clas_VH
  as select from I_ClfnClass
{
      @ObjectModel.text.association: '_ClassDescription'
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
  key Class,
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
  key ClassType,
      @Consumption.hidden: true
      ClassInternalID,
      @Consumption.hidden: true
      ClassMaintAuthGrp,
      /* Associations */
      _ClassDescription
}
where
      ClassType       = '001'
  and ValidityEndDate >= $session.system_date
