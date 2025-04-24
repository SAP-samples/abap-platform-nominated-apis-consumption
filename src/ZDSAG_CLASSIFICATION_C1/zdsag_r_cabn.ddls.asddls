@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root View for Characteristics'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZDSAG_R_CABN
  as select from I_ClfnCharcBasic
{
  key CharcInternalID,
      Characteristic
}
