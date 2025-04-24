@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Mitigate Material Value Help'
@Search.searchable: true
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.sizeCategory : #L
@ObjectModel.usageType.dataClass: #MASTER
@Consumption.ranked: true
define view entity ZDSAG_I_MaterialVH as select from I_Product
  association [0..1] to I_ProductDescription_2 as _ProductDescription on  $projection.Product     = _ProductDescription.Product
                                                                      and _ProductDescription.Language = $session.system_language

{
  key Product  ,
  ProductType,
  _ProductDescription.ProductDescription,
  _ProductDescription
}
