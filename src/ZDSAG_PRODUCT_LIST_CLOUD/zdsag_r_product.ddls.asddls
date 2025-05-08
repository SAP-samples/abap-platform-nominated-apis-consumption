@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product List root view'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZDSAG_R_PRODUCT
  as select from I_Product
  association [0..*] to ZDSAG_I_ClassificationHelper as _ClassificationHelper on  $projection.Product = _ClassificationHelper.ClfnObjectID
  association [0..1] to I_ProductDescription_2       as _ProductDescription_2 on  $projection.Product            = _ProductDescription_2.Product
                                                                              and _ProductDescription_2.Language = $session.system_language
{
  key Product,
      _ProductDescription_2.ProductDescription as ProductDescription,
      BaseUnit,
      CreationDateTime,
      ValidityStartDate,
      CreatedByUser,
      LastChangeDateTime,
      LastChangedByUser,
      /* Associations */
      _ClassificationHelper,
      _ProductDescription_2
}
