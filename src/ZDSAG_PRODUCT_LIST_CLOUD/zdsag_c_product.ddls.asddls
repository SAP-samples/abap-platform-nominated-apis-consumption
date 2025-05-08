@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product List consumption view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZDSAG_C_PRODUCT
  provider contract transactional_query
  as projection on ZDSAG_R_PRODUCT
{
      @Consumption: {
        semanticObject: 'Material',
        valueHelpDefinition: [{ entity: { name : 'I_ProductStdVH', element: 'Product' }, label: 'Products by Description' }]
        }
  key Product,
      ProductDescription,
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
