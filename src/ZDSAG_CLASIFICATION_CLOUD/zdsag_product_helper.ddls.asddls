@EndUserText.label: 'Product Helper'
@Metadata.ignorePropagatedAnnotations: true
@UI:{ headerInfo:{
    typeName: 'Assignment',
    typeNamePlural: 'Assignments',
    title:  { type: #STANDARD,value: 'ClfnObjectID', label: 'Product' }},
presentationVariant:[{
    sortOrder: [{ by:'ClfnObjectID', direction:#ASC}], visualizations: [{type: #AS_LINEITEM }] }] }


define view entity ZDSAG_PRODUCT_HELPER
  as select from I_ClfnObjectClassDEX
  association [0..1] to I_ProductDescription_2 as _ProductDescription on  $projection.ClfnObjectID     = _ProductDescription.Product
                                                                      and _ProductDescription.Language = $session.system_language

{
      @UI.facet: [{ id: 'Assignment',
                    purpose: #STANDARD,
                    type:     #IDENTIFICATION_REFERENCE,
                    label:    'Products Assignment',
                    position: 10 }]
      @UI: { identification: [ { position:10 } ],
              lineItem: [ { position:10 } ] }
  key ClfnObjectID,
  key ClassInternalID,
  key ClassType,
      @UI: { identification: [ { position:20 } ],
              lineItem: [ { position:20 } ] }
      _ProductDescription.ProductDescription,
      /* Associations */
      _Class,
      _ProductDescription
}
