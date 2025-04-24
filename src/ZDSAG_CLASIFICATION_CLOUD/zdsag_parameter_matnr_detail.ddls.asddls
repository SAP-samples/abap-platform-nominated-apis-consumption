@EndUserText.label: 'Assignmnet Of Classification  to Material'
define abstract entity ZDSAG_PARAMETER_MATNR_DETAIL
{
    @EndUserText.label: 'Material'
@Consumption.valueHelpDefinition: [{ entity: { name: 'I_ProductStdVH', element: 'Product' } }]
    object : matnr;
    
}
