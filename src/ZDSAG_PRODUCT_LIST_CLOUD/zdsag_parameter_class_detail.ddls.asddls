@EndUserText.label: 'Parameter for class details'

define abstract entity zdsag_parameter_class_detail
{
    @EndUserText.label: 'Class'
    @Consumption.valueHelpDefinition: [{ entity: { name: 'ZDSAG_I_Classification_Clas_VH', element: 'Class' } }]
    class : abap.char(18);
}
