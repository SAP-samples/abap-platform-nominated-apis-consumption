@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Decription C View'
@Metadata.allowExtensions: true
define view entity ZDSAG_C_CLASSDESCRIPTION
  as projection on ZDSAG_R_CLASSDESCRIPTION
{
  key ClassInternalID,
      @Consumption.valueHelpDefinition: [{ entity: { name : 'I_Language', element : 'Language' } }]
  key Language,
      @Consumption.valueHelpDefinition: [{ entity: { name : 'I_Language', element : 'Language' } }]
      LanguageEdit,
      ClassDescription,
      /* Associations */
      _Class,
      _Classfications : redirected to parent ZDSAG_C_CLASSIFICATION
}
