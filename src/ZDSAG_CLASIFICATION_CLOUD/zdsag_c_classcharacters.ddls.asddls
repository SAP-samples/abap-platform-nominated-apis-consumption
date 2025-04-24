@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Characteristics C view'
@Metadata.allowExtensions: true
define view entity ZDSAG_C_CLASSCHARACTERS
  as projection on ZDSAG_R_CLASSCHARACTERS
{
  key ClassInternalID,
  key CharcPositionNumber,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_ClfnCharcDesc', element: 'CharcInternalID' } }]
      @ObjectModel.text.element: ['CharacteristicName']

      CharcInternalID,
      _CharacteristicsName.Characteristic                                           as CharacteristicName,

      _ClassCharDescription[1:Language = $session.system_language].CharcDescription as CharacteristicDescription,

      /* Associations */
      _CharacteristicsName,
      _ClassCharDescription,
      _Classfications : redirected to parent ZDSAG_C_CLASSIFICATION
}
