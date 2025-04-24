@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Creation C view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZDSAG_C_CLASSIFICATION
  provider contract transactional_query
  as projection on ZDSAG_R_CLASSIFICATION
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_ClfnClassDescription', element: 'ClassInternalID' } }]


  key ClassInternalID,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_ClfnClassTypeStdVH', element: 'ClassType' },useForValidation: true } ]
      @ObjectModel.text.element: ['ClassTypeName']
      ClassType,
      _ClassType.ClassTypeName           as ClassTypeName    : localized,

      @ObjectModel.text.element: ['ClassDescription']
      Class,
      _ClassDescription.ClassDescription as ClassDescription : localized,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_ClfnClassStatusStdVH', element: 'ClassStatus' },
                                          additionalBinding: [{ element: 'ClassType', localElement: 'ClassType',usage: #FILTER }] }]
      @ObjectModel.text.element: ['ClassStatusName']
      ClassStatus,
      _ClassStatus.ClassStatusName       as ClassStatusName,
      CreatedByUser,
      CreationDate,
      LastChangedByUser,
      LastChangeDate,
      ValidityStartDate,
      ValidityEndDate,
      ClassLastChangedDateTime,
      /* Associations */
      _ClassCharacteristic,
      _ClassDescription,
      _ClassStatus,
      _ClassType,
      _ProductHelper,
      _ClassificationDescription    : redirected to composition child ZDSAG_C_CLASSDESCRIPTION,
      _ClassificationCharacteristic : redirected to composition child ZDSAG_C_CLASSCHARACTERS
}
