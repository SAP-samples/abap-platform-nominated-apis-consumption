@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Classification Description'
define view entity ZDSAG_R_CLASSDESCRIPTION
  as select from I_ClfnClassDescription
  association to parent ZDSAG_R_CLASSIFICATION as _Classfications on $projection.ClassInternalID = _Classfications.ClassInternalID

{
  key ClassInternalID,
  key Language,
      Language as LanguageEdit,  
      ClassDescription,
      /* Associations */
      _Class,
      _Classfications
}
