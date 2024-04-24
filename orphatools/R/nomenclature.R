#' Load the nomenclature file given in the Orphanet nomenclature pack,
#' then process it to make it more R-friendly.
#'
#' @return The nomenclature data as a data.frame object.
#'
#' @export
#' @examples
#' nom_data = load_nomenclature()
#'
load_nomenclature = function(){
  extdata_path = system.file('extdata', package='orphatools')
  nom_data_path = file.path(extdata_path, 'nom_data.RDS')

  if(file.exists(nom_data_path))
    nom_data = readRDS(nom_data_path)
  else
    stop(simpleError(
'Loading of nomenclature data failed. Internal files might be broken.
See `update_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(nom_data)
}


#' Load for each orphaCode its list of synonyms from the nomenclature data
#'
#' @return A data.frame object with three columns : ORPHAcode, preferred term, synonyms
#' @export
#'
#' @examples
#' df_synonyms = load_synonyms()
load_synonyms = function(){
  extdata_path = system.file('extdata', package='orphatools')
  synonyms_path = file.path(extdata_path, 'df_synonyms.RDS')

  if(file.exists(synonyms_path))
    df_synonyms = readRDS(synonyms_path)
  else
    stop(simpleError(
'Loading of synonyms failed. Internal files might be broken.
See `update_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(df_synonyms)
}


#' Load for each obsolete or deprecated ORPHAcode the corresponding association,
#' which means the ORPHAcode it was moved to or referred to
#'
#' @return A data.frame object giving the inactive ORPHAcodes, the corresponding associations with the association type
#' @export
#'
#' @examples
load_associations = function(){
  extdata_path = system.file('extdata', package='orphatools')
  associations_path = file.path(extdata_path, 'associations.RDS')

  if(file.exists(associations_path))
    df_associations = readRDS(associations_path)
  else
    stop(simpleError(
'Loading of associations failed. Internal files might be broken.
See `update_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(df_associations)
}


#' Translate abstract codes into literal values to make them more understandable
#'
#' @param nom_data The nomenclature data as loaded with the `load_nomenclature` function.
#'
#' @return The same properties (name, flag value, disorder type, classification level)
#' in a human-readable way
#'
#' @import magrittr
#' @importFrom xml2 xml_find_first xml_child xml_text xml_attr
#' @export
#'
#' @examples
#' nom_data = load_nomenclature()
#' nom_data_new = translate_properties(nom_data)
#'
translate_properties = function(nom_data){
  flagValue_literal = c('1'='Active',
                      '129'='Active',
                      '513'='Active',
                      '8449'='Inactive: Deprecated',
                      '8208'='Inactive: Obsolete',
                      '9216'='Inactive: Obsolete',
                      '8225'='Inactive: Non rare in Europe')
  disorderType_literal = c('36561'='Category',
                           '21436'='Clinical group',
                           '21394'='Disease',
                           '21422'='Clinical syndrome',
                           '21401'='Malformation syndrome',
                           '21408'='Biolofical anomaly',
                           '21415'='Morphological anomaly',
                           '21429'='Particular clinical situation in a disease or syndrome',
                           '21443'='Etiological subtype',
                           '21450'='Clinical subtype',
                           '21457'='Histopathological subtype')
  classLevel_literal = c('36540'='Group',
                         '36547'='Disorder',
                         '36554'='Subtype')

  nom_data_translated = nom_data %>%
    mutate(
      flagValue = flagValue_literal[flagValue] %>% unname,
      disorderType = disorderType_literal[disorderType] %>% unname,
      classLevel = classLevel_literal[classLevel] %>% unname
    )
  return(nom_data_translated)
}


#' Get the flag value of a given ORPHAcode
#'
#' @param orphaCode, ORPHAcode to get the flag value from
#'
#' @return The corresponding flag value
#' @export
#'
get_flag_value = function(code){
  flagValue = load_nomenclature() %>%
    translate_properties() %>%
    filter(orphaCode==code) %>%
    pull(flagValue)
  return(flagValue)
}


#' Get the disorder type of a given ORPHAcode
#'
#' @param orphaCode, ORPHAcodeto get the disorder type from
#'
#' @return The corresponding disorder type
#' @export
#'
get_disorder_type = function(code){
  disorderType = load_nomenclature() %>%
    translate_properties() %>%
    filter(orphaCode==code) %>%
    pull(disorderType)
  return(disorderType)
}


#' Get the classification level of a given ORPHAcode
#'
#' @param orphaCode, ORPHAcode to get the classification level from
#'
#' @return The corresponding classification level
#' @export
#'
get_classification_level = function(code){
  classLevel = load_nomenclature() %>%
    translate_properties() %>%
    filter(orphaCode==code) %>%
    pull(classLevel)
  return(classLevel)
}
