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

#' Load the nomenclature file given in the Orphanet nomenclature pack
#'
#' @return The nomenclature data as an xml2 object
#' @importFrom xml2 read_xml
#' @export
load_nomenclature = function()
{
  # Load data
  pack_nomenclature_path = system.file('extdata', 'Orphanet_Nomenclature_Pack_FR', package='orphatools')
  orpha_nomenclature_path = file.path(pack_nomenclature_path, 'ORPHAnomenclature_fr.xml')
  nomenclature_data = read_xml(orpha_nomenclature_path, encoding = 'ISO-8859-1')

  return(nomenclature_data)
}


#' Find properties related to a given Orpha code
#'
#' @param orphaCode The Orpha code to consider
#' @param nom_data The nomenclature data as it is in the Orphanet nomenclature pack. If NULL, the function loads it itself.
#' @param litteral_values If TRUE, returned values are human readable.
#'
#' It is recommended to load nomenclature first and pass it as an argument to avoid memory overflow when the function is called multiple times.
#'
#' @return The properties (flag value, disorder type, classification level) associated to the given Orpha code
#' @import magrittr
#' @importFrom xml2 xml_find_first xml_child xml_text xml_attr
#' @export
#'
#' @examples
#' code = '303'
#' props = get_code_properties(code)
#'
#' nom_data = load_nomenclature()
#' props = get_code_properties(code, nom_data = nom_data)
#'
#' props = get_code_properties(code, nom_data = nom_data, litteral_values = TRUE)
get_code_properties = function(orphaCode, nom_data=NULL, literal_values=FALSE)
{
  # Load nomenclature if necessary
  if(is.null(nom_data))
    nom_data = load_nomenclature()

  # Get disorder node
  node = xml_find_first(nom_data, sprintf('//Disorder[OrphaCode=%s]', orphaCode))

  # Retrieve properties
  flagValue = node %>% xml_child('FlagValue') %>% xml_text() # Active / Inactive
  disorderType = node %>% xml_child('DisorderType') %>% xml_attr('id') # Clinical entities
  classLevel = node %>% xml_child('ClassificationLevel') %>% xml_attr('id') # Group / Disorder / Subtype
  name = node %>% xml_child('Name') %>% xml_text()

  if(literal_values){
    flagValue = flagValue_literal[flagValue] %>% unname
    disorderType = disorderType_literal[disorderType] %>% unname
    classLevel = classLevel_literal[classLevel] %>% unname
  }

  # Gather properties and return
  props = data.frame(orphaCode = orphaCode,
                     name = name,
                     flagValue = flagValue,
                     disorderType = disorderType,
                     classLevel = classLevel)

  rm(nom_data)
  return(props)
}


#' Generate and load an OrphaCode -> Classification Level table
#'
#' @param load Load a dumped output. It the output does not exist, it is recalculated
#' @param save Dump the output for later readings
#'
#' @return The OrphaCode -> Classification Level table, as defined in the Orphanet nomenclature
#' @export
#'
#' @examples
#' class_levels = load_class_levels()
#'
load_class_levels = function(load=TRUE, save=TRUE)
{
  extdata_path = system.file('extdata', package='orphatools')

  # Load data
  if(load && file.exists(file.path(extdata_path, 'class_levels.RDS')))
    class_levels = readRDS(file.path(extdata_path, 'class_levels.RDS'))
  else
  {
    nom_data = load_nomenclature()
    all_codes = nom_data %>%
        xml_find_all('//OrphaCode') %>%
        xml_text() %>%
        unique()
    class_levels = data.frame(orphaCode = all_codes) %>%
        mutate(classLevel = sapply(orphaCode,
                                   function(x) get_code_properties(x,
                                                                   nom_data=nom_data) %>%
                                     pull(classLevel)))

    if(save)
      saveRDS(class_levels, file.path(extdata_path, 'class_levels.RDS'))
  }

  return(class_levels)
}

#' Translate a flagValue column into human readable data
#'
#' @param V, Vector to translate
#'
#' @return The dataframe with the translated column
#' @export
#'
translate_flag_values = function(V)
{
  new_V = flagValue_literal[V]
  return(new_V)
}

#' Translate a disorderType column into human readable data
#'
#' @param V, Vector to translate
#'
#' @return The dataframe with the translated column
#' @export
#'
translate_disorder_types = function(V)
{
  new_V = disorderType_literal[V]
  return(new_V)
}


#' Translate a classLevel column into human readable data
#'
#' @param V, Vector to translate
#'
#' @return The dataframe with the translated column
#' @export
#'
translate_class_level = function(V)
{
  new_V = classLevel_literal[V]
  return(new_V)
}
