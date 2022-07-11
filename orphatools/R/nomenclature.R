#' Load the nomenclature file given in the Orphanet nomenclature pack
#'
#' @return The nomenclature data as an xml2 object
#' @importFrom xml2 read_xml
#' @export
#'
#' @examples
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
#'
#' @return The properties (flag value, disorder type, classification level) associated to the given Orpha code
#' @import magrittr
#' @importFrom xml2 xml_find_first xml_child xml_text xml_attr
#' @export
#'
#' @examples
get_code_properties = function(orphaCode, nom_data=NULL)
{
  # Load nomenclature if necessary
  if(is.null(nom_data))
    nom_data = load_nomenclature()

  # Get disorder node
  node = xml_find_first(nom_data, sprintf('//Disorder[OrphaCode=%d]', orphaCode))

  # Retrieve properties
  flagValue = node %>% xml_child('FlagValue') %>% xml_text() %>% as.numeric() # Active / Inactive
  disorderType = node %>% xml_child('DisorderType') %>% xml_attr('id') %>% as.numeric() # Clinical entities
  classLevel = node %>% xml_child('ClassificationLevel') %>% xml_attr('id') %>% as.numeric() # Group / Disorder / Subtype

  # Gather properties and return
  props = c(orphaCode = orphaCode,
            flagValue = flagValue,
            disorderType = disorderType,
            classLevel = classLevel)

  rm(nom_data)
  return(props)
}

