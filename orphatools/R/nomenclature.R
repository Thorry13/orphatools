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

#' Update the Orpha nomenclature pack. Orphanet updates its pack on a yearly basis.
#'
#' @param orpha_pack_nomenclature_path the path to the uncompressed nomenclature pack.
#' You'll find it available here : https://www.orphadata.com/pack-nomenclature/
#' @param nomenclature_xml_file nomenclature file is supposed to be _ORPHAnomenclature_fr.xml_, but might differ.
#' @param classifications_folder classifications folder is supposed to be _Classifications_fr_, but might differ
#'
#' @return
#' @export
#'
#' @examples
update_nomenclature_pack = function(orpha_pack_nomenclature_path,
                                    nomenclature_xml_file = 'ORPHAnomenclature_fr.xml',
                                    classifications_folder = 'Classifications'){
  flatten_data = function(disorder_node){
    props_df = data.frame(
      orphaCode = disorder_node$OrphaCode[[1]],
      name = disorder_node$Name[[1]],
      flagValue = disorder_node$FlagValue[[1]],
      disorderType = disorder_node$DisorderType %>% attr('id'),
      classLevel = disorder_node$ClassificationLevel %>% attr('id')
    )
    return(props_df)
  }

  find_synonyms = function(disorder_node){
    df_synonyms = data.frame(
      orphaCode = disorder_node$OrphaCode[[1]],
      name = disorder_node$Name[[1]],
      count = disorder_node$SynonymList %>% attr('count') %>% as.numeric(),
      synonyms =
          sapply(disorder_node$SynonymList,
                 function(x)  x[[1]]) %>%
          unname()
      ) %>%
      mutate(synonyms = ifelse(count, synonyms, as.character(NA))) %>%
      select(-count)
  }

  nomenclature_path = file.path(orpha_pack_nomenclature_path, nomenclature_xml_file)
  classifications_path = file.path(orpha_pack_nomenclature_path, classifications_folder)
  if(file.exists(nomenclature_path) & file.exists(classifications_path)){
    message('Loading and processing Orpha data. This may take a few minutes.')

    # Load raw data from xml files
    nomenclature_data_raw = read_xml(nomenclature_path, encoding = 'ISO-8859-1') %>% as_list()

    # Preprocess data to make it R-manageable
    nomenclature_data = nomenclature_data_raw[[1]][[2]] %>%
      lapply(flatten_data) %>%
      bind_rows()

    df_synonyms = nomenclature_data_raw[[1]][[2]] %>%
      lapply(find_synonyms) %>%
      bind_rows()

    rm(nomenclature_data_raw)

    # Find classifications
    class_list = list.files(classifications_path) %>% as.list()
    all_class = class_list %>%
      lapply(function(class_file) read_class(file.path(classifications_path, class_file))) %>%
      setNames(class_list %>% sapply(tools::file_path_sans_ext))

    # Save changes
    extdata_path = system.file('extdata', package='orphatools')
    saveRDS(nomenclature_data, file.path(extdata_path, 'nom_data.RDS'))
    saveRDS(df_synonyms, file.path(extdata_path, 'df_synonyms.RDS'))
    saveRDS(all_class, file.path(extdata_path, 'all_class.RDS'))
  }
  else
    stop(simpleError('Invalid parameters.'))

  message('Nomenclature pack was succesfully updated.')
}


#' Load the nomenclature file given in the Orphanet nomenclature pack,
#' then process it to make it more R-friendly.
#'
#' @return The nomenclature data as a data.frame object.
#'
#' @import magrittr
#' @importFrom xml2 read_xml as_list
#' @importFrom dplyr bind_rows
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
See `upload_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(nom_data)
}


#' Load for each orphaCode its list of synonyms from the nomenclature data
#'
#' @return A data.frame object with three columns : orpha Code, preferred term, synonyms
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
See `upload_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(df_synonyms)
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
  nom_data_translated = nom_data %>%
    mutate(
      flagValue = flagValue_literal[flagValue] %>% unname,
      disorderType = disorderType_literal[disorderType] %>% unname,
      classLevel = classLevel_literal[classLevel] %>% unname
    )
  return(nom_data_translated)
}


#' Get the flag value of a given ORPHA code
#'
#' @param orphaCode, ORPHA code to get the flag value from
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

#' Get the disorder type of a given ORPHA code
#'
#' @param orphaCode, ORPHA codeto get the disorder type from
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


#' Get the classification level of a given ORPHA code
#'
#' @param orphaCode, ORPHA code to get the classification level from
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
