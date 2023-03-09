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


#' Load the nomenclature file given in the Orphanet nomenclature pack,
#' then process it to make it more R-friendly.
#'
#' @param load Load a dumped output. It the output does not exist, it is recalculated
#' @param save Dump the output for later readings
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
load_nomenclature = function(load = TRUE, save = TRUE)
{
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

  extdata_path = system.file('extdata', package='orphatools')

  # Load data
  if(load && file.exists(file.path(extdata_path, 'nom_data.RDS')))
    nom_data_R = readRDS(file.path(extdata_path, 'nom_data.RDS'))
  else
  {
    # Load data
    pack_nomenclature_path = system.file('extdata', 'Orphanet_Nomenclature_Pack_FR', package='orphatools')
    orpha_nomenclature_path = file.path(pack_nomenclature_path, 'ORPHAnomenclature_fr.xml')
    nomenclature_data = read_xml(orpha_nomenclature_path, encoding = 'ISO-8859-1') %>% as_list()

    # Preprocess data to make it R-manageable
    nom_data_R = nomenclature_data[[1]][[2]] %>%
      lapply(flatten_data) %>%
      bind_rows()

    rm(nomenclature_data)
    if(save)
      saveRDS(nom_data_R, file.path(extdata_path, 'nom_data.RDS'))
  }
  return(nom_data_R)
}


#' Load for each orphaCode its list of synonyms from the nomenclature data
#'
#' @param load Load a dumped output. It the output does not exist, it is recalculated
#' @param save Dump the output for later readings
#'
#' @return A data.frame object with three columns : orpha Code, preferred term, synonyms
#' @export
#'
#' @examples
#' df_synonyms = load_synonyms()
load_synonyms = function(load = TRUE, save = TRUE){
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


  extdata_path = system.file('extdata', package='orphatools')

  # Load data
  if(load && file.exists(file.path(extdata_path, 'df_synonyms.RDS')))
    df_synonyms = readRDS(file.path(extdata_path, 'df_synonyms.RDS'))
  else
  {
    # Load data
    pack_nomenclature_path = system.file('extdata', 'Orphanet_Nomenclature_Pack_FR', package='orphatools')
    orpha_nomenclature_path = file.path(pack_nomenclature_path, 'ORPHAnomenclature_fr.xml')
    nomenclature_data = read_xml(orpha_nomenclature_path, encoding = 'ISO-8859-1') %>% as_list()

    # Preprocess data to make it R-manageable
    df_synonyms = nomenclature_data[[1]][[2]] %>%
      lapply(find_synonyms) %>%
      bind_rows()

    rm(nomenclature_data)
    if(save)
      saveRDS(df_synonyms, file.path(extdata_path, 'df_synonyms.RDS'))
  }
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
translate_properties = function(nom_data)
{
  nom_data_translated = nom_data %>%
    mutate(
      flagValue = flagValue_literal[flagValue] %>% unname,
      disorderType = disorderType_literal[disorderType] %>% unname,
      classLevel = classLevel_literal[classLevel] %>% unname
    )
  return(nom_data_translated)
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
