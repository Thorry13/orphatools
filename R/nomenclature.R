#' ORPHAcodes properties
#'
#' @description
#' Load all ORPHAcodes properties in a combined data.frame.
#'
#' @details
#' The label is the clinical entity name and is returned in the `label` column.
#' If the displayed language does not meet your needs,
#' you should consider changing options via [orphatools_options()] or adding a new nomenclature pack using [add_nomenclature_pack()].
#'
#' The classification level is the clinical entity name and is returned in the `level` column.
#' Classification level can either be a group of disorders, a disorder or a subtype of disorder.
#'
#' The status of an ORPHAcode is a value contained in the following terms :
#' "Active", "Inactive: Deprecated", "Inactive: Obsolete", "Inactive: Non rare disease in Europe".
#'
#' The disorder type indicates the clinical entity's typology and is returned by `get_type`.
#'
#' For more details, please read the full Orphanet description included in the nomenclature pack (*.pdf*).
#'
#' @return The ORPHAcodes properties as a data.frame object.
#'
#' @export
#' @seealso [get_label()], [get_classification_level()], [get_status()], [get_type()] to access specific properties,
#' [load_synonyms()], [get_all_labels()], [load_redirections()], [load_redirections()] to access other pieces of information contained in the nomenclature file.
#' @name load_nomenclature
load_raw_nomenclature = function(){
  v = getOption('orphatools_nomenclature', default_pack_version())
  nomenclature_path = get_pack_versions() %>% filter(version==v) %>% pull(location)

  #internal pack_data is silently loaded
  if(file.exists(nomenclature_path))
    load(nomenclature_path) # Load other pack_data
  else if(nomenclature_path != 'internal')
    stop(simpleError(
    'Loading of nomenclature data failed. Internal files might be broken.
    See `orphatools_options`, `add_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(pack_data$nomenclature)
}


#' @rdname load_nomenclature
#' @export
load_nomenclature = function(){
  df_nomenclature = load_raw_nomenclature() %>% translate_orpha_concepts()
  return(df_nomenclature)
}


#' ORPHAcode specific properties
#'
#' @description
#' Get specific properties of given ORPHAcodes.
#'
#' @details
#' The label is the clinical entity name and is returned by `get_label`.
#' As multiple synonyms can be associated to a clinical entity, this function only returns the preferrential labels.
#' To get all synonyms instead, use `get_synonyms`. Besides, if the displayed language does not meet your needs,
#' you should consider changing options via [orphatools_options()] or adding a new nomenclature pack using [add_nomenclature_pack()].
#'
#' The classification level is the clinical entity name and is returned by `get_classification_level`.
#' Classification level can either be a group of disorders, a disorder or a subtype of disorder.
#'
#' The status of an ORPHAcode is a value contained in the following terms :
#' "Active", "Inactive: Deprecated", "Inactive: Obsolete", "Inactive: Non rare disease in Europe".
#' It is returned by `get_status`, while `is_active` just returns TRUE if status is "Active" and FALSE else.
#'
#' The disorder type indicates the clinical entity's typology and is returned by `get_type`.
#'
#' For more details, please read the full Orphanet description included in the nomenclature pack (*.pdf*).
#'
#' @param orpha_codes The ORPHAcodes as a vector from which properties should be returned.
#'
#' @import magrittr
#' @importFrom dplyr left_join
#' @return The corresponding properties of the given ORPHAcodes.
#'
#' @note All properties described above are accessible in a combined format through the `load_nomenclature` function.
#'
#' @seealso [load_nomenclature()], [load_raw_nomenclature()]
#'
#' @examples
#' orpha_codes = c('303', '595356', '79410')
#' get_label(orpha_codes)
#' get_classification_level(orpha_codes)
#' get_status(orpha_codes)
#' is_active(orpha_codes)
#' get_type(orpha_codes)
#'
#' @name orphacode-properties
NULL

#' @rdname orphacode-properties
#' @export
get_label = function(orpha_codes){
  labels = data.frame(orpha_code=as.character(orpha_codes)) %>%
    left_join(load_nomenclature(), by='orpha_code') %>%
    pull(label)
  return(labels)
}

#' @rdname orphacode-properties
#' @export
get_classification_level = function(orpha_codes){
  levels = data.frame(orpha_code=as.character(orpha_codes)) %>%
    left_join(load_nomenclature(), by='orpha_code') %>%
    pull(level)
  return(levels)
}


#' @rdname orphacode-properties
#' @export
get_status = function(orpha_codes){
  status = data.frame(orpha_code=as.character(orpha_codes)) %>%
    left_join(load_nomenclature(), by='orpha_code') %>%
    pull(status)
  return(status)
}

#' @rdname orphacode-properties
#' @export
is_active = function(orpha_codes){
  get_status(orpha_codes) %>%
    str_detect('Active') %>%
    return()
}

#' @rdname orphacode-properties
#' @export
get_type = function(orpha_codes){
  disorder_types = data.frame(orpha_code=as.character(orpha_codes)) %>%
    left_join(load_nomenclature(), by='orpha_code') %>%
    pull(type)
  return(disorder_types)
}


# Translate Orphanet concepts
translate_orpha_concepts = function(X, cols=NULL){
  t = function(x){
    xt = data.frame(id=as.character(x)) %>%
      left_join(df_dict, by='id') %>%
      pull(label)
    return(xt)
  }

  # Load dict
  dict = getOption('orphatools_dict', default_dict())
  dict_path = get_dict_versions() %>% filter(version==dict) %>% pull(location)

  # df_dict is already lazy-loaded. To be updated only if option references external version.
  if(file.exists(dict_path))
    df_dict = read.csv2(dict_path, colClasses='character')
  else if(dict_path != 'internal')
    stop(simpleError(
    'Loading of dictionary failed. Internal files might be broken.
    See `orphatools_options`, `add_dictionary` or consider reisntalling orphatools package.'))

  if(is.data.frame(X) & is.null(cols))
    cols = intersect(names(X), unique(df_dict$id_family))

  if(is.data.frame(X))
    Xt = X %>% mutate(across(all_of(cols), ~t(.x)))
  else
    Xt = t(X)

  return(Xt)
}

