#' Synonyms
#'
#' @description
#' Any disease can be named in multiple ways. For each ORPHAcode, Orphanet provides a
#' unique preferential label. Any other name is then called synonym.
#'
#' This function returns in a data.frame the ORPHAcodes, their preferential label and synonyms if any.
#'
#' @return A 3-columns data.frame indicating for each ORPHAcode its preferential label and synonyms.
#' `NA` is set if no synonyms was found.
#' @export
#' @seealso [get_all_labels()] to directly extract all the names associated to the given ORPHAcode.
load_synonyms = function(){
  # Get nomenclature version
  version = getOption('nomenclature_version', default_nom_version())
  extdata_path = system.file('extdata', package='orphatools')
  synonyms_path = file.path(extdata_path, 'nom_data', version, 'synonyms.RDS')

  if(file.exists(synonyms_path))
    df_synonyms = readRDS(synonyms_path)
  else
    stop(simpleError(
'Loading of synonyms failed. Internal files might be broken.
See `orphatools_options` or consider reisntalling orphatools package.'))

  return(df_synonyms)
}


#' Disease labels
#'
#' @description
#' Extract the synonyms of the given ORPHAcode as well as its preferential label, given in first position.
#'
#' @param orpha_code An ORPHAcode.
#'
#' @return A character vector containing all labels associated to the given ORPHAcode.
#'
#' @export
#' @seealso [load_synonyms()] to load the synonyms table.
get_all_labels = function(orpha_code){
  if(length(orpha_code) > 1)
    return(sapply(orpha_code, get_all_labels) %>% setNames(orpha_code))

  df_synonyms = load_synonyms() %>%
    filter(orpha_code == .env$orpha_code)
  return(unique(c(df_synonyms$pref_label, df_synonyms$synonyms)))
}
