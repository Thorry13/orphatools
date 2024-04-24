#' Load the list of genes provided by Orphanet and their associated ORPHAcodes
#'
#' @return A data frame containing for each ORPHAcode the kind of association (if any)
#' with a related gene and all known information (symbol, name, references, locus) about the latter.
#' @export
load_orpha_genes = function(){
  extdata_path = system.file('extdata', package='orphatools')
  orpha_genes_path = file.path(extdata_path, 'orpha_genes.RDS')

  if(file.exists(orpha_genes_path))
    df_orpha_genes = readRDS(orpha_genes_path)
  else
    stop(simpleError(
    'Loading of associations failed. Internal files might be broken.
    See `update_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(df_orpha_genes)
}


#' Load all possible synonyms for each known gene in Orphanet public data
#'
#' @return A data frame containing genes ids for joining operations, their full name and main symbol and all possible synonyms
#' @export
#'
#' @examples
load_genes_synonyms = function(){
  extdata_path = system.file('extdata', package='orphatools')
  genes_synonyms_path = file.path(extdata_path, 'genes_synonyms.RDS')

  if(file.exists(genes_synonyms_path))
    df_genes_synonyms = readRDS(genes_synonyms_path)
  else
    stop(simpleError(
    'Loading of associations failed. Internal files might be broken.
    See `update_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(df_genes_synonyms)
}

#' Find a more precise ORPHAcode according to the provided genes
#'
#' @param orpha_codes The ORPHAcodes to update
#' @param hgnc_codes The provided genes
#' @param by This argument specifies the groups in which the provided genes should be considered.
#'
#' @import magrittr
#' @importFrom dplyr n_distinct filter select distinct group_by summarize ungroup mutate left_join select tibble
#'
#' @return The updated ORPHAcodes, the same length as the `orpha_codes`.
#' An ORPHAcode is changed if a single correspondence was found.
#' @export
#'
#' @examples
#' library(dplyr)
#' df = tibble(
#'   patient_id=c('A', 'A', 'B', 'C', 'D', 'D'),
#'   initial_orphaCode = c("65753", "65753", "903", "65753", "65753", "65753"), # CMT1 and von Willebrand
#'   hgnc_code = c("16463", "16361", "15714", "26792", "16463", "15714")) # MPZ, VWF and LITAF
#'
#' df %>% mutate(assigned_orphaCode = specify_code(initial_orphaCode, hgnc_code))
#' df %>% mutate(assigned_orphaCode = specify_code(initial_orphaCode, hgnc_code, by='patient_id'))
specify_code = function(orpha_codes, hgnc_codes, by=NULL){
  choose_old_or_new = function(diag_code, potential_code){
    n_possibilities = n_distinct(potential_code, na.rm=T)
    if(n_possibilities == 1)
      return(unique(na.omit(potential_code)))
    else
      return(unique(diag_code))
  }

  df_tmp = load_orpha_genes() %>%
    filter(associationStatus == 'Assessed', HGNC %in% hgnc_codes) %>%
    select(orphaCode, hgnc_code=HGNC) %>%
    distinct() %>%
    orpha_df(orphaCode_col = 'orphaCode', force_nodes = unique(na.omit(orpha_codes))) %>%
    group_by(orphaCode, hgnc_code) %>%
    summarize(potential_code = orphaCode) %>%
    ungroup()

  df_new = tibble(orpha_code=orpha_codes, hgnc_code=hgnc_codes, by=by) %>%
    mutate(i = row_number()) %>%
    # unnest(orpha_code, keep_empty = TRUE) %>%
    left_join(df_tmp, by=c('orpha_code'='orphaCode', 'hgnc_code')) %>%
    group_by(across(c(by, orpha_code))) %>%
    mutate(new_code = choose_old_or_new(orpha_code, potential_code)) %>%
    ungroup() %>%
    select(i, new_code) %>%
    distinct()

  return(df_new$new_code)
}
