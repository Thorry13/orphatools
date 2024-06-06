#' Associated genes
#'
#' @description
#' Load all needed information to explore associations between ORPHAcodes and genes.
#'
#' `load_associated_genes` loads the association tables.
#'
#' `load_gene_synonyms` loads genes synonyms in case your available data refers
#' to genes in a different way.
#'
#' @details
#' The loaded dataframes directly depend on the `"orphatools_gene_file"` option that
#' you can set via the [orphatools_options()] interface. You can add an available option
#' with [add_associated_genes()].
#'
#' @return A data frame containing for each ORPHAcode the kind of association (if any).
#' with a related gene and all known information (symbol, name, references, locus) about the latter.
#'
#' @export
#' @seealso [orphatools_options()], [add_associated_genes()], [specify_code()]
#'
#' @examples
#' df_associated_genes = load_associated_genes()
#' df_genes_synonyms = load_genes_synonyms()
#'
#' @name load-genes

#' @rdname load-genes
#' @export
load_associated_genes = function(){
  version = getOption('gene_file_version', default_gene_version())
  extdata_path = system.file('extdata', package='orphatools')
  orpha_genes_path = file.path(extdata_path, 'gene_data', version, 'associated_genes.RDS')

  if(file.exists(orpha_genes_path))
    df_orpha_genes = readRDS(orpha_genes_path)
  else
    stop(simpleError(
    'Loading of associations failed. Internal files might be broken.
    See `orphatools_options`, `add_associated_genes` or consider reisntalling orphatools package.'))

  return(df_orpha_genes)
}


#' @rdname load-genes
#' @export
load_genes_synonyms = function(){
  version = getOption('gene_file_version', default_gene_version())
  extdata_path = system.file('extdata', package='orphatools')
  genes_synonyms_path = file.path(extdata_path, 'gene_data', version, 'genes_synonyms.RDS')

  if(file.exists(genes_synonyms_path))
    df_genes_synonyms = readRDS(genes_synonyms_path)
  else
    stop(simpleError(
    'Loading of associations failed. Internal files might be broken.
    See `orphatools_options` or consider reisntalling orphatools package.'))

  return(df_genes_synonyms)
}


#' Specify ORPHAcode with gene
#'
#' @description
#' Find a more precise ORPHAcode according to the provided mutated genes.
#'
#' Potential replacement ORPHAcodes will be searched among the descendants of the
#' given ORPHAcode.They must be associated to one of the mutated genes given in `genes`
#' argument. This kind of association is necessarily `"Assessed"` and `"Disease-causing"`
#' according to Orphanet.
#'
#' @details
#' Symbols (see [load_genes_synonyms()] to find them all) and HGNC codes are supported,
#' let the function know which you chose with the `mode` argument.
#'
#' Specification is applied if and only if a unique potential replacement ORPHAcode was found.
#' The function was first meant to be applied to a single ORPHAcode and a set of mutated genes
#' found in one individual. However you might want to apply function on data.frame columns
#' using `tidyverse` functions. This way your single ORPHAcode might be repeated over the full set of
#' mutated genes. The operation will then be row-wise and the duplicated ORPHAcodes will be considered
#' independently. Ths behavior makes a call on a `grouped_df` object unadvised.
#'
#' You have a couple of options to deal with it:
#'
#' - Set the `.by` argument instead of `group_by`.
#' - Use `group_by` to [chop()] mutated genes into a list-column and `ungroup` to make your data row-wise compatible.
#'
#'  See examples below.
#'
#' @param orpha_codes The ORPHAcodes to update. If length is greater than 1, the full set of genes will be applied to each vector element.
#' @param genes The mutated genes. If given as a list,
#' it should be length-compatible with `orpha_code`, so that each list element corresponds to one `orpha_codes` entry.
#' Set the `.by` argument properly to apply the right set of genes on each ORPHAcode.
#' @param mode Character constant, whether the given genes are `"symbol"` or `"HGNC"` codes.
#' @param .by Optionnaly, the set of mutated genes. The default is to consider each row as independent.
#' Set it to NULL or a constant value to apply the full set of `genes` to each element of `orpha_codes`.
#' A warning will be raised if any of the considered sets contains more than 10 elements.
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#'
#' @return The updated ORPHAcodes, the same length as `orpha_codes`.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Basic usage
#' orpha_code_cmt1 = 65753
#' orpha_code_cmtX = 64747
#'
#' ## Specification possible
#' specify_code(orpha_code_cmt1, 'MPZ', mode='symbol') # CMT1B is the lonely ORPHAcode both associated with CMT1 and MPZ
#' specify_code(orpha_code_cmt1, c('MPZ', 'POMT1'), mode='symbol') # CMT1B is the lonely ORPHAcode both associated with CMT1 and MPZ
#'
#' ## Specification impossible
#' specify_code(orpha_code_cmtX, 'MPZ', mode='symbol') # No ORPHAcode is associated both to CMTX and MPZ
#' specify_code(orpha_code_cmt1, 'PMP22', mode='symbol') # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
#' specify_code(orpha_code_cmt1, c('MPZ', 'PMP22'), mode='symbol') # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
#'
#' ## Alternatively with HGNC codes (the default mode)
#' specify_code(orpha_code_cmt1, 7225) # CMT1B is the lonely ORPHAcode both associated with CMT1 and MPZ
#' specify_code(orpha_code_cmt1, c(7225, 9202)) # CMT1B is the lonely ORPHAcode both associated with CMT1 and MPZ
#' specify_code(orpha_code_cmtX, 7225) # No ORPHAcode is associated both to CMTX and MPZ
#' specify_code(orpha_code_cmt1, 9118) # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
#' specify_code(orpha_code_cmt1, c(7225, 9118)) # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
#'
#' # Using dataframes
#' df = tibble(
#'  patient_id=c('A', 'A', 'B', 'C', 'D', 'D'),
#'  initial_orpha_code = c("65753", "65753", "903", "65753", "65753", "65753"), # CMT1 and von Willebrand
#   symbol = c("MPZ", "LITAF", "VWF", "LITAF", "MPZ", "VWF")) # MPZ, VWF and LITAF
#'
#' ## Basic call
#' df_spec = df %>% mutate(
#'   assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol'))
#'
#' ## Grouping may be preferable
#' df_spec = df %>% mutate(
#'   assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=))
#'
#' ## Equivalent method with genes in a list-column
#' df = tibble(
#'  patient_id=c('A', 'B', 'C', 'D'),
#'  initial_orpha_code = c("65753", "903", "65753", "65753"), # CMT1 and von Willebrand
#'   symbol = list(c("MPZ", "LITAF"), "VWF", "LITAF", c("MPZ", "VWF"))) # MPZ, VWF and LITAF
#'
#' df_spec = df %>% group_by(patient_id) %>% mutate(
#'   assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol'))
#'
specify_code = function(orpha_codes, genes=NULL, mode='HGNC', .by=1:length(orpha_codes)){
  choose_old_or_new = function(orpha_code, potential_codes){
    n_possibilities = n_distinct(potential_codes, na.rm=T)
    if(n_possibilities == 1)
      new_code = unique(na.omit(potential_codes))
    else
      new_code = orpha_code
  }

  # Void input
  if(length(orpha_codes)==0 || length(genes)==0)
    return(orpha_codes)

  # Input as a list is equivalent as grouped data
  if(is.list(genes)){
    new_codes = tibble(i=1:length(orpha_codes), orpha_code=orpha_codes, genes=genes, .by=.by) %>%
      unnest(any_of('genes')) %>%
      mutate(new_code = specify_code(orpha_code, genes, mode=mode, .by=.by)) %>%
      distinct(i, new_code) %>%
      pull(new_code)
    return(new_codes)
  }

  n_genes = tibble(gene=genes, .by=.by) %>% group_by(.by) %>% summarize(n_genes = n_distinct(genes)) %>% pull(n_genes)
  if(any(n_genes > 10))
    warning("One of the considerered set of genes is greater than 10.
            Please make sure you set the `.by` argument correctly.")

  # Find gene ids
  df_genes_synonyms = load_genes_synonyms() %>%
    filter(!synonyms %in% pref_symbol)

  # ...from symbols
  if(mode=='symbol'){
    # Given symbol might be a synonym or the preferential symbol
    df_add = df_genes_synonyms %>%
      mutate(synonyms = pref_symbol) %>%
      distinct()
    df_genes_synonyms_ext = bind_rows(df_add, df_genes_synonyms) %>%
      distinct()
    gene_ids = data.frame(symbols = genes) %>%
      left_join(df_genes_synonyms_ext, by=c('symbols'='synonyms')) %>%
      pull(gene_id)}

  # ...from HGNC codes
  else{
    gene_ids = data.frame(HGNC = as.character(genes)) %>%
      left_join(load_associated_genes() %>% distinct(gene_id, HGNC), by='HGNC', na_matches='never') %>%
      pull(gene_id)}

  # Cast ORPHAcodes
  orpha_codes = as.character(orpha_codes)

  # Get potential specifications (non-precise ORPHAcode -> precise ORPHAcode)
  df_prospects = load_associated_genes() %>%
    # Look for assessed associations between precise ORPHAcodes and genes
    filter(association_status == 'Assessed', str_detect(association_type, 'Disease-causing'), gene_id %in% gene_ids) %>%
    distinct(orpha_code, gene_id) %>%

    # Build relationships between given and found ORPHAcodes
    orpha_df(orpha_code_col = 'orpha_code', force_codes = unique(na.omit(orpha_codes))) %>%
    group_by(orpha_code, gene_id) %>%
    reframe(potential_codes = orpha_code) %>%
    ungroup() %>%
    filter(orpha_code %in% orpha_codes) %>%
    distinct(orpha_code, gene_id, potential_codes)

  # Check if there is a unique potential specification ORPHAcode
  new_codes = tibble(i=1:length(orpha_codes), orpha_code=orpha_codes, gene_id=gene_ids, .by=.by) %>%
    group_by(across(any_of('.by'))) %>%
    mutate(gene_id = list(gene_id)) %>%
    ungroup() %>%
    unnest(gene_id) %>%
    left_join(df_prospects, by=c('orpha_code', 'gene_id'), relationship='many-to-many') %>%
    group_by(across(any_of(c('.by', 'orpha_code')))) %>%
    mutate(new_code = choose_old_or_new(orpha_code, potential_codes)) %>%
    ungroup() %>%
    distinct(i, new_code) %>%
    pull(new_code)

  return(new_codes)
}


#' #' Return all genes associated to a given ORPHAcode with an assessed relationship.
#' #'
#' #' @param orpha_code The ORPHAcode from which associated genes should be extracted
#' #'
#' #' @return The associated genes symbols
#' #' @export
#' get_associated_genes = function(orpha_code){
#'   load_associated_genes() %>%
#'     filter(orpha_code == .env$orpha_code, associationStatus=='Assessed') %>%
#'     pull(hgnc_code) %>%
#'     return()
#' }
#'
#'
#' #' Return all genes associated to a given ORPHAcode with an assessed relationship.
#' #'
#' #' @param gene_symbol The gene symbol from which associated ORPHAcodes should be extracted
#' #'
#' #' @return The associated ORPHAcodes
#' #' @export
#' get_associated_orpha_codes = function(gene_symbol){
#'   # Search symbol among synonyms
#'   gene_pref_symbol = load_genes_synonyms() %>%
#'     filter(pref_symbol == gene_symbol | synonyms == gene_symbol) %>%
#'     pull(pref_symbol) %>%
#'     unique()
#'
#'   # Then find associated ORPHAcodes
#'   load_associated_genes() %>%
#'     filter(pref_symbol == gene_pref_symbol, associationStatus=='Assessed') %>%
#'     pull(orpha_code) %>%
#'     return()
#'
#' }
#'
#' #' Return all synonyms, including the preferential symbol, of a given gene symbol.
#' #'
#' #' @param gene_symbol The gene symbol from which synonyms should be found
#' #'
#' #' @return The given gene symbol with its synonyms.
#' #' Preferential gene symbol is given in first position.
#' #' @export
#' #'
#' #' @examples
#' get_gene_synonyms = function(gene_symbol){
#'   df_gene = load_genes_synonyms() %>%
#'     filter(pref_symbol == gene_symbol | synonyms == gene_symbol)
#'   synonyms = unique(c(df_gene$pref_symbol, df_gene$synonyms))
#'   return(synonyms, gene_symbol)
#' }
