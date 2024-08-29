get_genes_versions = function(){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_filepath = file.path(usr_dir, 'genes_versions.csv')
  if(!file.exists(versions_filepath))
    versions_filepath = system.file(package='orphatools', 'extdata', 'genes_versions.csv')
  df_versions = read.csv2(versions_filepath)
  return(df_versions)
}

check_genes_version = function(version){
  df_versions = get_genes_versions()
  return(any(version %in% df_versions$version))
}

set_default_genes_version = function(version){
  df_versions = get_genes_versions()
  df_versions$default = FALSE
  df_versions$default[df_versions$version==version] = TRUE
  save_genes_versions(df_versions)
}

save_genes_versions = function(df_versions){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  if(!file.exists(usr_dir))
    dir.create(usr_dir, recursive = TRUE)
  filepath = file.path(usr_dir, 'genes_versions.csv')
  write.csv2(df_versions, filepath, row.names=F)
}

save_genes = function(genes_data, version){
  usr_dir = tools::R_user_dir('orphatools', 'data')
  dest_dir = file.path(usr_dir, 'genes_data')
  if(!file.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  filename = paste0('genes_', version, '.rda')
  filepath = file.path(dest_dir, filename)
  save(genes_data, file=filepath)
}


#' Add ORPHAcodes-genes associations to `orphatools`.
#'
#' @description
#' This function analyzes the file containing associations between ORPHAcodes and genes
#' and saves them internally in an R-friendly format.
#'
#' @details
#' Orphanet publishes genes data on a 6-months basis, and is deployed in english only.
#' The different file versions are available \href{https://www.orphadata.com/genes/}{here}.
#' They must be downloaded locally to be added to `orphatools`.
#'
#' As a gene mutation may have various effects on individuals health, this file indicates how genes and
#' clinical entities are related, giving the association type and status. Genes can be referred through
#' multiple referentials including HGNC, OMIM, Ensembl, Genatlas, Reactome and SwissProt.
#'
#' The added file version will appear among the available options
#' through the [orphatools_options()] interface. It can also be manually set using the built-in
#' [options()] function and the `"gene_file_version"` name.
#'
#' @param filepath An xml file published by Orphanet containing the associations between ORPHAcodes and genes.
#' @param default If `TRUE`, set the added association file as default.
#' @param force If `TRUE`, adds the association file even if an identical version was internally found.
#' @param destdir The destination directory, in which the processed data will be saved.
#'
#' @import magrittr
#' @importFrom dplyr bind_rows distinct left_join mutate across
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split_1
#'
#' @export
#' @seealso [orphatools_options()], [load_associated_genes()], [load_genes_synonyms()]
add_associated_genes = function(filepath,
                                default=FALSE,
                                force=FALSE,
                                destdir=tools::R_user_dir('orphatools', 'data')){
  # Read file
  if(!file.exists(filepath))
    stop(simpleError('The given file was not found.'))

  # Load raw data from xml files
  message('Loading and processing genes associations file. This may take a few minutes.')
  genes_data_raw = read_xml(filepath, encoding = 'ISO-8859-1') %>% as_list()

  # Check version
  date = attr(genes_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
  if(!length(date))
    date = attr(genes_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
  lang = attr(genes_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
  version = sprintf('%s_%s', date, lang)
  if(check_genes_version(version)){
    if(force)
      warning(sprintf(
        'The genes associations file version (%s) already exists.
        It is going to be replaced.', version))
    else
      stop(simpleError(sprintf(
      'The genes associations file version (%s) already exists.
      Use force=TRUE to force installation.', version)))
  }
  else
    message(sprintf('Adding new genes associations file version (%s)', version))

  # Process
  genes_data = process_genes(genes_data_raw)

  # Save
  new_filename = paste0('genes_', version, '.rda')
  new_line = data.frame(version=version, location=file.path(destdir, 'genes_data', new_filename), default=FALSE)
  df_versions = get_genes_versions() %>% bind_rows(new_line) %>% distinct()
  save_genes_versions(df_versions)
  save_genes(genes_data, version)

  # Set default
  if(default)
    set_default_genes_version(version)

  message(sprintf('Genes associations file (%s) was succesfully added.', version))
  return(invisible(TRUE))
}


process_genes = function(genes_data_raw){
  adaptative_to_list = function(y){
    if(is.na(y)) return(NULL)
    else return(str_split_1(y, ','))
  }

  find_orpha_genes = function(disorder_node){
    props_df = data.frame(
      orpha_code = disorder_node$OrphaCode[[1]],

      gene_id = sapply(disorder_node$DisorderGeneAssociationList, \(x) attr(x$Gene, 'id')) %>% unname(),
      association_type = sapply(disorder_node$DisorderGeneAssociationList, function(x)  x$DisorderGeneAssociationType$Name[[1]]) %>% unname(),
      association_status = sapply(disorder_node$DisorderGeneAssociationList, function(x)  x$DisorderGeneAssociationStatus$Name[[1]]) %>% unname()
      )

    return(props_df)
  }

  find_gene_synonyms = function(gene_node){
    props_df = data.frame(
      gene_id = attr(gene_node$Gene, 'id'),
      n_syns = gene_node$Gene$SynonymList %>% attr('count') %>% as.numeric(),
      pref_symbol = gene_node$Gene$Symbol[[1]],
      name = gene_node$Gene$Name[[1]],
      synonyms = sapply(gene_node$Gene$SynonymList, \(x) x[[1]]) %>% unname()) %>%
      mutate(synonyms = ifelse(n_syns, synonyms, as.character(NA))) %>%
      select(-n_syns)
    return(props_df)
  }

  find_gene_extrefs = function(gene_node){
    if(attr(gene_node$Gene$ExternalReferenceList, 'count') != "0")
      props_df = data.frame(
        gene_id = attr(gene_node$Gene, 'id'),
        source = sapply(gene_node$Gene$ExternalReferenceList, \(x) x$Source[[1]]) %>% unname(),
        reference = sapply(gene_node$Gene$ExternalReferenceList, \(x) x$Reference[[1]]) %>% unname())
    else
      return(NULL)

    return(props_df)
  }

  find_gene_locus = function(gene_node){
    if(attr(gene_node$Gene$LocusList, 'count') != "0")
      props_df = data.frame(
        gene_id = attr(gene_node$Gene, 'id'),
        locus = sapply(gene_node$Gene$LocusList, \(x) x$GeneLocus[[1]]) %>% unname())
    else
      # return(NULL)
      props_df = data.frame(
        gene_id = attr(gene_node$Gene, 'id'),
        locus = as.character(NA))

    return(props_df)
  }

  # Preprocess data to make it R-manageable
  df_genes_synonyms = genes_data_raw[[1]][[2]] %>%
    lapply(\(disorder_node)
           lapply(disorder_node$DisorderGeneAssociationList, find_gene_synonyms) %>% bind_rows()) %>%
    bind_rows() %>%
    distinct()

  df_genes_extrefs = genes_data_raw[[1]][[2]] %>%
    lapply(\(disorder_node)
           lapply(disorder_node$DisorderGeneAssociationList, find_gene_extrefs) %>% bind_rows()) %>%
    bind_rows() %>%
    distinct()

  df_genes_locus = genes_data_raw[[1]][[2]] %>%
    lapply(\(disorder_node)
           lapply(disorder_node$DisorderGeneAssociationList, find_gene_locus) %>% bind_rows()) %>%
    bind_rows() %>%
    distinct()

  df_associated_genes = genes_data_raw[[1]][[2]] %>%
    lapply(find_orpha_genes) %>%
    bind_rows() %>%
    distinct() %>%
    # left_join(genes_synonyms %>% select(gene_id, pref_symbol, name) %>% distinct(), by='gene_id') %>%
    left_join(df_genes_extrefs %>% pivot_wider(names_from=source, values_from=reference, values_fn=\(x) paste0(x, collapse = ',')), by='gene_id') %>%
    mutate(across(c(OMIM, Reactome, SwissProt), ~ sapply(.x, adaptative_to_list, USE.NAMES = F))) %>%
    left_join(df_genes_locus, by='gene_id')

  return(list(
    'associations' = df_associated_genes,
    'synonyms' = df_genes_synonyms
  ))
}
