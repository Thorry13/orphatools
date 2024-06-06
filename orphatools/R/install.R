#' Add a dictionary
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' A dictionary is used to interprete Orphanet concepts, given as ids.
#' Copy the dictionary template then add it to the package after modification.
#'
#' Once added, the dictionary will appear among the available options
#' through the [orphatools_options()] interface. It can also be manually set using the built-in
#' [options()] function and the `"nomenclature_version"` name.
#'
#'
#' @name add-dictionary

#' @rdname add-dictionary
#' @param dest_path The destination path where the dictionary template should be copied.
#' @export
#' @seealso [orphatools_options()]
copy_dict_template = function(dest_path='.'){
  extdata_path = system.file('extdata', package='orphatools')
  file.copy(file.path(extdata_path, 'dict_template.csv'), dest_path)
}

#' @rdname add-dictionary
#' @param dict_path The path to the dictionary (*.csv*) to add.
#' @param set_active if `TRUE`, the added dictionary becomes the active dictionary.
#'
#' @importFrom stringr str_remove
#' @export
add_dictionary = function(dict_path, set_active=TRUE){
  filename = basename(dict_path)
  version = filename %>% str_remove('.csv')

  extdata_path = system.file('extdata', package='orphatools')
  new_path = file.path(extdata_path, 'dicts', filename)

  if(file.exists(dicts_dir) & !force)
    stop(simpleError('The nomenclature pack version already exists. Use force=TRUE to force installation.'))

  # Save
  file.copy(dict_path, new_path)

  # Set as the active pack
  if(set_active)
    options('orphatools_dict'=version)

  message(sprintf('Dictionary (%s) was succesfully added.', version))
}


#' Add a nomenclature pack to `orphatools`
#'
#' @description
#' This function analyzes the nomenclature pack files and saves them internally in an R-friendly format.
#'
#' @details
#' Orphanet publishes the nomenclature pack on a yearly basis, and is deployed in different language versions.
#' The different nomenclature pack versions are available at https://www.orphadata.com/pack-nomenclature/.
#' They must be downloaded locally and uncompressed to be added to `orphatools`.
#'
#' The uncompressed Orphanet nomenclature pack contains a set of *.xml* and *.xlsx* files for coding, including the nomenclature file
#' (e.g. *ORPHAnomenclature_fr_2023.xml*) and a set of more than 30 classifications (usually contained in *Classifications* folder).
#'
#' The nomenclature file contains for each ORPHAcode the associated properties,
#' like their associated label (and synonyms), status (active or not),
#' classification levels (group of disorder, disorder or subtype of disorder),
#' and redirections to another ORPHAcode (for deprecated or obsolete only).
#'
#' Each classification contains numerous from/to relationships between clinical entities,
#' depicting the global Orphanet classification system.
#'
#' Once added, the nomenclature pack will appear among the available options
#' through the [orphatools_options()] interface. It can also be manually set using the built-in
#' [options()] function and the `"nomenclature_version"` name.
#'
#' @param orpha_pack_nomenclature_path The path to the uncompressed nomenclature pack.
#' @param nomenclature_xml_file The name of the nomenclature file (*.xml*).
#' @param classifications_folder The name of the folder containing the 34 *.xml* classification files.
#' @param set_active if `TRUE`, the added pack becomes the active pack.
#' @param force if `TRUE`, adds the pack even if an identical version was internally found.
#'
#' @import magrittr
#' @importFrom xml2 read_xml as_list xml_find_all xml_find_first xml_text
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @seealso [orphatools_options()] to switch from a pack to another,
#'  [load_nomenclature()], [load_raw_nomenclature()],
#'  [load_classifications()], [load_synonyms()], [load_redirections()]
#'  to load the data added through this function.
#'
add_nomenclature_pack = function(orpha_pack_nomenclature_path,
                                 nomenclature_xml_file,
                                 classifications_folder,
                                 set_active=TRUE,
                                 force=FALSE){
  read_nomenclature = function(disorder_node){
    props_df = data.frame(
      orpha_code = disorder_node$OrphaCode[[1]],
      label = disorder_node$Name[[1]],
      level = disorder_node$ClassificationLevel %>% attr('id'),
      status = disorder_node$FlagValue[[1]],
      type = disorder_node$DisorderType %>% attr('id')
    )
    return(props_df)
  }

  read_classif = function(path)
  {
    class_data = read_xml(path)

    # Find classification nodes
    classification_nodes = xml_find_all(class_data, '//ClassificationNode')
    df_class = classification_nodes %>%
      lapply( function(class_node){
        from = class_node %>% xml_find_first('Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
        to = class_node %>% xml_find_all('ClassificationNodeChildList/ClassificationNode/Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
        tmp = NULL
        if(!is.na(from) & length(to) > 0)
          tmp = list(from=from, to=to)
        return(tmp)
      }) %>%
      bind_rows() %>%
      distinct() %>%
      as.data.frame()
  }

  find_synonyms = function(disorder_node){
    df_synonyms = data.frame(
      orpha_code = disorder_node$OrphaCode[[1]],
      pref_label = disorder_node$Name[[1]],
      count = disorder_node$SynonymList %>% attr('count') %>% as.numeric(),
      synonyms =
          sapply(disorder_node$SynonymList,
                 function(x)  x[[1]]) %>%
          unname()
      ) %>%
      mutate(synonyms = ifelse(count, synonyms, as.character(NA))) %>%
      select(-count)
  }

  find_redirections = function(disorder_node){
    ignore_flag_values = c('1', '129', '513', '8225') # active and non rare flags
    if(disorder_node$FlagValue[[1]] %in% ignore_flag_values | attr(disorder_node$DisorderDisorderAssociationList, 'count') == "0"){
      return(NULL)
    }
    else{
      df_redirections = data.frame(
        from = disorder_node$OrphaCode[[1]],
        to = sapply(disorder_node$DisorderDisorderAssociationList, \(x) x$TargetDisorder$OrphaCode[[1]]) %>% unname(),
        redir_type = sapply(disorder_node$DisorderDisorderAssociationList, \(x) attr(x$DisorderDisorderAssociationType, 'id')) %>% unname()
      )
      return(df_redirections)
    }
  }

  nomenclature_path = file.path(orpha_pack_nomenclature_path, nomenclature_xml_file)
  classifications_path = file.path(orpha_pack_nomenclature_path, classifications_folder)
  if(file.exists(nomenclature_path) & file.exists(classifications_path)){
    message('Loading and processing nomenclature pack. This may take a few minutes.')

    # Load raw data from xml files
    nomenclature_data_raw = read_xml(nomenclature_path, encoding = 'ISO-8859-1') %>% as_list()

    # Analyze version
    date = attr(nomenclature_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
    if(!length(date))
      date = attr(nomenclature_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
    lang = attr(nomenclature_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
    version = sprintf('%s_%s', date, lang)
    extdata_path = system.file('extdata', package='orphatools')
    version_path = file.path(extdata_path, 'nom_data', version)
    if(!dir.exists(version_path))
      dir.create(version_path, recursive = TRUE)
    else if(!force)
      stop(simpleError('The nomenclature pack version already exists. Use force=TRUE to force installation.'))

    message(sprintf('Adding new nomenclature pack version: %s', version))
    # Preprocess data to make it R-manageable
    df_nomenclature = nomenclature_data_raw[[1]][[2]] %>%
      lapply(read_nomenclature) %>%
      bind_rows()

    df_synonyms = nomenclature_data_raw[[1]][[2]] %>%
      lapply(find_synonyms) %>%
      bind_rows()

    df_redirections = nomenclature_data_raw[[1]][[2]] %>%
      lapply(find_redirections) %>%
      bind_rows()

    rm(nomenclature_data_raw)

    # Find classifications
    class_list = list.files(classifications_path) %>% as.list()
    all_class = class_list %>%
      lapply(function(class_file) read_classif(file.path(classifications_path, class_file))) %>%
      setNames(class_list %>% sapply(tools::file_path_sans_ext))

    # Save
    saveRDS(df_nomenclature, file.path(version_path, 'nomenclature.RDS'))
    saveRDS(df_synonyms, file.path(version_path, 'synonyms.RDS'))
    saveRDS(all_class, file.path(version_path, 'classifications.RDS'))
    saveRDS(df_redirections, file.path(version_path, 'redirections.RDS'))

    # Set as the active pack
    if(set_active)
      options('orphatools_nomenclature'=version)
  }
  else
    stop(simpleError('Invalid parameters.'))

  message(sprintf('Nomenclature pack (%s) was succesfully added.', version))
}


#' Add ORPHAcodes-genes associations to `orphatools`.
#'
#' @description
#' This function analyzes the file containing associations between ORPHAcodes and genes
#' and saves them internally in an R-friendly format.
#'
#' @details
#' Orphanet publishes the nomenclature pack on a 6-months basis, and is deployed in english only.
#' The different file versions are available at https://www.orphadata.com/genes/.
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
#' @param filepath An xml file published by Orphanet.
#' @param set_active if `TRUE`, the added file becomes the active file.
#'
#' @import magrittr
#' @importFrom dplyr bind_rows distinct left_join mutate across
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split_1
#'
#' @export
#' @seealso [orphatools_options()], [load_associated_genes()], [load_genes_synonyms()]
add_associated_genes = function(filepath, set_active=TRUE){
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

  if(file.exists(filepath)){
    # Load raw data from xml files
    genes_data_raw = read_xml(filepath, encoding = 'ISO-8859-1') %>% as_list()

    # Analyze version
    date = attr(genes_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
    if(!length(date))
      date = attr(genes_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
    lang = attr(genes_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
    version = sprintf('%s_%s', date, lang)
    extdata_path = system.file('extdata', package='orphatools')
    version_path = file.path(extdata_path, 'gene_data', version)
    if(!dir.exists(version_path))
      dir.create(version_path, recursive = TRUE)
    else if(!force)
      stop(simpleError('The gene fie version already exists. Use force=TRUE to force installation.'))

    message(sprintf('Adding new file for associated genes: %s', version))
    # Preprocess data to make it R-manageable
    genes_synonyms = genes_data_raw[[1]][[2]] %>%
      lapply(\(disorder_node)
             lapply(disorder_node$DisorderGeneAssociationList, find_gene_synonyms) %>% bind_rows()) %>%
      bind_rows() %>%
      distinct()

    genes_extrefs = genes_data_raw[[1]][[2]] %>%
      lapply(\(disorder_node)
             lapply(disorder_node$DisorderGeneAssociationList, find_gene_extrefs) %>% bind_rows()) %>%
      bind_rows() %>%
      distinct()

    genes_locus = genes_data_raw[[1]][[2]] %>%
      lapply(\(disorder_node)
             lapply(disorder_node$DisorderGeneAssociationList, find_gene_locus) %>% bind_rows()) %>%
      bind_rows() %>%
      distinct()

    df_associated_genes = genes_data_raw[[1]][[2]] %>%
      lapply(find_orpha_genes) %>%
      bind_rows() %>%
      distinct() %>%
      left_join(genes_synonyms %>% select(gene_id, pref_symbol, name) %>% distinct(), by='gene_id') %>%
      left_join(genes_extrefs %>% pivot_wider(names_from=source, values_from=reference, values_fn=\(x) paste0(x, collapse = ',')), by='gene_id') %>%
      mutate(across(c(OMIM, Reactome, SwissProt), ~ sapply(.x, adaptative_to_list, USE.NAMES = F))) %>%
      left_join(genes_locus, by='gene_id')

    # Save
    saveRDS(df_associated_genes, file.path(version_path, 'associated_genes.RDS'))
    saveRDS(genes_synonyms, file.path(version_path, 'genes_synonyms.RDS'))

    # Set as the active file
    if(set_active)
      options('orphatools_gene_file'=version)
  }
  else
    stop(simpleError('Invalid parameters.'))

  message(sprintf('Gene file (%s) was succesfully added.', version))
}
