#' Update the Orpha nomenclature pack. Orphanet updates its pack on a yearly basis.
#'
#' @param orpha_pack_nomenclature_path the path to the uncompressed nomenclature pack,
#' available here: https://www.orphadata.com/pack-nomenclature/
#' @param nomenclature_xml_file nomenclature file is supposed to be _ORPHAnomenclature_fr.xml_, but might differ.
#' @param classifications_folder classifications folder is supposed to be _Classifications_fr_, but might differ
#'
#' @import magrittr
#' @importFrom xml2 read_xml as_list
#' @importFrom dplyr bind_rows
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

  find_associations = function(disorder_node){
    active_flag_values = c('1', '129', '513')
    if(disorder_node$FlagValue[[1]] %in% active_flag_values | attr(disorder_node$DisorderDisorderAssociationList, 'count') == "0"){
      return(NULL)
    }
    else{
      df_association = data.frame(
        orphaCode = disorder_node$OrphaCode[[1]],
        associationType = sapply(disorder_node$DisorderDisorderAssociationList, \(x) attr(x$DisorderDisorderAssociationType, 'id')) %>% unname(),
        associatedDisorder = sapply(disorder_node$DisorderDisorderAssociationList, \(x) x$TargetDisorder$OrphaCode[[1]]) %>% unname()
      )
      return(df_association)
    }
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

    df_associations = nomenclature_data_raw[[1]][[2]] %>%
      lapply(find_associations) %>%
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
    saveRDS(df_associations, file.path(extdata_path, 'associations.RDS'))
  }
  else
    stop(simpleError('Invalid parameters.'))

  message('Nomenclature pack was succesfully updated.')
}



#' Read class returns a from/to dataframe associated to a classification
#'
#' @param path Path to classification file
#'
#' @return A from/to dataframe representing the edges between the classification entities
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom xml2 read_xml xml_find_first xml_find_all
#' @export
read_class = function(path)
{
  class_data = read_xml(path)

  # Find classification nodes
  classification_nodes = xml_find_all(class_data, '//ClassificationNode')
  df_class = classification_nodes %>%
    lapply( function(class_node){
      from = class_node %>% xml_find_first('Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
      to = class_node %>% xml_find_all('ClassificationNodeChildList/ClassificationNode/Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
      df_temp = NULL
      if(length(to) != 0)
        df_temp = list(from=from, to=to)
      return(df_temp)
    }) %>%
    bind_rows() %>%
    distinct() %>%
    as.data.frame()
}



#' Update genes information from orphanet public data,
#' available here: https://www.orphadata.com/genes/
#'
#' @param genes_file An xml file published by Orphanet
#'
#' @import magrittr
#' @importFrom dplyr bind_rows distinct left_join mutate across
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split_1
#'
#'
#' @export
update_genes = function(genes_file){
  adaptative_to_list = function(y){
    if(is.na(y)) return(NULL)
    else return(str_split_1(y, ','))
  }

  find_orpha_genes = function(disorder_node){
    props_df = data.frame(
      orphaCode = disorder_node$OrphaCode[[1]],

      gene_id = sapply(disorder_node$DisorderGeneAssociationList, \(x) attr(x$Gene, 'id')) %>% unname(),
      associationType = sapply(disorder_node$DisorderGeneAssociationList, function(x)  x$DisorderGeneAssociationType$Name[[1]]) %>% unname(),
      associationStatus = sapply(disorder_node$DisorderGeneAssociationList, function(x)  x$DisorderGeneAssociationStatus$Name[[1]]) %>% unname()
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

  if(file.exists(genes_file)){
    # Load raw data from xml files
    genes_data_raw = read_xml(genes_file, encoding = 'ISO-8859-1') %>% as_list()

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

    orpha_genes = genes_data_raw[[1]][[2]] %>%
      lapply(find_orpha_genes) %>%
      bind_rows() %>%
      distinct() %>%
      left_join(genes_synonyms %>% select(gene_id, pref_symbol, name) %>% distinct(), by='gene_id') %>%
      left_join(genes_extrefs %>% pivot_wider(names_from=source, values_from=reference, values_fn=\(x) paste0(x, collapse = ',')), by='gene_id') %>%
      mutate(across(c(OMIM, Reactome, SwissProt), ~ sapply(.x, adaptative_to_list, USE.NAMES = F))) %>%
      left_join(genes_locus, by='gene_id')

    # Save changes
    extdata_path = system.file('extdata', package='orphatools')
    saveRDS(orpha_genes, file.path(extdata_path, 'orpha_genes.RDS'))
    saveRDS(genes_synonyms, file.path(extdata_path, 'genes_synonyms.RDS'))
  }
  else
    stop(simpleError('Given file is invalid.'))
}
