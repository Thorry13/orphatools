get_pack_versions = function(){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_filepath = file.path(usr_dir, 'pack_versions.csv')
  if(!file.exists(versions_filepath))
    versions_filepath = system.file(package='orphatools', 'extdata', 'pack_versions.csv')
  df_versions = read.csv2(versions_filepath)
  return(df_versions)
}

check_pack_version = function(version){
  df_versions = get_pack_versions()
  return(any(version %in% df_versions$version))
}

set_default_pack_version = function(version){
  df_versions = get_pack_versions()
  df_versions$default = FALSE
  df_versions$default[df_versions$version==version] = TRUE
  save_pack_versions(df_versions)
}

save_pack_versions = function(df_versions){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  if(!file.exists(usr_dir))
    dir.create(usr_dir, recursive = TRUE)
  filepath = file.path(usr_dir, 'pack_versions.csv')
  write.csv2(df_versions, filepath, row.names=F)
}

save_pack = function(pack_data, version){
  usr_dir = tools::R_user_dir('orphatools', 'data')
  dest_dir = file.path(usr_dir, 'pack_data')
  if(!file.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  filename = paste0('pack_', version, '.rda')
  filepath = file.path(dest_dir, filename)
  save(pack_data, file=filepath)
}


#' Add a nomenclature pack to `orphatools`
#'
#' @description
#' This function analyzes the nomenclature pack files and saves them internally in an R-friendly format.
#'
#' @details
#' Orphanet publishes the nomenclature pack on a yearly basis, and is deployed in different language versions.
#' The different nomenclature pack versions are available \href{https://www.orphadata.com/pack-nomenclature/}{here}.
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
#' @param zip_filepath The location of the _.zip_ file containing the nomenclature pack.
#' @param default If `TRUE`, set the added pack as default.
#' @param force If `TRUE`, adds the pack even if an identical version was internally found.
#' @param destdir The destination directory, in which the processed data will be saved.
#'
#' @import magrittr
#' @importFrom xml2 read_xml as_list xml_find_all xml_find_first xml_text
#' @importFrom stringr str_detect str_ends
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @seealso [orphatools_options()] to switch from a pack to another,
#'  [load_nomenclature()], [load_raw_nomenclature()],
#'  [load_classifications()], [load_synonyms()], [load_redirections()]
#'  to load the data added through this function.
#'
add_nomenclature_pack = function(zip_filepath,
                                 default=FALSE,
                                 force=FALSE,
                                 destdir=tools::R_user_dir('orphatools', 'data')
                                 ){
  if(!file.exists(zip_filepath))
    stop(simpleError('The given file was not found.'))

  # Unzip
  zip_filename = basename(zip_filepath)
  unzipdir = file.path(tempdir(), zip_filename)
  n_classif_files = utils::unzip(zip_filepath, junkpaths = TRUE, list=TRUE) %>%
    filter(str_detect(Name, 'ORPHAclassification')) %>% nrow()
  utils::unzip(zip_filepath, junkpaths = TRUE, exdir=unzipdir)

  # Detect files
  unzipped_files = list.files(unzipdir)
  nomenclature_file = unzipped_files %>%
    Filter(\(x) str_detect(x, 'ORPHAnomenclature') & str_ends(x, '.xml'),.)
  classif_files = unzipped_files %>%
    Filter(\(x) str_detect(x, 'ORPHAclassification'), .)
  if(length(nomenclature_file) != 1 | length(classif_files) != n_classif_files)
    stop(simpleError('The given file does not have the right format for `orphatools`.'))
  else
    message('Loading and processing nomenclature pack. This may take a few minutes.')

  # Read nomenclature file
  nomenclature_data_raw =
    read_xml(file.path(unzipdir, nomenclature_file),
             encoding = 'ISO-8859-1') %>%
    as_list()

  # Check version
  date = attr(nomenclature_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
  if(!length(date))
    date = attr(nomenclature_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
  lang = attr(nomenclature_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
  version = sprintf('%s_%s', date, lang)
  if(check_pack_version(version)){
    if(force)
      warning(sprintf('The nomenclature pack version (%s) already exists. It is going to be replaced.', version))
    else
      stop(simpleError(sprintf(
      'The nomenclature pack version (%s) already exists.
      Use force=TRUE to force installation.', version)))
  }
  else
    message(sprintf('Adding new nomenclature pack version (%s)', version))

  # Process
  nomenclature_data = process_nomenclature(nomenclature_data_raw)
  classif_data = process_classif(file.path(unzipdir, classif_files))

  # Save
  new_filename = paste0('pack_', version, '.rda')
  new_line = data.frame(version=version, location=file.path(destdir, 'pack_data', new_filename), default=FALSE)
  df_versions = get_pack_versions() %>% bind_rows(new_line) %>% distinct() # does not work for internal...?
  save_pack_versions(df_versions)
  save_pack(c(nomenclature_data, classif_data), version)

  # Set default
  if(default){
    set_default_pack_version(version)
    # options('orphatools.pack'=version)
  }

  message(sprintf('Nomenclature pack (%s) was succesfully added.', version))

  # Clean tempdir
  file.remove(list.files(unzipdir, full.names = TRUE))

  return(invisible(TRUE))
}


process_nomenclature = function(nomenclature_data_raw){
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
    find_target = function(x){
      target = x$TargetDisorder$OrphaCode[[1]]
      if(is.null(target))
        target = as.character(NA)
      return(target)
    }

    ignore_flag_values = c('1', '129', '513', '8225') # active and non rare flags
    if(disorder_node$FlagValue[[1]] %in% ignore_flag_values | attr(disorder_node$DisorderDisorderAssociationList, 'count') == "0"){
      return(NULL)
    }
    else{
      df_redirections = data.frame(
        from = disorder_node$OrphaCode[[1]],
        to = sapply(disorder_node$DisorderDisorderAssociationList, find_target) %>% unname() %>% unlist(),
        redir_type = sapply(disorder_node$DisorderDisorderAssociationList, \(x) attr(x$DisorderDisorderAssociationType, 'id')) %>% unname() %>% unique()
      ) %>%
        filter(!is.na(to))
      return(df_redirections)
    }
  }

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

  return(list(
    'nomenclature' = df_nomenclature,
    'synonyms' = df_synonyms,
    'redirections' = df_redirections
  ))
}


process_classif = function(classification_files){
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

  # Find classifications
  classif_names = classification_files %>% basename()
  classif_data = classification_files %>%
    lapply(function(classif_file) read_classif(classif_file)) %>%
    stats::setNames(classif_names %>% str_remove('ORPHAclassification_') %>% sapply(tools::file_path_sans_ext))

  return(list('classifications'=classif_data))
}
