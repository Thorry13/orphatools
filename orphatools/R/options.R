#' Options for `orphatools`
#'
#' @description
#' Interface to list and set orphatools-related options.
#'
#' Options are useful to roll back to older versions or to switch between languages for display.
#'
#' - `"orphatools_dict"` refers to the dictionary used to interprete Orphanet concepts ids.
#'
#' - `"orphatools_nomenclature"` refers to a nomenclature pack version (extraction date and used language).
#'
#' - `"orphatools_gene_file"` refers to the version of the file used to associated genes to ORPHAcodes.
#'
#' All options can be set manually via the built-in [options()] function.
#'
#' @export
#' @seealso [add_dictionary()], [add_nomenclature_pack()], [add_associated_genes()]
#' @examples
#' \dontrun{orphatools_options()}
orphatools_options = function(){
  extdata_path = system.file('extdata', package='orphatools')

  # Show current options
  cat('Current options:\n\n')
  cat(sprintf('orphatools_dict: %s\n', getOption('orphatools_dict')))
  cat(sprintf('orphatools_nomenclature: %s\n', getOption('orphatools_nomenclature')))
  cat(sprintf('orphatools_gene_file: %s\n', getOption('orphatools_gene_file')))
  cat('\n')

  # Choose an option to set
  cat('Which type of option would you like to set ?\n\n')
  cat('1. Dictionary\n')
  cat('2. Nomenclature pack\n')
  cat('3. Associated genes file\n')
  cat('\n')
  choice = readline('Enter the corresponding option type number or an empty line to skip: ') %>% as.numeric()
  cat('\n')

  # Analyze corresponding options
  if(is.na(choice))
    return(invisible(NULL))
  if(choice == 1){
    option_type = 'orphatools_dict'
    all_options = list.files(file.path(extdata_path, 'dicts'), full.names = FALSE, recursive = FALSE) %>%
      str_remove('.csv')
    current_option = getOption(option_type, default=default_dict())
  }
  else if(choice == 2){
    option_type = 'orphatools_nomenclature'
    all_options = list.dirs(file.path(extdata_path, 'nom_data'), full.names = FALSE, recursive = FALSE)
    current_option = getOption(option_type, default=default_nom_version())
  }
  else if(choice == 3){
    option_type = 'orphatools_gene_file'
    all_options = list.dirs(file.path(extdata_path, 'gene_data'), full.names = FALSE, recursive = FALSE)
    current_option = getOption(option_type, default=default_gene_version())
  }
  else
    stop(simpleError('Invalide choice.'))

  # Choose option
  df_options = data.frame(option=all_options) %>%
      mutate(disp_option = case_when(
        option==current_option ~ paste0(option, ' (active)'),
        TRUE ~ option
      ))

  cat('Choose new option:\n\n')
  for(i in 1:nrow(df_options))
    cat(sprintf('%d. %s\n', i, df_options$disp_option[i]))
  cat('\n')
  choice = readline('Enter the corresponding option number or an empty line to skip: ') %>% as.numeric()
  cat('\n')

  # Apply option
  if(is.na(choice))
    return(invisible(NULL))
  else if(!choice %in% 1:nrow(df_options))
    stop('Invalid choice.')
  else
    options(as.list(df_options$option[choice] %>% setNames(option_type)))

  message(sprintf('%s option was set to %s', option_type, df_options$option[choice]))
}


#' @importFrom stringr str_remove
default_dict = function(){
  extdata_path = system.file('extdata', package='orphatools')
  dicts = list.files(file.path(extdata_path, 'dicts'), full.names = F, recursive = F) %>%
    str_remove('.csv')

  if(length(dicts)==0)
    stop(simpleError('No dictionary was found. See `add_dictionary`.'))

  # Set english dictionary as active if the option hasn't been set yet
  if('dict_en' %in% dicts)
    dict = 'dict_en'
  else
    dict = dicts[1]

  if(is.null(getOption('orphatools_dict')))
    options('orphatools_dict'=dict)

  return(dict)
}


default_nom_version = function(){
  extdata_path = system.file('extdata', package='orphatools')
  versions = list.dirs(file.path(extdata_path, 'nom_data'), full.names = F, recursive = F)

  if(length(versions)==0)
    stop(simpleError('No nomenclature pack was added. See `add_nomenclature_pack`.'))

  # Set as active if the option hasn't been set yet
  if(is.null(getOption('orphatools_nomenclature')))
    options('orphatools_nomenclature'=max(versions))
  return(max(versions))
}


default_gene_version = function(){
  extdata_path = system.file('extdata', package='orphatools')
  versions = list.dirs(file.path(extdata_path, 'gene_data'), full.names = F, recursive = F)

  if(length(versions)==0)
    stop('No gene file was added. See `add_associated_genes`.')

  # Set as active if the option hasn't been set yet
  if(is.null(getOption('orphatools_gene_file')))
    options('orphatools_gene_file'=max(versions))
  return(max(versions))
}
