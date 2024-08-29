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
#' @importFrom stats setNames
#' @export
#' @seealso [add_dictionary()], [add_nomenclature_pack()], [add_associated_genes()]
#' @examples
#' \dontrun{orphatools_options()}
orphatools_options = function(){
  # Show current options
  cat('Current options:\n\n')
  cat(paste0('orphatools_dict: ', getOption('orphatools_dict'), '\n'))
  cat(paste0('orphatools_nomenclature: ', getOption('orphatools_nomenclature'), '\n'))
  cat(paste0('orphatools_gene_file: ', getOption('orphatools_gene_file'), '\n'))
  cat('\n')

  # Choose an option to set
  cat('Which type of option would you like to set ?\n\n')
  cat('1. Dictionary\n')
  cat('2. Nomenclature pack\n')
  cat('3. Associated genes file\n')
  cat('\n')
  cat('Enter the corresponding option type number or an empty line to skip: ')
  choice = readLines(con=getOption('orphatools.connection'), n=1) %>% as.numeric()
  cat('\n')

  # Analyze corresponding options
  if(is.na(choice))
    return(invisible(NULL))
  if(choice == 1){
    option_type = 'orphatools_dict'
    all_options = get_dict_versions() %>% pull(version)
    current_option = getOption(option_type, default=default_dict())
    default_option = default_dict()
  }
  else if(choice == 2){
    option_type = 'orphatools_nomenclature'
    all_options = get_pack_versions() %>% pull(version)
    current_option = getOption(option_type, default=default_pack_version())
    default_option = default_pack_version()
  }
  else if(choice == 3){
    option_type = 'orphatools_gene_file'
    all_options = get_genes_versions() %>% pull(version)
    current_option = getOption(option_type, default=default_genes_version())
    default_option = default_genes_version()
  }
  else
    stop(simpleError('Invalid choice.'))

  # Choose option
  df_options = data.frame(option=all_options) %>%
      mutate(disp_option = case_when(
        option==current_option & option==default_option ~ paste0(option, ' (active)(default)'),
        option==current_option ~ paste0(option, ' (active)'),
        option==default_option ~ paste0(option, ' (default)'),
        TRUE ~ option
      ))

  cat('Choose new option:\n\n')
  for(i in 1:nrow(df_options))
    cat(sprintf('%d. %s\n', i, df_options$disp_option[i]))
  cat('\n')
  cat('Enter the corresponding option number or an empty line to skip: ')
  choice = readLines(con=getOption('orphatools.connection'), n=1) %>% as.numeric()
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
  dict = get_dict_versions() %>% filter(default) %>% pull(version)

  if(length(dict)==0)
    stop(simpleError('No dictionary was found. See `add_dictionary`.'))

  if(is.null(getOption('orphatools_dict')))
    options('orphatools_dict'=dict)

  return(dict)
}


default_pack_version = function(){
  version = get_pack_versions() %>% filter(default) %>% pull(version)

  if(length(version)==0)
    stop(simpleError('No nomenclature pack was added. See `add_nomenclature_pack`.'))

  # Set as active if the option hasn't been set yet
  if(is.null(getOption('orphatools_nomenclature')))
    options('orphatools_nomenclature'=version)
  return(version)
}


default_genes_version = function(){
  version = get_genes_versions() %>% filter(default) %>% pull(version)

  if(length(version)==0)
    stop('No gene file was added. See `add_associated_genes`.')

  # Set as active if the option hasn't been set yet
  if(is.null(getOption('orphatools_gene_file')))
    options('orphatools_gene_file'=version)

  return(version)
}
