get_dict_versions = function(){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_filepath = file.path(usr_dir, 'dict_versions.csv')
  if(!file.exists(versions_filepath))
    versions_filepath = system.file(package='orphatools', 'extdata', 'dict_versions.csv')
  df_versions = read.csv2(versions_filepath)
  return(df_versions)
}

check_dict_version = function(version){
  df_versions = get_dict_versions()
  return(any(version %in% df_versions$version))
}

set_default_dict_version = function(version){
  df_versions = get_dict_versions()
  df_versions$default = FALSE
  df_versions$default[df_versions$version==version] = TRUE
  save_dict_versions(df_versions)
}

save_dict_versions = function(df_versions){
  usr_dir = tools::R_user_dir('orphatools', 'config')
  if(!file.exists(usr_dir))
    dir.create(usr_dir, recursive = TRUE)
  filepath = file.path(usr_dir, 'dict_versions.csv')
  utils::write.csv2(df_versions, filepath, row.names=F)
}

save_dict = function(dict_data, version){
  usr_dir = tools::R_user_dir('orphatools', 'data')
  dest_dir = file.path(usr_dir, 'dict_data')
  if(!file.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  filename = paste0(version, '.csv')
  filepath = file.path(dest_dir, filename)
  utils::write.csv2(dict_data, file=filepath, row.names = FALSE)
}


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

#' @param filepath The location of the _.csv_ file containing the labels in another language.
#' A template of such a dicitonary can be saved using `copy_dict_template`.
#' @param default If `TRUE`, set the added dictionary as default.
#' @param destdir The destination directory, in which the processed data will be saved.
#'
#' @rdname add-dictionary
#' @importFrom stringr str_remove
#' @importFrom utils read.csv2
#' @export
add_dictionary = function(filepath,
                          default=FALSE,
                          destdir=tools::R_user_dir('orphatools', 'data')){
  # Read file
  if(!file.exists(filepath))
    stop(simpleError('The given file was not found.'))

  dict_data = read.csv2(filepath)
  filename = basename(filepath)
  version = filename %>% str_remove('.csv')

  if(check_dict_version(version))
    stop(simpleError(sprintf(
    'The dictionary version (%s) already exists.
    You can rename it to add it to the dictionary collection.', version)))
  else
    message(sprintf('Adding new dictionary version (%s)', version))

  # Save
  new_line = data.frame(version=version, location=file.path(destdir, 'dict_data', filename), default=FALSE)
  df_versions = get_dict_versions() %>% bind_rows(new_line) %>% distinct()
  save_dict_versions(df_versions)
  save_dict(dict_data, version)

  # Set as default
  if(default)
    options('orphatools_dict'=version)

  message(sprintf('Dictionary (%s) was succesfully added.', version))

  return(invisible(TRUE))
}
