#' Redirections
#'
#' @description
#' Load for each obsolete or deprecated ORPHAcode the corresponding association (*Moved to* or *Referred to*).
#'
#' While `load_raw_redirections` keep the original id values for each Orphanet concept,
#' `load_redirections` translate them according to `"orphatools_dict"` option, which can be set manually using
#' built-in [options()] function or through the [orphatools_options()] interface.
#'
#' @return A data.frame object giving the deprecated or obsolete ORPHAcodes,
#' the corresponding associations with the association type
#' @export
#' @seealso [redirect_code()]
#'
#' @examples
#' df_redirections = load_raw_redirections()
#' df_redirections = load_redirections()
load_redirections = function(){
  df_redirections = load_raw_redirections() %>% translate_orpha_concepts()
  return(df_redirections)
}


#' @rdname load_redirections
#' @export
load_raw_redirections = function(){
  version = getOption('nomenclature_version', default_nom_version())
  extdata_path = system.file('extdata', package='orphatools')
  redirections_path = file.path(extdata_path, 'nom_data', version, 'redirections.RDS')

  if(file.exists(redirections_path))
    df_redirections = readRDS(redirections_path)
  else
    stop(simpleError(
'Loading of associations failed. Internal files might be broken.
See `orphatools_options` or consider reisntalling orphatools package.'))

  return(df_redirections)
}


#' Apply ORPHAcodes redirections.
#'
#' @description
#' An ORPHAcode is redirected to an active ORPHAcode when it becomes deprecated or obsolete,
#' respectively with a *moved to* (appropriate redirection) or *referred to* (suggestion) association type.
#'
#' There is no redirection when status is *"Inactive: Non rare disease in Europe"*.
#' Redirection is always provided for deprecated ORPHAcodes, but not for obsolete ones.
#'
#' @param orpha_codes ORPHAcodes to redirect.
#' @param deprecated_only If `TRUE`, redirect only deprecated ORPHAcodes.
#'
#' @return The redirected ORPHAcodes. If no redirection is found, ORPHAcodes remain the same as given,
#' so ORPHAcodes may remain inactive after redirection.
#'
#' @export
#' @seealso [load_redirections()], [load_raw_redirections()]
#' @examples
#' orpha_codes = c(303, 166068, 166457)
#' redirect_code(orpha_codes)
#' redirect_code(orpha_codes, deprecated_only=FALSE)
redirect_code = function(orpha_codes, deprecated_only=TRUE){
  # Load known redirections
  df_redirections = load_raw_redirections()
  if(deprecated_only)
    df_redirections = df_redirections %>%
      filter(redir_type == 21471) # 21471 is the id for "moved to"

  # Redirect
  redirected_codes = data.frame(from=as.character(orpha_codes)) %>%
    left_join(df_redirections, by='from') %>%
    pull(to)

  return(redirected_codes)
}


