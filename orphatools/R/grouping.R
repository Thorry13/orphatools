#' Prepare grouping
#'
#' @description
#' This class allows performing hierarchical grouping operations using the Orphanet classification system.
#' Basically each ORPHAcode group built with [group_by.orpha_df()] will also contain rows with descendants ORPHAcodes.
#' This away a row may belong to several groups.
#'
#' The `orpha_df` class just needs to know in which column ORPHAcodes are located to work.
#'
#' @param x A data.frame object or data.frame extension.
#' @param orpha_code_col The column name of `x` containing the ORPHAcodes.
#' @param df_classif The classification to consider, in a from/to format.
#' @param force_codes ORPHAcodes to be forced to appear in `groups` attribute after a [group_by()] operation.
#' @param mode Should either be `"on"` (the default) to convert data.frame into `orpha_df` object or
#' `"off"` to roll back to previous class and remove relevant attributes.
#'
#' @return An `orpha_df` instantiation. Any data.frame extension will be kept.
#' Former attributes are erased if `orpha_df` is called multiple times.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Build patients data.frame
#' df_patients = data.frame(
#' patient_id = c(1,1,2,3,4,5,6),
#' code = c('303', '158673', '595356', '305', '79406', '79406', '595356'))
#'
#' df_counts = df_patients %>%
#'  group_by(code) %>%
#'  count() %>%
#'  as.data.frame()
#'
#' df_counts = df_patients %>%
#'  orpha_df(orpha_code_col = 'code') %>%
#'  group_by(code) %>%
#'  count() %>%
#'  as.data.frame()
#'
orpha_df = function(x, orpha_code_col='orpha_code', df_classif=NULL, force_codes=NULL, mode='on'){
  if(mode == 'on'){
    if(!'data.frame' %in% class(x))
      stop(simpleError('`x` is not a data.frame.'))
    if(!orpha_code_col %in% names(x)){
      warning(sprintf('`%s` is not a column of `x`, which remains unchanged.',
                      orpha_code_col))
      return(x)}

    attr(x, 'orpha_code_col') = orpha_code_col
    attr(x, 'df_classif') = df_classif
    attr(x, 'force_codes') = as.character(force_codes)
    class(x) <- unique(c('orpha_df', class(x)))}

  else if(mode == 'off'){
    attr(x, 'orpha_code_col') = NULL
    attr(x, 'df_classif') = NULL
    attr(x, 'force_codes') = NULL
    class(x) <- setdiff(class(x), c('grouped_orpha_df', 'orpha_df'))}

  else
    stop(simpleError("Invalide `mode` argument. It should be set among c('on', 'off')"))

  return(x)
}

#' @export
dplyr_reconstruct.orpha_df = function(data, template){
  attrs = attributes(template)
  new_data = NextMethod()

  attr(new_data, 'orpha_code_col') = attrs$orpha_code_col
  attr(new_data, 'df_classif') = attrs$df_classif
  attr(new_data, 'force_codes') = attrs$force_codes
  attr(new_data, 'class') = attrs$class
  return(new_data)
}

#' @export
dplyr_col_modify.orpha_df = function(data, cols){
  new_data = NextMethod()

  code_col = attr(data, 'orpha_code_col')
  if(code_col %in% names(cols)){
    warning("The column containing ORPHAcodes was modified. Applying `orpha_df(., mode='off')`")
    return(new_data %>% orpha_df(mode='off'))
  }
  else
    return(new_data)
}

#' @export
`[.orpha_df` <- function(x, loc, ...){
  new_x = NextMethod()
  if(is.data.frame(new_x)){
    new_x = new_x %>% dplyr_reconstruct(x)
    if(!attr(new_x, 'orpha_code_col') %in% names(new_x)){
      warning("The column containing ORPHAcodes was removed. Applying `orpha_df(., mode='off')`")
      new_x = orpha_df(new_x, mode='off')
    }
  }
  return(new_x)
}

#' @export
`names<-.orpha_df` = function(x, value){
  old_names = names(x)
  code_col = attr(x, 'orpha_code_col')
  attr(x, 'orpha_code_col') = value[old_names == code_col]
  return(NextMethod())
}


#' Build group using Orphanet classifications
#'
#' @description
#' This is a method for the dplyr [group_by()] generic. It extends initial groups by adding
#' rows containing descendants ORPHAcodes. This way any row may belong to several groups.
#'
#' The function will force the creation of groups (they may be empty) for any ORPHAcode provided
#' in `force_codes` argument when [orpha_df()] is called.
#'
#' @param .data An [orpha_df()] instantiation.
#' @inheritParams dplyr::group_by
#'
#' @export
#' @import dplyr
#' @importFrom tidyr complete unnest
#' @examples
#' library(dplyr)
#'
#' # Build patients data.frame
#' df_patients = data.frame(
#' patient_id = c(1,2,3,4,5,6),
#' code = c('158673', '595356', '305', '79406', '79406', '595356'))
#'
#' df_counts = df_patients %>% group_by(code)
#' attr(df_counts, 'groups')
#'
#' df_counts = df_patients %>% orpha_df(orpha_code_col = 'code') %>% group_by(code)
#' attr(df_counts, 'groups')
#'
group_by.orpha_df = function(.data, ...){
  df_classif = attr(.data, 'df_classif')
  force_codes = attr(.data, 'force_codes')
  code_col = attr(.data, 'orpha_code_col')
  df_grouped = NextMethod()
  grouping_cols = group_vars(df_grouped)

  if(code_col %in% grouping_cols){
    # Load the whole classification if unspecified
    if(is.null(df_classif))
      df_classif = load_classifications() %>% bind_rows() %>% distinct()

    # Find all Orpha codes present in the data and add forced nodes
    orpha_codes = unique(.data[[code_col]]) %>% as.character()
    all_codes = unique(c(orpha_codes, force_codes)) %>% as.character()

    # Find ancestors for each code present in .data
    class_graph = graph_from_data_frame(df_classif)
    all_paths = find_roots(class_graph) %>%
      lapply(function(root)
        all_simple_paths(class_graph,
                         from=root,
                         to=intersect(names(V(class_graph)), orpha_codes))) %>%
      unlist(recursive = F)
    df_ancestors = cbind(all_paths) %>% as.data.frame() %>%
      mutate(
        all_paths = lapply(all_paths, function(path) names(path[names(path) %in% all_codes])),
        !!code_col := sapply(all_paths, function(x) last(x))) %>%
      group_by(!!sym(code_col)) %>%
      summarize(all_ancestors = list(unique(unlist(all_paths)))) %>%
      unnest(!!sym(code_col))

    # Group data and manually modify groups to roll up rows to ancestors
    groups = attr(df_grouped, 'groups') %>%
      mutate(!!code_col := factor(.data[[code_col]], levels=all_codes)) %>%
      complete(!!sym(code_col), fill=list(".rows"=list(integer(0)))) %>%
      left_join(df_ancestors, by=code_col) %>%
      relocate(all_ancestors, .before = all_of(code_col)) %>%
      unnest(all_ancestors, keep_empty = TRUE) %>%
      mutate(all_ancestors =
               if_else(is.na(all_ancestors), .data[[code_col]], all_ancestors)) %>%
      select(-all_of(code_col)) %>%
      group_by(across(c(-.rows))) %>%
      summarize(.rows = list(unique(unlist(.rows)))) %>%
      rename(!!code_col := all_ancestors)

    attr(df_grouped, 'groups') = groups
  }
  if('grouped_df' %in% class(df_grouped)){
    # cls = c('grouped_df', 'grouped_orpha_df', 'orpha_df')
    # class(df_grouped) = c(cls, setdiff(class(df_grouped), cls))
    class(df_grouped) = c('grouped_orpha_df', 'orpha_df', class(df_grouped))
  }
  else
    df_grouped = dplyr_reconstruct(df_grouped, .data)
  return(df_grouped)
}

# #' @export
# `[.grouped_orpha_df` <- function(x, loc, ...){
#   new_x = NextMethod() %>% dplyr_reconstruct(x)
#   return(new_x)
# }
#

# `names<-.grouped_orpha_df` = function(x, value){
#   y = NextMethod()
#   class(y) = c('grouped_orpha_df', 'orpha_df', class(y))
#   return(dplyr_reconstruct(y, x))
# }

#' @export
`names<-.grouped_orpha_df` = function(x, value){
  y = NextMethod()

  attrs = attributes(x)
  attr(y, 'orpha_code_col') = attrs$orpha_code_col
  attr(y, 'df_classif') = attrs$df_classif
  attr(y, 'force_codes') = attrs$force_codes
  attr(y, 'class') = attrs$class

  return(y)
}

#' @export
dplyr_reconstruct.grouped_orpha_df = function(data, template){
  new_data = NextMethod()
  # attr(new_data, 'groups') = attr(template, 'groups')
  return(new_data)
}

#' @export
dplyr_row_slice.grouped_orpha_df = function(data, i, ...){
  new_data = NextMethod() %>% dplyr_reconstruct(data)
  # Regroup ?
  return(new_data)
}

#' @export
dplyr_col_modify.grouped_orpha_df = function(data, cols){
  new_data = NextMethod()

  attrs = attributes(data)
  attr(new_data, 'orpha_code_col') = attrs$orpha_code_col
  attr(new_data, 'df_classif') = attrs$df_classif
  attr(new_data, 'force_codes') = attrs$force_codes
  attr(new_data, 'class') = attrs$class

  code_col = attr(data, 'orpha_code_col')
  if(code_col %in% names(cols)){
    new_data = new_data %>%
      orpha_df(mode='off') %>%
      ungroup() %>%
      group_by(!!sym(code_col))

    return(new_data)
  }

  return(new_data)
}


#' @export
ungroup.grouped_orpha_df = function(.data, ...){
  attrs = attributes(.data)
  ungrouped_df = NextMethod()
  cls = class(ungrouped_df)
  attributes(ungrouped_df) = attrs[names(attrs) != 'groups']
  class(ungrouped_df) = c('orpha_df', cls)

  return(ungrouped_df)
}

#' @export
mutate.grouped_orpha_df = function(.data, ...){
  keys = group_vars(.data)
  code_col = attr(.data, 'orpha_code_col')
  dots = enquos(...)
  dots_code = dots[names(dots) == code_col]
  other_dots = dots[names(dots) != code_col]

  if(code_col %in% keys){
    # Aggregate first before a potential regrouping (like mutate.grouped_df)
    tmp = .data %>% summarize(!!!other_dots) %>% ungroup()

    # attr(.data, 'groups') = attr(.data, 'groups') %>% mutate(!!!dots_code)
    df_res = .data %>%
      orpha_df(mode='off') %>%
      ungroup() %>%
      left_join(tmp, by=keys, suffix=c('.x', ''))

    if(length(dots_code))
      df_res = df_res %>% mutate(!!!dots_code)
    else
      df_res = df_res %>% select(-all_of(code_col))

    df_res = df_res %>%
      select(any_of(names(tmp)))

    return(dplyr_col_modify(.data, as.list(df_res)))
  }
  else
    return(NextMethod())
}

