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
orpha_df = function(x, orpha_code_col='orpha_code', df_classif=NULL, force_codes=NULL){
  cls = class(x)
  attr(x, 'orpha_code_col') = orpha_code_col
  attr(x, 'df_classif') = df_classif
  attr(x, 'force_codes') = as.character(force_codes)
  class(x) <- unique(c('orpha_df', cls))

  return(x)
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
    # Find all Orpha codes present in the data and add forced nodes
    orpha_codes = unique(.data[[code_col]]) %>% as.character()
    all_codes = unique(c(.data[[code_col]], force_codes)) %>% as.character()

    # Load the whole classification if unspecified
    if(is.null(df_classif))
      df_classif = load_classifications() %>% bind_rows() %>% distinct()

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
  class(df_grouped) = c('grouped_orpha_df', 'orpha_df', class(df_grouped))
  return(df_grouped)
}


dplyr_reconstruct.grouped_orpha_df = function(data, template){
  attrs = attributes(template)
  new_data = NextMethod()
  if(!attrs$orpha_code_col %in% names(new_data))
    warning("Column containing ORPHAcodes was removed from `orpha_df` object.")
  else{
    attr(new_data, 'orpha_code_col') = attrs$orpha_code_col
    attr(new_data, 'df_classif') = attrs$df_classif
    attr(new_data, 'force_codes') = attrs$force_codes
    attr(new_data, 'class') = attrs$class
  }
  return(new_data)
}


dplyr_col_modify.grouped_orpha_df = function(data, cols){
  new_data = NextMethod() %>% dplyr_reconstruct(data)
  return(new_data)
}


dplyr_row_slice.grouped_orpha_df = function(data, i, ...){
  new_data = NextMethod() %>% dplyr_reconstruct(data)
  return(new_data)
}


`names<-.grouped_orpha_df` = function(x, value){
  return(NextMethod() %>% dplyr_reconstruct(x))
}


`[.grouped_orpha_df` = function(x, loc, ...){
  return(NextMethod() %>% dplyr_reconstruct(x))
}


#' @export
ungroup.grouped_orpha_df = function(.data){
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
  tmp = .data %>% summarize(...)
  df_res = .data %>%
    ungroup() %>%
    left_join(tmp, by=keys, suffix=c('.x', '')) %>%
    select(-ends_with('.x')) %>%
    select(all_of(setdiff(names(tmp), keys))) %>%
    as.list()

  return(dplyr_col_modify(.data, df_res))
}


