#' The orpha_df class allows taking ORPHA structure when codes are grouped together.
#' For instance, the counting of individuals related to a specific disorder will also
#' take into account individuals having the related subtypes of this disorder.
#' It needs the specification of the column of x containing the ORPHAcodes
#'
#' @param x A data frame or data frame extension
#' @param orphaCode_col The column name of `x` containing the ORPHAcodes
#' @param class_data
#' @param force_nodes
#'
#' @return An orpha_df instantiation
#' @export
#'
#' @examples
orpha_df = function(x, orphaCode_col='diag_code', class_data=NULL, force_nodes=NULL){
  cls = class(x)
  attr(x, 'orphaCode_col') = orphaCode_col
  attr(x, 'class_data') = class_data
  attr(x, 'force_nodes') = force_nodes
  class(x) <- c('orpha_df', cls)

  return(x)
}

dplyr_reconstruct.grouped_orpha_df = function(data, template){
  attrs = attributes(template)
  new_data = NextMethod()
  if(!attrs$orphaCode_col %in% names(new_data))
    warning("Column containing ORPHAcodes was removed from `orpha_df` object.")
  else{
    attr(new_data, 'orphaCode_col') = attrs$orphaCode_col
    attr(new_data, 'class_data') = attrs$class_data
    attr(new_data, 'force_nodes') = attrs$force_nodes
    attr(new_data, 'class') = attrs$class}
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


#' To be documented
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
ungroup.grouped_orpha_df = function(df){
  attrs = attributes(df)
  ungrouped_df = NextMethod()
  cls = class(ungrouped_df)
  attributes(ungrouped_df) = attrs[names(attrs) != 'groups']
  class(ungrouped_df) = c('orpha_df', cls)

  return(ungrouped_df)
}

#' To be documented
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
mutate.grouped_orpha_df = function(df, ...){
  keys = group_vars(df)
  tmp = df %>% summarize(...)
  df_res = df %>%
    ungroup() %>%
    left_join(tmp, by=keys, suffix=c('.x', '')) %>%
    select(-ends_with('.x')) %>%
    select(all_of(setdiff(names(tmp), keys))) %>%
    as.list()

  return(dplyr_col_modify(df, df_res))
}

#' To be documented
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#'@importFrom tidyr complete
#'
#' @examples
group_by.orpha_df = function(.data, ...){
  class_data = attr(.data, 'class_data')
  force_nodes = attr(.data, 'force_nodes')
  code_col = attr(.data, 'orphaCode_col')
  df_grouped = NextMethod()
  grouping_cols = group_vars(df_grouped)

  if(code_col %in% grouping_cols){
    # Find all Orpha codes present in the data and add forced nodes
    codes_list = unique(.data[[code_col]]) %>% as.character()
    all_codes = unique(c(.data[[code_col]], force_nodes)) %>% as.character()

    # Load the whole classification if unspecified
    if(is.null(class_data))
      class_data = load_classifications() %>% bind_rows() %>% distinct()

    # Find ancestors for each code present in .data
    class_graph = graph_from_data_frame(class_data)
    all_paths = find_roots(class_graph) %>%
      lapply(function(root)
        all_simple_paths(class_graph,
                         from=root,
                         to=intersect(names(V(class_graph)), codes_list))) %>%
      unlist(recursive = F)
    df_ancestors = cbind(all_paths) %>% as.data.frame() %>%
      mutate(
        all_paths = lapply(all_paths, function(path) names(path[names(path) %in% all_codes])),
        !!code_col := sapply(all_paths, function(x) last(x))) %>%
      group_by(!!sym(code_col)) %>%
      summarize(all_ancestors = list(unique(unlist(all_paths))))

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
    class(df_grouped) = c('grouped_orpha_df', 'orpha_df', class(df_grouped))
  }
  else{
    class(df_grouped) = c('orpha_df', class(df_grouped))
  }

  return(df_grouped)
}


#' [deprecated] A common group_by operation to take descendants of the present Orpha codes into account in the aggregation.
#'
#' @param .data The data consider
#' @param ... Additional parameters to transmit to the dplyr group_by function
#' @param class_data The classification data to consider. If NULL, automatically search a classification for each code
#' @param force_nodes function usually uses Orpha codes present in the original dataframe only,
#' but you can force aggregation on some additional nodes here
#' @param code_col default 'code', Orpha codes column name
#'
#' @return Results of the group_by operation
#' @import magrittr
#' @importFrom dplyr bind_rows group_by arrange desc distinct summarize rename mutate if_else left_join last relocate sym select
#' @importFrom tidyr unnest
#' @importFrom igraph graph_from_data_frame all_simple_paths
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' df_patients = data.frame(patient_id = c(1,1,2,3,4,5,6),
#'                          code = c('303', '158673', '595356', '305', '79406', '79406', '595356'))
#' df_counts = df_patients %>% group_by(code) %>% count() %>% as.data.frame() # Naive counting
#' df_counts = df_patients %>% group_by_code() %>% count() %>% as.data.frame() # New method - Without deduplication
#' df_counts = df_patients %>% group_by_code() %>% summarize(n = n_distinct(patient_id)) %>% as.data.frame() # New method - With deduplication
group_by_code = function(.data, ..., class_data=NULL, force_nodes=NULL, code_col='code'){
  warning("`group_by_code` is deprecated. Please transform your data frame to an `orpha_df` object first, then use `group_by`.")

  # Find all Orpha codes present in the data and add forced nodes
  codes_list = unique(.data[[code_col]]) %>% as.character()
  all_codes = unique(c(.data[[code_col]], force_nodes)) %>% as.character()

  # Load the whole classification if unspecified
  if(is.null(class_data))
    class_data = load_classifications() %>% bind_rows() %>% distinct()

  # Find ancestors for each code present in .data
  class_graph = graph_from_data_frame(class_data)
  all_paths = find_roots(class_graph) %>%
    lapply(function(root)
      all_simple_paths(class_graph,
                       from=root,
                       to=intersect(names(V(class_graph)), codes_list))) %>%
    unlist(recursive = F)
  df_ancestors = cbind(all_paths) %>% as.data.frame() %>%
    mutate(
      all_paths = lapply(all_paths, function(path) names(path[names(path) %in% all_codes])),
      !!code_col := sapply(all_paths, function(x) last(x))) %>%
    group_by(!!sym(code_col)) %>%
    summarize(all_ancestors = list(unique(unlist(all_paths))))

  # Group data and manually modify groups to roll up rows to ancestors
  df_grouped = group_by(.data, !!sym(code_col), ...)
  groups = attr(df_grouped, 'groups') %>%
    left_join(df_ancestors, by=code_col) %>%
    relocate(all_ancestors, .before = code_col) %>%
    unnest(all_ancestors, keep_empty = TRUE) %>%
    mutate(all_ancestors =
             if_else(is.na(all_ancestors), .data[[code_col]], all_ancestors)) %>%
    select(-all_of(code_col)) %>%
    group_by(across(c(-.rows))) %>%
    summarize(.rows = list(unique(unlist(.rows)))) %>%
    rename(!!code_col := all_ancestors)
  attr(df_grouped, 'groups') = groups
  class(df_grouped) = c('orpha_df_depr', class(df_grouped))

  return(df_grouped)
}


#' [deprecated] Overridden version of `dplyr::mutate` for dataframes grouped
#' with `group_by_code` function.
#'
#' @param df Dataframe to mutate
#' @param ... arguments for dplyr::mutate
#'
#' @return The mutated dataframe
#' @import magrittr
#' @importFrom dplyr group_vars summarize mutate left_join select ends_with
#' @export
#'
#' @examples
#' library(magrittr)
#' df = data.frame(code = c('303', '79408', '79408'), val = 1:3)
#' # df %>% group_by_code %>% dplyr::mutate(val2 = max(val)) # May rise an error "Can't recycle..."
#' df %>% group_by_code %>% orphatools::mutate(val2 = max(val)) # Works well
mutate = function(df, ...){
  if(any(class(df) == "orpha_df_depr")){
    old_vars = names(df)
    keys = group_vars(df)
    tmp = df %>% summarize(...)
    df_res = df %>% left_join(tmp, by=keys, suffix=c('.x', '')) %>% select(-ends_with('.x'))
    new_vars = setdiff(names(df_res), old_vars)
    df_res = df_res[,c(old_vars, new_vars)]
    class(df_res) = c('orpha_df', class(df_res))
    return(df_res)
  }
  else
    return(df %>% dplyr::mutate(...))
}
