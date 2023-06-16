#' A common group_by operation to take descendants of the present Orpha codes into account in the aggregation.
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
#' @importFrom dplyr bind_rows group_by arrange desc distinct summarize rename
#' @importFrom tidyr unnest
#' @importFrom igraph graph_from_data_frame
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

  # all_paths = all_simple_paths(class_graph, from = root, to = intersect(names(V(class_graph)), codes_list))
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
    unnest(all_ancestors) %>%
    group_by(all_ancestors) %>%
    summarize(.rows = list(unique(unlist(.rows)))) %>%
    rename(!!code_col := all_ancestors)

  attr(df_grouped, 'groups') = groups
  return(df_grouped)
}
