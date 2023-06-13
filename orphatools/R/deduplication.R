#' For counting operations, all descendants of a given code should be rolled up.
#' The rows corresponding to a descendant are then duplicated to be attributed to the given code also
#'
#' @param .data The data to consider
#' @param orphaCode The given Orpha code
#' @param edgelist Information of how codes are related to each other
#'
#' @return The duplicated descendants rows newly attributed to the given Orpha code
#' @import magrittr
#' @importFrom dplyr filter pull
roll_up_descendants = function(.data, orphaCode, edgelist, code_col='code')
{
  codes_descendants = edgelist %>% filter(from == orphaCode) %>% pull(to)
  # Duplicated rows to be returned
  df_to_add = .data %>% filter(!!sym(code_col) %in% codes_descendants)
  if(length(df_to_add[[code_col]]))
    df_to_add[[code_col]] = orphaCode
  return(df_to_add)
}



#' A common group_by operation to take descendants of the present Orpha codes into account in the aggregation.
#'
#' @param .data The data consider
#' @param ... Additional parameters to transmit to the dplyr group_by function
#' @param class_data The classification data to consider. If NULL, automatically search a classification for each code
#' @param include_descendants If TRUE (default), for each code every descendants of this code are included in its group
#' @param force_nodes function usually uses Orpha codes present in the original dataframe only,
#' but you can force aggregation on some additional nodes here
#' @param code_col default 'code', Orpha codes column name
#'
#' @return Results of the group_by operation
#' @import magrittr
#' @importFrom dplyr bind_rows group_by
#' @importFrom igraph as_data_frame
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
group_by_code = function(.data, ..., class_data=NULL, include_descendants=TRUE, force_nodes=NULL, code_col='code'){
  # Find all Orpha codes present in the data and add forced nodes
  all_codes = unique(c(.data[[code_col]], force_nodes)) %>% as.character()

  # Get the corresponding orpha structure
  common_graph = get_common_graph(all_codes, class_data = class_data,
                                  what = 'descendants', shortcuts = TRUE)
  common_edgelist = as_data_frame(common_graph, what='edges')

  # Duplicate descendants rows to count
  df_to_add = all_codes %>%
    lapply(function(orphaCode) roll_up_descendants(.data, orphaCode, common_edgelist, code_col)) %>%
    bind_rows()
  .data = bind_rows(list(.data, df_to_add))

  return(group_by(.data, !!sym(code_col), ...))
}
