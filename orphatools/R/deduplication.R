#' For counting operations, all descendants of a given code should be rolled up.
#' The rows corresponding to a descendant are then duplicated to be attributed to the given code also
#'
#' @param .data The data to consider
#' @param orphaCode The given Orpha code
#' @param class_data The classification data to consider.
#' If NULL, automatically search a classification contatining the given Orpha code
#'
#' @return The duplicated descendants rows newly attributed to the given Orpha code
#' @import magrittr
#' @importFrom dplyr filter
#' @export
#'
#' @examples
roll_up_descendants = function(.data, orphaCode, class_data=NULL)
{
  # Find descendants
  codes_descendants = get_descendants(orphaCode, class_data, codes_only=TRUE)

  # Duplicated rows to be returned
  df_to_add = .data %>% filter(code %in% codes_descendants)
  if(length(df_to_add$code))
    df_to_add$code = orphaCode
  return(df_to_add)
}


#' A common group_by operation to take descendants of the present Orpha codes into account in the aggregation.
#' Aggregation works if column to group is named "code".
#'
#' @param .data The data consider
#' @param class_data The classification data to consider. If NULL, automatically search a classification for each code
#' @param ... Additional parameters to transmit to the dplyr group_by function
#' @param include_descendants
#'
#' @return Results of the group_by operation
#' @import magrittr
#' @importFrom dplyr bind_rows group_by
#' @export
#'
#' @examples
group_by_code = function(.data, ..., class_data=NULL, include_descendants=TRUE)
{
  # Find all Orpha codes present in the data
  all_codes = unique(.data$code) %>% as.numeric()

  # Duplicate descendants rows to count
  df_to_add = all_codes %>%
    lapply(function(orphaCode) roll_up_descendants(.data, orphaCode, class_data)) %>%
    bind_rows()
  .data = bind_rows(list(.data, df_to_add)) %>% distinct()

  return(group_by(.data, code, ...))
}
