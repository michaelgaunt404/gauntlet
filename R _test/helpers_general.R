#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#plusEqual operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))


#takes quick counts for columns and their items
quick_value_count = function(df, rows, column, filter = NA){
  if(is.na(filter)){
  df[rows,] %>%
    select(all_of(column)) %>%
    nrow()
  } else {
    df[rows,] %>%
      select(all_of(column)) %>%
      filter(!!as.symbol(column) == filter) %>%
      nrow()
  }
}


#' Flatten a named list
#'
#' This function takes a named list as input and returns a data frame with two
#' columns: the name of each list element (including parent element names), and
#' its corresponding value. If the input is not a list, the function returns a
#' data frame with a single row containing the value and its parent name (if
#' provided).
#'
#' @param lst A named list to be flattened into a data frame.
#' @param parent_name A character string indicating the name of the parent list
#' element. This argument is optional and should only be provided if the input
#' list is a child element of a larger parent list. By default, the parent name
#' is set to an empty string.
#'
#' @return A data frame with two columns: the name of each list element (including
#' parent element names), and its corresponding value.
#'
#' @examples
#' test_list <- list(a = 1, b = list(c = 2, d = 3))
#' flatten_named_list(test_list)
#'
#' @export
#'
#' @keywords named list, data frame, recursion
#'
flatten_named_list <- function(lst, parent_name = "") {
  # If input is not a list, return a data frame with the single value
  if (!is.list(lst)) {
    return(data.frame(name = parent_name, value = lst))
  }
  # If input is a list, recursively flatten its elements and combine into a data frame
  else {
    child_dfs <- lapply(names(lst), function(name) {
      child_name <- if (parent_name == "") name else paste0(parent_name, ".", name)
      flatten_named_list(lst[[name]], child_name)
    })
    do.call(rbind, child_dfs)
  }
}
