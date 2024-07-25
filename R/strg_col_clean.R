#' Quickly clean a dataframe's column names
#'
#' This function cleans a data frame by removing empty rows and columns, converting column names to snake_case, and replacing a specified value with NA.
#'
#' @param df A data frame to clean.
#' @param na_marker A value that should be replaced with NA.
#'
#' @return The cleaned data frame.
#'
#' @import magrittr
#' @importFrom janitor clean_names remove_empty
#'
#' @examples
#' df <- data.frame(A = c(1, NA, 3), B = c("apple", "orange", ""), C = c("", "banana", ""))
#' strg_col_clean(df, "")
#'
#' @export
strg_col_clean = function(df, na_marker = ""){
  df %>%
    # mutate(across(everything(), ~dplyr::na_if(.x, na_marker)))
    # dplyr::na_if(na_marker) %>% print()
    janitor::clean_names() %>%
    janitor::remove_empty(c("cols", "rows"))
}








