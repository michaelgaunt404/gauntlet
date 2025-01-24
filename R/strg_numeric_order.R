#' Order factor levels numerically
#'
#' `strg_numeric_order` orders factor levels numerically based on the values in the input vector.
#' If `rev` is set to `TRUE`, the levels are reversed.
#'
#' This function is useful for ensuring that factor levels are ordered correctly when dealing with numeric values.
#' It leverages the `stringr::str_sort` function to sort the levels.
#'
#' @param col A factor or character vector.
#' @param rev Logical; if `TRUE`, reverse the order of levels.
#' @return A factor with reordered levels.
#'
#' @importFrom forcats fct_relevel fct_rev
#' @importFrom stringr str_sort
#' @import magrittr
#'
#' @export
#' @examples
#' \dontrun{
#' temp_df = paste0("lab_", seq(1, 50, 2)) %>%
#' data.frame(label_unordered = .
#'            ,label_ordered = strg_numeric_order(.))
#'
#' temp_df %>% arrange(label_unordered) %>% print()
#' temp_df %>% arrange(desc(label_ordered)) %>% print()
#'
#' }
strg_numeric_order <- function(col, rev = TRUE) {
  if (rev) {
    forcats::fct_relevel(
      col,
      unique(stringr::str_sort(col, numeric = TRUE))
    ) %>% forcats::fct_rev()
  } else {
    forcats::fct_relevel(
      col,
      unique(stringr::str_sort(col, numeric = TRUE))
    )
  }
}
