#' Make multiple rolling mean functions.
#'
#' Makes rolling mean column(s) from a dataframe column defined by the user using window sizes also defined by the user.
#'
#' @param col a column to make rolling means of
#' @param width integer input indicating size of sliding window used to caluclate mean
#'
#' @return dataframe with rolling mean column(s)S
#' @export
#' @importFrom dplyr across
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' \dontrun{
#' df = data.frame(index = 1:10, value1 = rnorm(10), value2 = rnorm(10))
#'  dplyr::mutate(df, auto_make_rMean_col(col = value1, width = c(3, 7)))
#'  }
auto_make_rMean_col = function(col, width = c(7)){
  dplyr::across({{col}}, make_rMean_col(width = width, equal = 2), .names = "{.col}_rMean_{width}")
}
