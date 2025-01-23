#' Make multiple lagged columns.
#'
#' Makes lagged column(s) from a dataframe column defined by the user.
#'
#' @param col a column to make lags of
#' @param lags numeric list
#'
#' @return dataframe with lagged column(s)
#'
#' @importFrom dplyr across
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data.frame(index = 1:10,
#' value1 = rnorm(10), value2 = rnorm(10)) %>%
#' mutate(auto_make_lag_col(col = value1, c(1, 3, 5)))
#' }
auto_make_lag_col = function(col, lags = c(1, 7)){
  dplyr::across({{col}}, make_lag_col(lags = lags), .names = "{.col}_lag_{lags}")
}
