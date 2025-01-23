#' make_diff
#'
#' Makes difference column from a dataframe column defined by the user using lags as defined by the user. Should not be used directly by user - it is better to use via auto_make_diff_col() function.
#'
#' @param col a column to make lags
#' @param lags numeric list
#' @param percent boolean to determine if difference should be percent difference
#'
#' @return dataframe with differenced column
#'
#' @importFrom dplyr lag
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data.frame(index = 1:10,
#'           value1 = rnorm(10), value2 = rnorm(10)) %>%
#'  mutate(value1_differenced = make_diff(col = index, c(1), percent = F)) %>%
#'  mutate(value1_differenced_percent = make_diff(col = index, c(1), percent = T))
#'  }
make_diff = function(col, lags, percent = F){
  if (percent) {
    ({{col}}-dplyr::lag({{col}}, n = lags))/dplyr::lag({{col}}, n = lags)
  } else {
    {{col}}-dplyr::lag({{col}}, n = lags)
  }
}
