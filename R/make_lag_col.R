#' make_lag_col
#'
#' Pre-filled lag function. This function should not be directly used, it is used inside of auto_make_lag_col function.
#'
#' @param lags numeric list
#'
#' @return pre-filled function
#'
#' @importFrom purrr map partial
#'
#' @export
#'
#' @examples
#' #none
make_lag_col = function(lags = c(1, 7)){
  purrr::map(lags, ~purrr::partial(dplyr::lag, n = .x))
}
