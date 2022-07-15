#' make_lag_col
#'
#' Pre-filled lag function. This function should not be directly used, it is used inside of auto_make_lag_col function.
#'
#' @param lags numeric list
#'
#' @return pre-filled function
#' @export
#'
#' @examples
#'
#' #none
make_lag_col = function(lags = c(1, 7)){
  purrr::map(lags, ~purrr::partial(dplyr::lag, n = .x))
}

#' Make multiple lagged columns.
#'
#' Makes lagged column(s) from a dataframe column defined by the user.
#'
#' @param col a column to make lags of
#' @param lags numeric list
#'
#' @return dataframe with lagged column(s)
#' @export
#'
#' @examples
#'
#' data.frame(index = 1:10,
#' value1 = rnorm(10), value2 = rnorm(10)) %>%
#' mutate(auto_make_lag_col(col = value1, c(1, 3, 5)))
auto_make_lag_col = function(col, lags = c(1, 7)){
  dplyr::across({{col}}, make_lag_col(lags = lags), .names = "{.col}_lag_{lags}")
}

#' make_diff
#'
#' Makes difference column from a dataframe column defined by the user using lags as defined by the user. Should not be used directly by user - it is better to use via auto_make_diff_col() function.
#'
#' @param col a column to make lags
#' @param lags numeric list
#' @param percent boolean to determine if difference should be percent difference
#'
#' @return dataframe with differenced column
#' @export
#'
#' @examples
#'
#' data.frame(index = 1:10,
#'           value1 = rnorm(10), value2 = rnorm(10)) %>%
#'  mutate(value1_differenced = make_diff(col = index, c(1), percent = F)) %>%
#'  mutate(value1_differenced_percent = make_diff(col = index, c(1), percent = T))
make_diff = function(col, lags, percent = F){
  if (percent) {
    ({{col}}-dplyr::lag({{col}}, n = lags))/dplyr::lag({{col}}, n = lags)
  } else {
    {{col}}-dplyr::lag({{col}}, n = lags)
  }
}

#' make_diff_col
#'
#' Pre-filled difference function. This function should not be directly used, it is used inside of auto_make_diff_col() .
#'
#' @param lags numeric list
#' @param percent boolean to determine if difference should be percent difference
#'
#' @return pre-filled function
#' @export
#'
#' @examples
#'
#' #none
make_diff_col = function(lags = c(1, 7), percent = F){
  purrr::map(lags, ~purrr::partial(make_diff, lags = .x, percent = percent))
}

#' Make multiple differenced columns.
#'
#' Makes differenced column(s) from a dataframe column defined by the user.
#'
#' @param col a column to make differences of
#' @param lags numeric list
#' @param percent boolean to determine if difference should be percent difference
#'
#' @return dataframe with differenced column(s)
#' @export
#'
#' @examples
#'
#' data.frame(index = 1:10,
#'           value1 = rnorm(10), value2 = rnorm(10)) %>%
#'  mutate(auto_make_diff_col(col = value1, percent = T))
 auto_make_diff_col = function(col, lags = c(1, 7), percent = F){
   dplyr::across({{col}}
                 ,make_diff_col(lags = lags, percent = percent)
                 ,.names = "{.col}_diff{ifelse(percent, 'p', '')}_{lags}")
 }


#' make_rMean_col
#'
#' Pre-filled rolling mean function. This function should not be directly used, it is used inside of auto_make_rMean_col() .
#'
#' @param width integer input indicating size of sliding window used to caluclate mean
#' @param equal currently not used
#'
#' @return  pre-filled function
#' @export
#'
#' @examples
#'
#' #none
make_rMean_col = function(width, equal){
  #can only perform equal weight for now

  purrr::map(width,
             ~purrr::partial(
               roll::roll_mean,
               width = .x,
               weights =
                 # ifelse(equal == T,
                 rep(1, .x)
               # ((c(1:.x)**equal)/(.x**equal/.9))
               # )
             )
  )
}

#' Make multiple rolling mean functions.
#'
#' Makes rolling mean column(s) from a dataframe column defined by the user using window sizes also defined by the user.
#'
#' @param col a column to make rolling means of
#' @param width integer input indicating size of sliding window used to caluclate mean
#'
#' @return dataframe with rolling mean column(s)S
#' @export
#'
#' @examples
#' data.frame(index = 1:10,
#'           value1 = rnorm(10), value2 = rnorm(10)) %>%
#'  mutate(auto_make_rMean_col(col = value1, width = c(3, 7)))
auto_make_rMean_col = function(col, width = c(7)){
  dplyr::across({{col}}, make_rMean_col(width = width, equal = 2), .names = "{.col}_rMean_{width}")
}
