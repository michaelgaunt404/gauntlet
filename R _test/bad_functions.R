# #' make_diff_col
# #'
# #' Pre-filled difference function. This function should not be directly used, it is used inside of auto_make_diff_col() .
# #'
# #' @param lags numeric list
# #' @param percent boolean to determine if difference should be percent difference
# #'
# #' @return pre-filled function
# #' @export
# #'
# #' @examples
# #' #none
# make_diff_col = function(lags = c(1, 7), percent = F){
#   purrr::map(lags, ~purrr::partial(make_diff, lags = .x, percent = percent))
# }

# #' Make multiple differenced columns.
# #'
# #' Makes differenced column(s) from a dataframe column defined by the user.
# #'
# #' @param col a column to make differences of
# #' @param lags numeric list
# #' @param percent boolean to determine if difference should be percent difference
# #'
# #' @return dataframe with differenced column(s)
# #' @export
# #'
# #' @examples
# #' #df = data.frame(index = 1:10,
# #'  #         value1 = rnorm(10), value2 = rnorm(10))
# #' #
# #' # dplyr::mutate(df, auto_make_diff_col(col = value1, percent = T))
# auto_make_diff_col = function(col, lags = c(1, 7), percent = F){
#   dplyr::across({{col}}
#                 ,make_diff_col(lags = lags, percent = percent)
#                 ,.names = "{.col}_diff{ifelse(percent, 'p', '')}_{lags}")
# }
