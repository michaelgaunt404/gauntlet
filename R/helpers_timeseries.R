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
#' #none
# TODO can only perform equal weight for now
make_rMean_col = function(width, equal){
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


#' Make rolling zscore attribute from timeseries.
#'
#' @param data a dataframe containing the timeseries attribute
#' @param grp character list of attributes to group by
#' @param width integer value indicating widtch of window
#' @param align character indicating which side of value window will be aligned. Default is `right`, which only uses past values. Other options are `center` and `left`.
#' @param threshold integer indicating zscore away from center function will flag value as an outlier
#'
#' @return a dataframe with zcore and zscore_flag attributes
#' @export
#'
#' @examples
#'data.frame(index = 1:60, value = c(rnorm(50, 10, 3), rnorm(10, 25, 0))) %>%
#'  na.omit() %>%
#'  ts_zscore(width = 30)
#'
#'c(10, 30) %>%
#'  map(~data.frame(index = 1:60
#'                  ,value = c(rnorm(50, 10, 3), rnorm(10, 25, 0))) %>%
#'        na.omit() %>%
#'        ts_zscore(width = .x)) %>%
#'  reduce(bind_rows)
ts_zscore = function(data, grp = NULL, width = 7
                     ,align = "right", threshold = 3){

  tmp = data %>%
    { if (!is.null(grp)) (.) %>%
        group_by(across({{grp}}))
      else .} %>%
    mutate(tmp_med = zoo::rollapply(value, width = width,fill = NA, align = align, mean)
           ,tmp_sd = zoo::rollapply(value, width = width,fill = NA, align = align, sd)) %>%
    ungroup() %>%
    mutate(
      window_used = width,
      threshold_used = threshold,
      zscore = (value-tmp_med)/tmp_sd) %>%
    select(!c(tmp_med, tmp_sd))

  if (!is.null(threshold)){
    tmp = tmp %>%
      mutate(zscore_flag = case_when(
        abs(zscore) < threshold ~ str_glue("Under {threshold}sd threshold"),
        T~str_glue("Over {threshold}sd threshold")) %>%
          as.factor(),
        zscore_flag_num = case_when(
          abs(zscore) < threshold ~ 0,
          T~1)
      )
  }

  return(tmp)
}

#' Make MAD (median absolute deviation) values.
#'
#' Function computes MAD statistic(s) for numeric vector. All values in vector are used to create MAD statistic. MAD values for all or last entry alone can be returned.
#'
#' @param values vector of numeric values to MAD.
#' @param last boolean - determines if last value should only be returned. Default is `FALSE`.
#'
#' @return returns a single MAD value for the last value in vector
#' @export
#'
#' @examples
#'numeric_vector = c(rnorm(10, 10, 3), 100)
#'
#'roll_mad(numeric_vector)
#'
#'roll_mad(numeric_vector, last = T)
#'
#'#compare to zscore
#'scale(numeric_vector)[,1]
# TODO should be able to pick tail, head, or centered for window alignment
roll_mad = function(values, last = F){
  ((abs(values-median(values, na.rm = T)))/
     median(abs(values-median(values, na.rm = T)), na.rm = T)) %>%
    { if (last) (.) %>%
        tail(1)
      else .}
}

#' Make rolling MAD (median absolute deviation) values for dataframe attribute.
#'
#' @param data a data frame with a timerseries attribute - attribute must be named value to work
#' @param window an integer indicating width of window
#'
#' @return a dataframe with rolling MAD statistic attribute
#' @export
#'
#' @examples
#'test_data = data.frame(index = 1:53,
#'                       value = c(rnorm(25, 10, 3)
#'                                 ,rnorm(3, 100, 3)
#'                                 ,rnorm(25, 10, 3)))
#'test_data %>%
#'  ts_mad(window = 7)
ts_mad = function(data, window = 20){

  data = data %>%
    mutate(index_forced = row_number())

  temp_list = list()
  for (i in 1:(nrow(data)-window)){

    #make window subset
    temp_df = data[i:(i+window),]

    #make distance matrix for window
    window_values = temp_df %>%
      pull(value)

    temp_list[[i]] = temp_df %>%
      tail(1) %>%
      mutate(mad_value = roll_mad(window_values, last = T))

  }

  #collapses summary list objects
  temp_dffff =
    temp_list %>%
    data.table::rbindlist(fill=TRUE) %>%
    select(!index_forced)


  return(temp_dffff)
}
























