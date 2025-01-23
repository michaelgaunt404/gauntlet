#' Make rolling zscore attribute from timeseries.
#'
#' @param data a dataframe containing the timeseries attribute
#' @param grp character list of attributes to group by
#' @param width integer value indicating widtch of window
#' @param align character indicating which side of value window will be aligned. Default is `right`, which only uses past values. Other options are `center` and `left`.
#' @param threshold integer indicating zscore away from center function will flag value as an outlier
#'
#' @return a dataframe with zcore and zscore_flag attributes
#'
#' @importFrom dplyr across case_when group_by mutate select ungroup
#' @importFrom stringr str_glue
#' @importFrom zoo rollapply
#' @importFrom magrittr %>%
#'
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
        dplyr::group_by(dplyr::across({{grp}}))
      else .} %>%
    dplyr::mutate(tmp_med = zoo::rollapply(value, width = width,fill = NA, align = align, mean)
           ,tmp_sd = zoo::rollapply(value, width = width,fill = NA, align = align, sd)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      window_used = width,
      threshold_used = threshold,
      zscore = (value-tmp_med)/tmp_sd) %>%
    dplyr::select(!c(tmp_med, tmp_sd))

  if (!is.null(threshold)){
    tmp = tmp %>%
      dplyr::mutate(zscore_flag = dplyr::case_when(
        abs(zscore) < threshold ~ stringr::str_glue("Under {threshold}sd threshold"),
        T ~ stringr::str_glue("Over {threshold}sd threshold")) %>%
          as.factor(),
        zscore_flag_num = dplyr::case_when(
          abs(zscore) < threshold ~ 0,
          T~1)
      )
  }

  return(tmp)
}
