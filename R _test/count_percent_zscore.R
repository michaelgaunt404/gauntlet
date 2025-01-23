#automates aggregating counts and percents for different groupings of data
#' Group by count, sum, percent and zscore.
#'
#' This function can be used to perform a number of mathematical operation on a dataframe.
#' Count or sum numerical values by group, get percent breakdown of counts or sums by group, and get optional zscore of values.
#'
#' @param data a dataframe
#' @param grp_c a vector of columns to group counting operation with - do not quote columns
#' @param grp_p a vector of columns to group percent calculation operation with - do not quote columns
#' @param grp_z a vector of columns to group zscore calculation operation with - do not quote columns
#' @param col a column to count or sum - do not quote column
#' @param prefix a string used to prefix calculated columns with - leave empty if you do not want a prefix
#' @param rnd integer indicating how many digits you want calculated columns to be rounded to - leave empty if you do not want rounding
#' @param cntr_scl (`TRUE`/`FALSE`) boolean to indicate if zscore should be calculated - default is `FALSE`
#'
#' @return a dataframe
#' @import dplyr
#' @export
#'
#' @examples
#'
#' \dontrun{
#'temp_data = data.frame(group = c(rep("A", 4), rep("B", 4))
#'                       ,order = c(1:4, 1:4)) %>%
#'  mutate(value = 2*order+rnorm(8, 5)
#'         ,count = 1)
#'
#'count_percent_zscore(temp_data, grp_c = c(group), grp_p = c(), col = count)
#'
#'count_percent_zscore(temp_data, grp_c = c(order), grp_p = c(), col = value)
#'}
count_percent_zscore = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                col , prefix = NULL, rnd = NULL, cntr_scl = FALSE){
  tmp = data %>%
    dplyr::group_by(across({{grp_c}})) %>%
    dplyr::summarise(count = sum({{col}})) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across({{grp_p}})) %>%
    dplyr::mutate(percent = (count/sum(count)) %>%
                    { if (!is.null(rnd)) round(., rnd) else .}
    ) %>%
    dplyr::ungroup() %>%
    { if (cntr_scl) (.) %>%
        dplyr::group_by(across({{grp_z}})) %>%
        dplyr::mutate(zscore = as.vector(scale(count)))
      else .}

  if (is.null(prefix)){
    tmp
  } else {
    newname1 = stringr::str_glue("{prefix}_count")
    newname2 = stringr::str_glue("{prefix}_percent")
    dplyr::rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
} = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                col , prefix = NULL, rnd = NULL, cntr_scl = FALSE){
  tmp = data %>%
    dplyr::group_by(across({{grp_c}})) %>%
    dplyr::summarise(count = sum({{col}})) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across({{grp_p}})) %>%
    dplyr::mutate(percent = (count/sum(count)) %>%
                    { if (!is.null(rnd)) round(., rnd) else .}
    ) %>%
    dplyr::ungroup() %>%
    { if (cntr_scl) (.) %>%
        dplyr::group_by(across({{grp_z}})) %>%
        dplyr::mutate(zscore = as.vector(scale(count)))
      else .}

  if (is.null(prefix)){
    tmp
  } else {
    newname1 = stringr::str_glue("{prefix}_count")
    newname2 = stringr::str_glue("{prefix}_percent")
    dplyr::rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
}
