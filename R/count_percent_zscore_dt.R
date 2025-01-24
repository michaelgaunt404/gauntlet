#' Group by count, sum, percent and zscore - datatable variant.
#'
#' This function can be used to perform a number of mathematical operation on a datatable.
#' Count or sum numerical values by group, get percent breakdown of counts or sums by group, and get optional zscore of values.
#'
#' @param data a datatable
#' @param grp_c a vector of columns to group counting operation with - do not quote columns
#' @param grp_p a vector of columns to group percent calculation operation with - do not quote columns
#' @param grp_z a vector of columns to group zscore calculation operation with - do not quote columns
#' @param col a string indicating which column to count or sum
#' @param prefix a string used to prefix calculated columns with - leave empty if you do not want a prefix
#' @param rnd integer indicating how many digits you want calculated columns to be rounded to - leave empty if you do not want rounding
#' @param cntr_scl (`TRUE`/`FALSE`) boolean to indicate if zscore should be calculated - default is `FALSE`
#'
#' @return a datatable
#' @import data.table
#' @import palmerpenguins
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' \dontrun{
#' palmerpenguins::penguins %>%
#'mutate(count = 1
#'       ,zcount = 2) %>%
#'  data.table() %>%
#'  count_percent_zscore_dt(
#'    grp_c = c("species", "year")
#'    ,grp_p = c("species")
#'    ,col = "count"
#'  ) %>%
#'  print()
#'  }
count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                   col, prefix = NULL, rnd = NULL, cntr_scl = FALSE){

  tmp = data %>%
    data.table::data.table() %>%
    .[,.(count = sum(.SD)), .SDcols = col, by = grp_c] %>%
    .[,`:=`(percent = (count/sum(count)) %>%
              { if (!is.null(rnd)) round(., rnd) else .}), by = grp_p] %>%
    { if (cntr_scl) (.) %>%
        .[,`:=`(zscore = as.vector(scale(count))), by = grp_z]
      else .}

  if (is.null(prefix)){
    tmp = tmp
  } else {
    newname1 = str_glue("{prefix}_count")
    newname2 = str_glue("{prefix}_percent")
    rename(tmp, !!newname1 := count, !!newname2 := percent)
  }

  return(tmp)

}
