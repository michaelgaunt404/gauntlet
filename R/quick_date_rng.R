#' Quick Date Range
#'
#' Function returns min and max date values given month input by user.
#' Mostly, to create date ranges for plots.
#'
#' @param mon integer input.
#'
#' @return vector of dates
#' @export
#'
#' @examples
#' quick_date_rng()
#'
#' quick_date_rng(month = 3)
quick_date_rng = function(mon = 6){
  c(Sys.Date()-months(mon), Sys.Date())
}
