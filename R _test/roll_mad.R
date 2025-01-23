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
#' #compare to zscore
#' scale(numeric_vector)[,1]
# TODO should be able to pick tail, head, or centered for window alignment
roll_mad = function(values, last = F){
  ((abs(values-median(values, na.rm = T)))/
     median(abs(values-median(values, na.rm = T)), na.rm = T)) %>%
    { if (last) (.) %>%
        tail(1)
      else .}
}
