#' Limit to Non-Negative Values
#'
#' This function takes a numeric vector and replaces any negative values with zero.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with non-negative values.
#'
#' @examples
#' lmt0(c(5, 6, 2, 0, -1))
#'
#' @export
lmt0 <- function(x) {
  ifelse(x < 0, 0, x)
}
