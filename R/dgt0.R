#' Round to 0 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt0(rnorm(10))
dgt0 = function(x){
  round(x, 0)
}
