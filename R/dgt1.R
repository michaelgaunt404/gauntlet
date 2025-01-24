#' Round to 1 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt1(rnorm(10))
dgt1 = function(x){
  round(x, 1)
}
