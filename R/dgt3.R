#' Round to 3 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt3(rnorm(10))
dgt3 = function(x){
  round(x, 3)
}
