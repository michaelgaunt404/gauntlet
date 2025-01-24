#' Round to 2 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt2(rnorm(10))
dgt2 = function(x){
  round(x, 2)
}
