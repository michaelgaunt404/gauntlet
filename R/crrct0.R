#' Rescale a vector with first entry.
#'
#' Use this function to rescale a vector using the first entry
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' @examples
#'
#' \dontrun{
#'data.frame(group = c(rep("A", 4), rep("B", 4))
#'           ,order = c(1:4, 1:4)) %>%
#'  mutate(value = 2*order+rnorm(8, 5)) %>%
#'  group_by(group) %>%
#'  mutate(value_corrected = crrct0(value))
#'  }
crrct0 = function(x){
  x-x[1]
}
