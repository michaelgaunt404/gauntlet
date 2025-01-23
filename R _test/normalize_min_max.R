#' Apply Min-Max normalization to a numeric vector.
#'
#' @param x a numeric vector. Works in tidyverse mututate function.
#' @param na.rm boolean(TRUE/FALSE)
#'
#' @return a normalized numeric vector.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars %>%
#'   mutate(hp_norm = normalize_min_max(hp))
#'   }
normalize_min_max <- function(x, na.rm = TRUE) {
  return((x - min(x)) /(max(x)-min(x)))
}
