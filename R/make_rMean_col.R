#' make_rMean_col
#'
#' Pre-filled rolling mean function. This function should not be directly used, it is used inside of auto_make_rMean_col() .
#'
#' @param width integer input indicating size of sliding window used to calculate mean
#' @param equal currently not used
#'
#' @return  pre-filled function
#'
#' @importFrom purrr map partial
#' @importFrom roll roll_mean
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #none
#' }
# TODO can only perform equal weight for now
make_rMean_col = function(width, equal){
  purrr::map(width,
             ~purrr::partial(
               roll::roll_mean,
               width = .x,
               weights =
                 # ifelse(equal == T,
                 rep(1, .x)
               # ((c(1:.x)**equal)/(.x**equal/.9))
               # )
             )
  )
}
