#' Make a number pretty for printing
#'
#' This function formats a number to make it more readable. Large numbers are expressed in millions or billions and small numbers are not rounded.
#'
#' @param vector A numeric vector to make pretty.
#' @param rnd The number of digits to round to.
#'
#' @return The pretty number string.
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @import magrittr
#'
#' @export
#' @examples
#' \dontrun{
#' strg_pretty_num(1234)
#' strg_pretty_num(12345678)
#' strg_pretty_num(-987654321, 2)
#'
#' }
strg_pretty_num = function(vector, rnd = 0){
  digit = vector %>%
    round(., rnd)

  dplyr::case_when(
    str_length(abs(digit))>9~paste0(round((digit/1e6), rnd), "B")
    ,str_length(abs(digit))>6~paste0(round((digit/1e6), rnd), "M")
    ,str_length(abs(digit))>3~paste0(round((digit/1e3), rnd), "k")
    ,str_length(abs(digit))>0~paste(digit)
    ,T~"Undefined")
}
