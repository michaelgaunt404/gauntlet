#' Make a string using a character repeated some amount of times.
#'
#' @details
#' This function is deprecated please use `gauntlet::strg_make_space_2` instead.
#'
#' @param with string character to repeat - default is a plus sign ('+')
#' @param n integer indicating number of times string character should be repeated - default 50
#' @param c string character collapse with input - default is an empty character ('')
#'
#' @return string of collapsed, repeated character
#' @export
#'
#' @examples
#'
#' make_space(with = "//", c = "\\")
make_space = function(with = "+", n = 50, c = ""){
  paste0(rep(with, n),collapse=c) %>%
    paste0("\n", .)
}
