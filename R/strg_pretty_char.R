#' Make a character string pretty
#'
#' This function replaces underscores with spaces and capitalizes the first letter of each word in a character string.
#'
#' Mostly used when making colnames or items in table pretty.
#'
#' @param col A character string to make pretty.
#'
#' @return The pretty character string.
#'
#' @importFrom stringr str_replace_all str_to_title
#' @import magrittr
#'
#' @examples
#' strg_pretty_char("hello_world")
#' strg_pretty_char("the_quick_brown_fox")
#'
#' @export
strg_pretty_char = function(col){
  col %>%
    stringr::str_replace_all(., "_", " ") %>%
    stringr::str_to_title(.)
}
