#' Create a string of repeated characters for indentation purposes
#'
#' This function creates a string of a specified length containing a specified character. It is commonly used to create indentation in printed output.
#'
#' @param with A character to repeat.
#' @param n An integer specifying the number of times to repeat the character.
#' @param c A string to separate each repeated character. Defaults to an empty string.
#' @param last A boolean indicating whether the resulting string should end with a newline character. Defaults to TRUE.
#'
#' @return A string of repeated characters separated by the specified separator string.
#'
#' @examples
#' make_space_2(n = 5, with = " ")
#' make_space_2(n = 5, with = "-", c = "", last = FALSE)
#'
#' @export
strg_make_space_2 = function(with = "+", n = 50, c = "", last = T){
  if (last){
    paste0(rep(with, n), collapse = c) %>% paste0(., "\n")
  } else {
    paste0(rep(with, n), collapse = c) %>% paste0("\n", .)
  }
}
