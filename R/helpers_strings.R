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

#' Make a string using a character repeated some amount of times.
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

#' Make a clean datetime string of current datetime with simple punctuation and syntax.
#'
#' @return character string of datetime using only underscores
#' @export
#'
#' @examples
#'
#' clean_datetime()
strg_clean_datetime = function(){
  str_remove_all(Sys.time(), "[:punct:]") %>%
    str_replace_all(" ", "_")
}

#' Make a clean date string of current date with simple punctuation and syntax.
#'
#' @return character string of date without bad punctuation
#' @export
#'
#' @examples
#'
#' clean_date()
strg_clean_date = function(){
  str_remove_all(Sys.Date(), "[:punct:]") %>%
    str_replace_all(" ", "_")
}

#' Evaluate a string as a command
#'
#' Takes a string and evaluates it as an R command.
#'
#' @param text A character string representing the R command to evaluate.
#'
#' @return The result of evaluating the R command.
#'
#' @examples
#' strg_fun_eval("1 + 1")
#' strg_fun_eval("mean(c(1, 2, 3))")
#'
#' @export
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))
}

#' Quickly clean a dataframe's column names
#'
#' This function cleans a data frame by removing empty rows and columns, converting column names to snake_case, and replacing a specified value with NA.
#'
#' @param df A data frame to clean.
#' @param na_marker A value that should be replaced with NA.
#'
#' @return The cleaned data frame.
#'
#' @import dplyr
#' @import janitor
#'
#' @examples
#' df <- data.frame(A = c(1, NA, 3), B = c("apple", "orange", ""), C = c("", "banana", ""))
#' strg_col_clean(df, "")
#'
#' @export
strg_col_clean = function(df, na_marker = ""){
  df %>%
    # mutate(across(everything(), ~dplyr::na_if(.x, na_marker)))
    # dplyr::na_if(na_marker) %>% print()
    janitor::clean_names() %>%
    janitor::remove_empty(c("cols", "rows"))
}

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
#' @import stringr
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

#' Make a number pretty for printing
#'
#' This function formats a number to make it more readable. Large numbers are expressed in millions or billions and small numbers are not rounded.
#'
#' @param vector A numeric vector to make pretty.
#' @param rnd The number of digits to round to.
#'
#' @return The pretty number string.
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#' strg_pretty_num(1234)
#' strg_pretty_num(12345678)
#' strg_pretty_num(-987654321, 2)
#'
#' @export
strg_pretty_num = function(vector, rnd = 0){
  digit = vector %>%
    gauntlet::dgt0()

  case_when(str_length(abs(digit))>9~paste0(round((digit/1e6), rnd), "B")
            ,str_length(abs(digit))>6~paste0(round((digit/1e6), rnd), "M")
            ,str_length(abs(digit))>3~paste0(round((digit/1e3), rnd), "k")
            ,str_length(abs(digit))>0~paste(digit)
            ,T~"Undefined")
}


#' Order factor levels numerically
#'
#' `strg_numeric_order` orders factor levels numerically based on the values in the input vector.
#' If `rev` is set to `TRUE`, the levels are reversed.
#'
#' This function is useful for ensuring that factor levels are ordered correctly when dealing with numeric values.
#' It leverages the `stringr::str_sort` function to sort the levels.
#'
#' @param col A factor or character vector.
#' @param rev Logical; if `TRUE`, reverse the order of levels.
#' @return A factor with reordered levels.
#'
#' @examples
#' temp_df = paste0("lab_", seq(1, 50, 2)) %>%
#' data.frame(label_unordered = .
#'            ,label_ordered = strg_numeric_order(.))
#'
#' temp_df %>% arrange(label_unordered) %>% print()
#' temp_df %>% arrange(desc(label_ordered)) %>% print()
#'
#' @export
strg_numeric_order <- function(col, rev = TRUE) {
  if (rev) {
    forcats::fct_relevel(
      col,
      unique(stringr::str_sort(col, numeric = TRUE))
    ) %>% forcats::fct_rev()
  } else {
    forcats::fct_relevel(
      col,
      unique(stringr::str_sort(col, numeric = TRUE))
    )
  }
}

