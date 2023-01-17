#' Log and print to console a warn message.
#'
#' @param message string message to print.
#' @param logger a logger object made using log4r package.
#'
#' @return message printed to console and log file with logged message
#' @export
#'
#' @examples
#'
#' #none
log_and_warn = function(message, logger){
  message(message)
  log4r::warn(logger, message)}

#' Log and print to console an info message.
#'
#' @param message string message to print.
#' @param logger a logger object made using log4r package.
#'
#' @return message printed to console and log file with logged message
#' @export
#'
#' @examples
#'
#' #none
log_and_info = function(message, logger){
  message(message)
  log4r::info(logger, message)}

#' Log and print to console an info message.
#'
#' @param message string message to print.
#' @param logger a logger object made using log4r package.
#'
#' @return message printed to console and log file with logged message
#' @export
#'
#' @examples
#'
#' #none
log_and_fatal = function(message, logger){
  message(message)
  log4r::fatal(logger, message)}

#' Make a clean datetime string of current datetime with simple punctuation and syntax.
#'
#' @return character string of datetime using only underscores
#' @export
#'
#' @examples
#'
#' clean_datetime()
clean_datetime = function(){
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
clean_date = function(){
  str_remove_all(Sys.Date(), "[:punct:]") %>%
    str_replace_all(" ", "_")
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

#' Create a multi-line, printable console message from an object.
#'
#'  Function takes glue input and properly collpases it using "\n" to make it multi-line string.
#'  Using message without properly collapsing will create one long, flat message string.
#'  Nice when printing small tables.
#'
#' @param msg a string using str_glue syntax to print a df like object
#'
#' @return a string congaing "\n" to force string to print on multiple lines
#' @export
#'
#' @examples
#'
#' temp = data.frame(
#'highway = c("90 East", "405 Northbound", "101")
#',count = c(100, 205, 92)
#') %>%
#'  mutate(percent = round(count/sum(count), 2))
#'
#'str_glue("{temp$count} ({temp$percent}%) - {temp$highway}") %>%
#'  message()
#'
#'glue_collapse("{temp$count} ({temp$percent}%) - {temp$highway}") %>%
#'  message()
glue_collapse = function(msg){
  str_glue(msg) %>% paste0(collapse = '\n')
}




#end
