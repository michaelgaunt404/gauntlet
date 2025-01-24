





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
#' \dontrun{
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
#'  }
glue_collapse = function(msg){
  str_glue(msg) %>% paste0(collapse = '\n')
}


gntlt_error_msg = function(err) {
  message("Error occurred:")
  message(err$message)
  message("Returning NA value...")
  return(NA)
}

#end
