#' Make a clean datetime string of current datetime with simple punctuation and syntax.
#'
#' @return character string of datetime using only underscores
#' @export
#' @importFrom stringr str_replace_all str_trunc
#'
#' @examples
#'
#' clean_datetime()
strg_clean_datetime = function(strip = FALSE){
  temp = stringr::str_replace_all(stringr::str_remove_all(Sys.time(), "[:punct:]"), " ", "_")
  if(strip){temp = stringr::str_trunc(temp, width = 15, side = "right", ellipsis = "")}
  return(temp)
}
