#' Make a clean date string of current date with simple punctuation and syntax.
#'
#' @return character string of date without bad punctuation
#' @export
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#'
#' clean_date()
#' }
strg_clean_date = function(){
  stringr::str_replace_all(stringr::str_remove_all(Sys.Date(), "[:punct:]"), " ", "_")
}
