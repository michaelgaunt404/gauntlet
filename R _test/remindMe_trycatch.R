#' Print basic to console basic tryCatch template
#'
#' @return None (prints to console).
#' @importFrom stringr str_glue
#'
#' @examples
#' remindMe_trycatch()
#'
#' @export
remindMe_trycatch = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing two examples of common tryCatches{gauntlet::strg_make_space_2(last = F)}"))

  cat('
p = 8
p = "string"

tryCatch({
  8 + p
}, error = function(err) {
  message("An error orccured....")
  print(err$message)
})


tt = tryCatch({
  temp = 8 + p
  return(temp)
}, error = function(err) {
  print(err$message)
  temp = NULL
  return(temp)
})
print(tt)
'
  )
}






