#' Make new R script from template.
#'
#' This function copies an R script template from the `gauntlet` package and writes it to a new location as defined by the user.
#' Write-to folder locations must exist before running this code.
#'
#' @param folder a string name
#' @param file_name a string name (include .r extentsion)
#'
#' @return a script in the location and with the name choosen by the user
#' @export
#'
#' @examples
#' #none
make_new_script = function(folder = "code", file_name = NULL){
  folder_location = here::here(folder)
  file_location = here::here(stringr::str_glue("{folder}/{file_name}"))

  stopifnot("Please provide a filename (include .r extentsion)..." = !is.null(file_name)
            ,"Folder location does not exist, please make it first..." = !exists(folder_location))

  file.copy(
    system.file("scripts", "zz_emptyScriptTemplate_CODE.r", package="gauntlet")
    ,file_location
  )

  file.edit(file_location)
}

