#' Make new R script from template.
#'
#' This function copies an R script template from the `gauntlet` package and writes it to a new location as defined by the user.
#' Write-to folder locations must exist before running this code.
#'
#' @param folder a string name
#' @param file_name a string name (include .r extentsion)
#'
#' @return a script in the location and with the name choosen by the user
#' @importFrom utils file.edit
#' @importFrom here here
#' @importFrom stringr str_glue
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

  utils::file.edit(file_location)
}


#' Make new Rmd script from template.
#'
#' This function copies an Rmd script template from the `gauntlet` package and writes it to a new location as defined by the user.
#' Write-to folder locations must exist before running this code.
#'
#' @param folder a string name (defaults to `analysis` folder)
#' @param file_name a string name (include .Rmd extension)
#'
#' @return an Rmd in the location and with the name chosen by the user
#' @importFrom utils file.edit
#' @importFrom here here
#' @importFrom stringr str_glue
#' @export
#'
#' @examples
#' #none
make_new_rmd = function(folder = "analysis", file_name = NULL){
  folder_location = here::here(folder)
  file_location = here::here(stringr::str_glue("{folder}/{file_name}"))

  stopifnot("Please provide a filename (include .Rmd extentsion)..." = !is.null(file_name)
            ,"Folder location does not exist, please make it first..." = !exists(folder_location))

  file.copy(
    system.file("scripts", "zz_template_markdown_analysis.Rmd", package="gauntlet")
    ,file_location
  )

  utils::file.edit(file_location)
}


#' Check and create a directory path
#'
#' This function checks if the directory path exists and creates it if it doesn't.
#'
#' @param dir_path A string specifying the path of the directory to check and create if necessary.
#'
#' @return None
#' @export
#'
#' @examples
#' check_dir_path("data/raw")
#' check_dir_path("data/processed")
check_dir_path <- function(dir_path) {
  dir_path = here::here(dir_path)

  if (!dir.exists(dir_path)) {
    message(paste0("The file location\n'", dir_path, "'\nDOES NOT exist.\n"))
    create_dir = robust_prompt_used(prompt = "create this directory (Y) or or create at folder with a different path (N)")

    if (create_dir) {
      dir.create(dir_path, recursive = TRUE)
      message(paste0("The directory\n'", dir_path, "'\nhas been created.\n"))
    } else {
      new_dir_path <- readline(prompt = "Enter a new file location: ")
      dir_path <- new_dir_path
      if (!file.exists(dir_path)) {
        check_dir_path(dir_path)
      } else {
        message(paste0("The file location '", dir_path, "' exists.\n"))
      }
    }
  } else {
    message(paste0("The file location '", dir_path, "' exists.\n"))
  }
}


#' Create a directory with a prefix and date
#'
#' This function creates a directory with a specified prefix and the current date.
#'
#' @param directory_location A string specifying the location of the directory to create.
#' @param prefix A string specifying the prefix to use for the directory name.
#'
#' @return A string specifying the path of the created directory.
#'
#' @export
#'
#' @examples
#' make_dir_prfx_date("data/processed", "my_data_")
#' make_dir_prfx_date("data/raw", "my_data_")
make_dir_prfx_date <- function(directory_location, prefix) {
  date = gauntlet::strg_clean_date()
  directory_path = here::here(directory_location, paste0(prefix, date))
  message(str_glue("{gauntlet::strg_make_space_2()}The data acquired from this query will be stored here:\n{directory_path}"))
  if (dir.exists(directory_path)){
    message("This directory already exists\nyou need to supply a suffix to make it unique...")
    additional_suffix <- readline(prompt="Enter additional suffix: ")
    directory_path = paste0(directory_path, additional_suffix)
  } else {
    check_suffix = robust_prompt_used("to supply and additional suffix to the directroy name")
    if (check_suffix){
      additional_suffix <- readline(prompt="Enter additional suffix (press Enter for none): ")
      directory_path = paste0(directory_path, additional_suffix)
    }
  }

  dir.create(directory_path)

  message(paste0("Subdirectory\n", directory_path, "\nwas created"))
  return(directory_path)
}


#' Convert named list to string
#'
#' This function converts a named list to a string, with each element of the list on a separate line and with its name.
#'
#' @param my_list A named list to be converted to a string.
#'
#' @return A string with each element of the list on a separate line and with its name.
#'
#' @examples
#' my_list <- list(a = 1, b = 2, c = 3)
#' print_named_list(my_list)
#'
#' @export
print_named_list <- function(my_list) {
  my_string <- ""

  for (i in seq_along(my_list)) {
    my_string <- paste(my_string, names(my_list)[i], ": ", my_list[[i]], "\n", sep = "")
  }

  return(my_string)
}

