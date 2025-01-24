#' Read multiple RDS files in singular folder.
#'
#' This function can be used to read many files at once.
#' Intended to read the same file that has been written multiple times using different date suffix.
#' Example: same data that is queried daily and needs to be saved bu analyzed together.
#'
#' @param data_location a string that defines which folder data is in - omit trailing `/`. Default is set to `/data/`.
#' @param specifically a string used to reprex filter files - use to limit which files are read.
#' @param clean a boolean to clean the data - BROKEN, always keep as default `FALSE`.
#' @param clean_string a string indicating a string that should be removed from file name when naming list entries.
#' @param latest a boolean (T/F) indicating if the function should only load the latest data - usually the latest by file name string date.
#' @param sheet a string indicating which sheet to import
#'
#' @return a named list of dataframes
#' @export
#'
#' @examples
#' #none
read_xlsx_allFiles <- function(data_location = "./data/", specifically = NULL,
                               clean = F, clean_string = NULL, sheet = sheet, latest = F) {
  file_list = list.files(data_location) %>%
    .[str_detect(., "xlsx")] %>%
    paste0(data_location, .) %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}

  if (clean){
    data_list =
      file_list %>%
      map(~readxl::read_xlsx(.x, sheet = sheet) %>%
            na_if("NULL") %>%
            janitor::remove_empty("cols") %>%
            janitor::clean_names()
      )
  } else {
    data_list =
      file_list %>%
      map(~readxl::read_xlsx(.x, sheet = sheet)
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".csv") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = paste0(file_list, sheet)
  }

  data_list
}
