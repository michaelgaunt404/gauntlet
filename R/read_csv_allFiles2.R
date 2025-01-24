
#' Read multiple csv files in singular folder.
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
#'
#' @return a named list of dataframes
#' @export
#'
#' @examples
#' #none
read_csv_allFiles2 <- function(data_location = "data", specifically = NULL,
                               clean = F, clean_string = NULL, latest = F) {

  file_list = here(data_location) %>%
    list.files() %>%
    .[str_detect(., "csv")] %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}


  if (clean){
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             data.table::fread() %>%
             na_if("NULL") %>%
             janitor::remove_empty("cols") %>%
             janitor::clean_names()
      )
  } else {
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             data.table::fread()
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".csv") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}
