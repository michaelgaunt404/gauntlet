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
#'
#' @return a named list of dataframes
#' @export
#'
#' @examples
#' #none
read_rds_allFiles <- function(data_location = "data", specifically = NULL
                              , clean = F, clean_string = NULL, latest = F) {
  #data location: string that defines which folder data is in - omit trailing '/'
  #specifically: index that filters full file list to specific items
  #clean: cleans up the imported data but is buggy
  #latest: just loads singular data - usually the latest by file name string date

  file_list = here(data_location) %>%
    list.files() %>%
    .[str_detect(., "rds")] %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[readr::parse_number(.) == max(readr::parse_number(.))] else .}

  if (clean){
    data_list =
      tidyr::crossing(data_location, file_list) %>%
      purrr::pmap(~here(.x, .y) %>%
                    readRDS() %>%
                    na_if("NULL") %>%
                    janitor::remove_empty("cols") %>%
                    janitor::clean_names()
      )
  } else {
    data_list =
      tidyr::crossing(data_location, file_list) %>%
      purrr::pmap(~here::here(.x, .y) %>%
                    readRDS()
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~stringr::str_remove(.x, data_location) %>%
            stringr::str_remove(".rds") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}
