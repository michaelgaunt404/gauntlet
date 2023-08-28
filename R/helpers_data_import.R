
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
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}

  if (clean){
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             readRDS() %>%
             na_if("NULL") %>%
             janitor::remove_empty("cols") %>%
             janitor::clean_names()
      )
  } else {
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             readRDS()
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".rds") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}

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


#' Send RDS Files to Google Drive
#'
#' This function programmatically sends RDS files saved in a user-provided directory
#' to a specified Google Drive location. The RDS files are converted to CSV format
#' before being uploaded to Google Drive.
#'
#' @param folder_with_files The directory path containing the RDS files to be sent.
#'
#' @return None (invisibly returns NULL)
#'
#' @examples
#' \dontrun{
#' # Provide the directory path containing the RDS files
#' folder_with_files <- "path/to/rds/files"
#'
#' # Send the RDS files to Google Drive
#' gdrive_send_rds_files(folder_with_files)
#' }
#'
#' @import here
#' @import googledrive
#' @importFrom purrr pmap
#' @importFrom readr read_rds write_csv
#'
#' @keywords RDS, Google Drive, file upload
#'
#' @export
gdrive_send_rds_files = function(folder_with_files){
  files = here::here(folder_with_files) %>%
    list.files()

  full_path_files = paste0(folder_with_files, "/", files)

  list(
    full_path_files
    ,files) %>%
    pmap(~{

      tryCatch({

        temp_file = tempfile(fileext = ".csv")
        temp_data = readr::read_rds(.x)
        readr::write_csv(temp_data, temp_file)

        # googl
        googledrive::drive_put(
          temp_file
          ,name = paste0(gsub("\\.rds.*", "\\1", .x), ".csv")
          ,type = "csv"
          # ,path = as_id(query_table_sel[['gdrive']]) #I guess you don't really need this
        )

      }, error = function(err) {
        message("An error orccured....")
        print(err$message)
      })
    })
}


#' Get and Save Files from Google Drive
#'
#' This function programmatically downloads CSV files stored in your Google Drive,
#' saves them temporarily as CSV files, and then converts and saves them as RDS
#' objects. The function is designed to work with singular data frames that have
#' one record each. If working with larger data frames, you can break them into
#' singular records and use `purrr::map` to process them.
#'
#' @param data_to_fetch A data frame containing information about the files to fetch
#'   from Google Drive. Each row should represent a file and include columns 'id'
#'   (file ID on Google Drive) and 'name' (file name).
#' @param save_location The directory path where the downloaded files should be saved.
#' @param save_format The format in which to save the files. Default is ".rds".
#'
#' @return None (invisibly returns NULL)
#'
#' @examples
#' \dontrun{
#' # Provide the data frame with file information
#' files_to_fetch <- data.frame(
#'   id = c("file_id_1", "file_id_2"),
#'   name = c("file1.csv", "file2.csv")
#' )
#'
#' # Specify the save location
#' save_location <- "path/to/save"
#'
#' # Get and save files from Google Drive
#' gdrive_get_files(files_to_fetch, save_location)
#' }
#'
#' @import googledrive
#' @importFrom data.table fread
#' @importFrom purrr map
#' @export
gdrive_files_to_rds <- function(data_to_fetch, save_location, save_format = ".rds") {
  tryCatch({
    temp_file <- tempfile(fileext = ".csv")

    googledrive::drive_download(
      file = googledrive::as_id(data_to_fetch$id),
      path = temp_file, overwrite = TRUE
    )

    temp_data <- data.table::fread(temp_file)
    file_names <- gsub(".*/(.*?)\\.csv$", "\\1", data_to_fetch$name)
    saveRDS(
      temp_data,
      here::here(save_location, paste0(file_names, save_format))
    )

  }, error = function(err) {
    message("An error occurred...")
    print(err$message)
  })
}


















#end
