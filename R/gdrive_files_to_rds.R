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
#' @import googledrive
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom here here
#' @export
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
