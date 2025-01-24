#' Send RDS Files to Google Drive
#'
#' This function programmatically sends RDS files saved in a user-provided directory
#' to a specified Google Drive location. The RDS files are converted to CSV format
#' before being uploaded to Google Drive.
#'
#' @param folder_with_files The local directory path containing the RDS files to be sent. Other file types can be in this location - they will be filtered out.
#' @param gdrive_folder The folder on the GDrive you want data saved to - needs to be top level, do not make it multi-level.
#'
#' @return None (invisibly returns NULL)
#'
#' @import here
#' @import googledrive
#' @importFrom purrr pmap
#' @importFrom readr read_rds write_csv
#'
#' @keywords RDS, Google Drive, file upload
#'
#' @export
#' @examples
#' \dontrun{
#' # Provide the directory path containing the RDS files
#' folder_with_files <- "path/to/rds/files"
#'
#' # Send the RDS files to Google Drive
#' gdrive_send_rds_files(folder_with_files)
#' }
gdrive_send_rds_files = function(folder_with_files, gdrive_folder){

  message("Checking if folder exists already and if supplied name is unique")

  tmp_item = googledrive::drive_find(pattern = gdrive_folder, type = "folder") %>%
    filter(name == gdrive_folder)

  if (nrow(tmp_item) == 0) {
    message("No Gdrive objects returned that match user provided name - creating folder now")
    googledrive::drive_mkdir(name = gdrive_folder)

  } else {
    message("Folder already exists - skipping folder creation")
  }

  message("Reading files located in supplied local folder......")

  files = here::here(folder_with_files) %>%
    list.files()

  files = files[str_detect(files, "rds")]

  message(str_glue("There are {length(files)} RDS files in folder location"))

  full_path_files = paste0(folder_with_files, "/", files)

  #actual write once path and folder checked out and created
  list(
    full_path_files
    ,files) %>%
    purrr::pmap(~{

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
