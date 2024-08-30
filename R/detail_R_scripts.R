#' Extract and Summarize Script Metadata from R Scripts
#'
#' This function searches a specified directory for R scripts and extracts
#' metadata from the top of each script. The metadata includes the script's
#' description, author, and additional details (referred to as README information).
#'
#' **Note:** The metadata must follow a very specific format that is prescribed in
#' the 'gauntlet' package, which this function is a part of. The function assumes
#' that the format used is consistent with this prescribed structure. The format is listed below in the example.
#'
#' @param dir A character string specifying the directory to search for R scripts.
#' The function will look for files with a `.R` extension within this directory.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{dir}{The directory where the script is located.}
#'   \item{script}{The name of the script file.}
#'   \item{author}{The author of the script, extracted from the script's metadata.}
#'   \item{description}{A high-level description of what the script does, extracted
#'   from the script's metadata.}
#'   \item{details}{Additional details or README information about the script,
#'   extracted from the script's metadata.}
#' }
#'
#' @importFrom dplyr arrange
#' @importFrom purrr map_df
#' @importFrom stringr str_detect str_pad
#'
#' @examples
#' \dontrun{
#' # Create an example R script with the specific metadata format
#' script_content <-
#'   "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'    #
#'    # DESC: A high-level description of what this script does.
#'    #
#'    # By: mike gaunt, michael.gaunt@wsp.com
#'    #
#'    # README: Additional details re/ script
#'    #-------- [[insert brief readme here]]
#'    #
#'    # *please use 80 character margins
#'    #
#'    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'    mtcars %>% print"
#'
#' # Write the script to the temporary directory
#' temp_dir = tempdir()
#' writeLines(script_content, file.path(temp_dir, "example_script.R"))
#'
#' # Run the describe_R_scripts function on the temporary directory
#' detail_R_scripts(temp_dir)
#' }
#'
#' @export
detail_R_scripts <- function(dir){

  scripts <- list.files(dir, full.names = TRUE) %>%
    .[stringr::str_detect(., "\\.R$")]

  scripts %>%
    purrr::map_df(~{

      tryCatch({
        lines <- readLines(.x) %>%
          head(30)

        index_top <- which(stringr::str_detect(lines, "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))[1:2]

        lines_top <- lines %>%
          .[index_top[1]:index_top[2]] %>%
          .[which(. != "#")] %>%
          .[which(!stringr::str_detect(., "#~"))] %>%
          .[which(!stringr::str_detect(., "# \\*please"))]

        temp_data <- data.frame(
          dir = dir,
          script = basename(.x),
          author = lines_top %>% .[stringr::str_detect(., "# By: ")] %>% gsub("# By: ", "\\1", .),
          description = lines_top %>% .[stringr::str_detect(., "# DESC: ")] %>% gsub("# DESC: ", "\\1", .),
          details = lines_top %>% .[stringr::str_detect(., "# README: |#-------- ")] %>%
            gsub("# README: |#-------- ", "\\1", .) %>%
            .[!stringr::str_detect(., "insert brief readme")] %>%
            stringr::str_pad(side = "right", pad = " ", width = 120) %>%
            paste(collapse = "")
        )

        return(temp_data)

      }, error = function(err) {
        print(paste("Error occurred:", err$message))

        temp_data <- data.frame(
          dir = dir,
          script = basename(.x),
          author = "z_NULL",
          description = "NULL",
          details = "NULL"
        )

        return(temp_data)
      })
    }) %>%
    dplyr::arrange(author, script)

}
