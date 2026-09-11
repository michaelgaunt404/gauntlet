#' Source all R files in a folder and report failures
#'
#' Attempts to source all R scripts in a specified directory, capturing and
#' reporting any files that fail to load due to errors. Each file is sourced
#' into a fresh environment to avoid side effects between scripts.
#'
#' This function is primarily intended as a development and QA utility,
#' for example when validating that all files in a package's \code{R/}
#' directory parse and execute without error.
#'
#' @param folder Character string giving the directory containing R files.
#'   Defaults to \code{"./R/"}.
#' @param pattern Regular expression used to match files to source.
#'   Defaults to \code{"\\\\.R$"}.
#'
#' @return
#' If any files fail to load, returns a data frame with columns:
#' \describe{
#'   \item{File}{Path to the R file that failed.}
#'   \item{Error}{Error message produced while sourcing the file.}
#' }
#'
#' If all files load successfully, prints a success message and returns
#' \code{invisible(NULL)}.
#'
#' @details
#' Files are sourced using \code{source(file, local = new.env())} so that
#' objects created in one script do not affect others. This function does not
#' modify the calling environment.
#'
#' @examples
#' \dontRun{
#' # Validate that all R files in a package load cleanly
#' targets_source_R_files(folder = "./R")
#'
#' # Check only specific scripts
#' targets_source_R_files(folder = "./R", pattern = "targets")
#' }
#'
#' @export
targets_source_R_functions <- function(folder = "./R/", pattern = "\\.R$") {

  r_files <- list.files(folder, pattern = pattern, full.names = TRUE)

  failed_loads <- data.frame(
    File = character(),
    Error = character(),
    stringsAsFactors = FALSE
  )

  for (file in r_files) {
    result <- tryCatch(
      {
        source(file, local = new.env())
        NULL
      },
      error = function(e) {
        data.frame(
          File = file,
          Error = e$message,
          stringsAsFactors = FALSE
        )
      }
    )

    if (!is.null(result)) {
      failed_loads <- rbind(failed_loads, result)
    }
  }

  if (nrow(failed_loads) > 0) {
    message("Some files failed to load:")
    return(failed_loads)
  } else {
    message("All files loaded successfully.")
    return(invisible(NULL))
  }
}
