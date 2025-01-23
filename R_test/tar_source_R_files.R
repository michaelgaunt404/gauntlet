tar_source_R_files = function(folder = "./R/", pattern = "\\.R$") {
  # List all R files in the folder matching the pattern
  r_files <- list.files(folder, pattern = pattern, full.names = TRUE)

  # Initialize a data frame to store files that fail to load
  failed_loads <- data.frame(File = character(), Error = character(), stringsAsFactors = FALSE)

  # Loop through each file and try to source it
  for (file in r_files) {
    result <- tryCatch(
      {
        source(file, local = new.env())
        NULL  # If successful, return NULL
      },
      error = function(e) {
        # If an error occurs, store the file and error message
        data.frame(File = file, Error = e$message, stringsAsFactors = FALSE)
      }
    )

    # If an error was captured, append it to the failed_loads data frame
    if (!is.null(result)) {
      failed_loads <- rbind(failed_loads, result)
    }
  }

  # Check if any files failed to load and return the result
  if (nrow(failed_loads) > 0) {
    print("Some files failed to load:")
    return(failed_loads)
  } else {
    print("All files loaded successfully.")
    return(invisible(NULL))
  }
}
