#' Remove Reversed Duplicate Rows from a Data Frame
#'
#' This function removes rows from a data frame where two specified columns have
#' reversed duplicate values. It identifies pairs of columns that contain the same
#' values but in reverse order, and retains only one instance of each pair.
#'
#' This is useful for cleaning up data where the order of elements in the two columns
#' doesn't matter, such as in undirected networks or paired observations.
#'
#' @param df A data frame containing the data to be processed.
#' @param col1 The name of the first column to check for reversed duplicates.
#' @param col2 The name of the second column to check for reversed duplicates.
#'
#' @return A data frame with reversed duplicate rows removed.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   var_1 = c(1, 2, 3, 4, 2) %>% paste0("street_", .),
#'   var_2 = c(2, 1, 4, 3, 1) %>% paste0("street_", .)
#' )
#'
#' # Remove reversed duplicates
#' cleaned_df <- remove_reversed_dupe_cols(df, "var_1", "var_2")
#'
#' @export
remove_reversed_dupe_cols <- function(df, col1, col2) {
  # Create a sorted version of the two columns
  sorted_df <- data.frame(
    var1 = pmin(df[[col1]], df[[col2]]),
    var2 = pmax(df[[col1]], df[[col2]])
  )

  # Add the sorted columns back to the original data frame
  df_sorted <- cbind(sorted_df, df)

  # Remove duplicates based on the sorted columns
  df_unique <- df_sorted[!duplicated(sorted_df), ]

  # Return the data frame without the additional sorted columns
  return(df_unique[, -(1:2)])
}






