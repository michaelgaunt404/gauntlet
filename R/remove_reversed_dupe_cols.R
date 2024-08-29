remove_reversed_dupe_cols = function(df, col1, col2) {
  # Create a sorted version of the two columns
  sorted_df = data.frame(
    var1 = pmin(df[[col1]], df[[col2]]),
    var2 = pmax(df[[col1]], df[[col2]])
  )

  # Add the sorted columns back to the original data frame
  df_sorted = cbind(sorted_df, df)

  # Remove duplicates based on the sorted columns
  df_unique = df_sorted[!duplicated(sorted_df), ]

  # Return the data frame without the additional sorted columns
  return(df_unique[, -(1:2)])
}


# data.frame(
#   var_1 = c(1, 2, 3, 4, 2) %>% paste0("street_", .)
#   ,var_2 = c(2, 1, 4, 3, 1) %>% paste0("street_", .)
# ) %>%
#   remove_reversed_dupe_cols("var_1", "var_2")
