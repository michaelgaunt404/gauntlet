#' Make rolling MAD (median absolute deviation) values for dataframe attribute.
#'
#' @param data a data frame with a timerseries attribute - attribute must be named value to work
#' @param window an integer indicating width of window
#'
#' @return a dataframe with rolling MAD statistic attribute
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate pull
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' \dontrun{
#'test_data = data.frame(index = 1:53,
#'                       value = c(rnorm(25, 10, 3)
#'                                 ,rnorm(3, 100, 3)
#'                                 ,rnorm(25, 10, 3)))
#'test_data %>%
#'  ts_mad(window = 7)
#'  }
ts_mad = function(data, window = 20){

  data = data %>%
    dplyr::mutate(index_forced = row_number())

  temp_list = list()
  for (i in 1:(nrow(data)-window)){

    #make window subset
    temp_df = data[i:(i+window),]

    #make distance matrix for window
    window_values = temp_df %>%
      dplyr::pull(value)

    temp_list[[i]] = temp_df %>%
      utils::tail(1) %>%
      dplyr::mutate(mad_value = roll_mad(window_values, last = T))

  }

  #collapses summary list objects
  temp_df =
    temp_list %>%
    data.table::rbindlist(fill=TRUE) %>%
    select(!index_forced)


  return(temp_df)
}
