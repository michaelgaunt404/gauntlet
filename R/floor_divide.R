

#' floor_divide
#'
#' @param value A numeric value or dataframe column to be floor divided
#' @param floor A value to floor divide by
#'
#' @return A numeric value or vector
#' @export
#'
#' @examples
#'
#' temp_index = rnorm(10, 100, 100)
#' floor_divide(temp_index, 5)
#' floor_divide(temp_index, 50)
#' floor_divide(temp_index, 100)

floor_divide = function(value, floor){
  (value %/% floor)*floor
}




