#' Parse Command Line Arguments
#'
#' Parses a vector of command-line arguments and returns a named list of argument-value pairs.
#' Arguments are expected to follow the format `--key=value`.
#'
#' @param args A character vector of command-line arguments.
#' Each argument should start with `--` followed by a key and an optional value, separated by `=`.
#'
#' @return A named list where the names are the keys from the arguments (without `--`),
#' and the values are the corresponding values from the arguments. If an argument
#' does not contain `=`, it will not be included in the returned list.
#'
#' @examples
#' # Example usage:
#' args <- c("--name=John", "--age=30", "--country=USA")
#' parsed_args <- strg_parse_args(args)
#' print(parsed_args)
#' # Output: list(name = "John", age = "30", country = "USA")
#'
#' @export
strg_parse_args <- function(args) {
  parsed_args <- list()
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=")[[1]]
      parsed_args[[parts[1]]] <- parts[2]
    }
  }
  return(parsed_args)
}
