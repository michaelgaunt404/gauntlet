#' Ask user for T/F input with user defined prompt.
#'
#' This function prompts the user with a message asking whether or not they want
#' to perform a specific action. The function accepts the user's response and
#' handles invalid input by prompting the user again.
#'
#' @param prompt A character string indicating the action to be performed.
#' @return A logical value indicating whether or not the user elected to perform
#' the specified action. Returns TRUE if the user entered "Y", and FALSE if the
#' user entered "N".
#'
#' @examples
#' robust_prompt_used("continue with the analysis")
#'
#' @export
#'
#' @importFrom stringr str_glue
robust_prompt_used <- function(prompt) {
  message(str_glue("Do you want to {prompt}? Y/N"))
  response <- toupper(readline())

  if (response == "Y") {
    message(stringr::str_glue('You elected to {prompt}...'))
    return(response == "Y")
  } else if (response == "N") {
    message(stringr::str_glue('You did not elect to {prompt}...'))
    return(response == "Y")
  } else {
    message("Invalid response. Please enter Y or N.")
    robust_prompt_used(prompt = prompt)
  }
}

