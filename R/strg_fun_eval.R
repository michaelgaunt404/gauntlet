#' Evaluate a string as a command
#'
#' Takes a string and evaluates it as an R command.
#'
#' @param text A character string representing the R command to evaluate.
#'
#' @return The result of evaluating the R command.
#' @importFrom rlang parse_expr
#'
#' @examples
#' strg_fun_eval("1 + 1")
#' strg_fun_eval("mean(c(1, 2, 3))")
#'
#' @export
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))
}
