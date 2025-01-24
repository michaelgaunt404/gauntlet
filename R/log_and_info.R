#' Log and print to console an info message.
#'
#' @param message string message to print.
#' @param logger a logger object made using log4r package.
#'
#' @return message printed to console and log file with logged message
#' @export
#'
#' @examples
#'
#' #none
log_and_info = function(message, logger){
  message(message)
  log4r::info(logger, message)}
