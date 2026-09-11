#' Print a timestamped, leveled console message
#'
#' Lightweight logger used throughout the `dirOrg_*` functions instead of
#' `cli` so the workflow has no additional messaging dependency.
#'
#' @param ... One or more objects to be pasted together and printed, as in
#'   [base::paste0()].
#' @param level One of `"info"`, `"warn"`, `"success"`, or `"error"`.
#'   Controls the tag prefixed to the message. Defaults to `"info"`.
#'
#' @return Invisibly `NULL`. Called for its side effect of printing a
#'   message via [base::message()].
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' dirOrg_say("Starting scan...")
#' dirOrg_say("Drive is nearly full", level = "warn")
#' }
dirOrg_say <- function(..., level = c("info", "warn", "success", "error")) {
  level <- match.arg(level)
  tag <- switch(level,
                info    = "[INFO]",
                warn    = "[WARN]",
                success = "[ OK ]",
                error   = "[FAIL]"
  )
  message(sprintf("%s %s  %s", format(Sys.time(), "%H:%M:%S"), tag, paste0(...)))
  invisible(NULL)
}
