#' Display an alert pop-up screen
#'
#' This function creates a pop-up screen that alerts the user that a process
#' has been completed. By default, it generates a big red pop-up screen with
#' the message "ALERT: SCRIPT HAS COMPLETED".
#'
#' @details
#' The alert pop-up screen is created using the ggplot2 package to generate
#' a simple plot with a label containing the alert message.
#'
#' @seealso
#' \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{labs}},
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{element_blank}},
#' \code{\link[ggplot2]{element_rect}}
#'
#' @examples
#' \dontrun{
#'   alert_me()
#' }
#'
#' @return NULL
#' @export
#' @import ggplot2
#'
#' @export
alert_me <- function(){
  # Create a red pop-up window
  windows(bg = 'red', width = 100, height = 75);

  # Create a data frame with alert text
  data.frame(x = 0, y = 0, text = "ALERT: SCRIPT HAS COMPLETED") %>%
    ggplot2::ggplot(
      ggplot2::aes(x, y, label = text)) +
    ggplot2::geom_label(size = 10) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x= ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "red"),
                   panel.background = ggplot2::element_rect(fill = "red")
    )
}
