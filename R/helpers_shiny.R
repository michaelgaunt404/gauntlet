#shiny specific=================================================================
list = list(closable = F,
            collapsed = F,
            collapsible = T,
            width = "100%",
            solidHeader = T,
            status = "primary")

#' Create a Shiny button with popover message
#'
#' This function generates a Shiny button along with a popover message. The
#' button, when pressed or hovered over (based on the specified trigger), displays
#' a popover message with the provided title and text. The default behavior is
#' to show an info-style message with a question mark icon.
#'
#' @param id The unique identifier for the button and popover.
#' @param title The title of the popover message.
#' @param text The text content of the popover message.
#' @param trigger The trigger for displaying the popover (default is "hover").
#' @seealso
#' \code{\link[shinyBS]{bsButton}}, \code{\link[shinyBS]{bsPopover}},
#' \code{\link[shiny]{icon}}
#'
#' @examples
#' \dontrun{
#'   shiny_bs_popover(id = "example_button", title = "Information",
#'                    text = "This is an example popover message.")
#' }
#'
#' @return A Shiny tagList containing the button and popover components.
#' @export
#' @import shinyBS
#' @import shiny
#'
#' @export
shiny_bs_popover <- function(id, title, text, trigger = "hover"){
  htmltools::tagList(
    shinyBS::bsButton(inputId = id, label = "Info", icon = shiny::icon("question"), style = "info", size = "small"),
    shinyBS::bsPopover(id = id, title = title,
                       content = text,
                       placement = "right",
                       trigger = trigger)
  )
}


#' Extract specific elements from the Shiny input list
#'
#' This function takes the master Shiny input list at any given time and
#' extracts the list elements by matching their names based on a suffix and/or
#' a string pattern. It provides a convenient way for users to obtain the
#' instantaneous values of specific input elements for troubleshooting purposes.
#'
#' @param input_list The Shiny input list to extract elements from.
#' @param suffix The optional suffix to filter input elements.
#' @param string The string pattern to filter input elements.
#' @param purrr Logical indicating whether to use purrr-style functions for filtering.
#' @seealso
#' \code{\link[stringr]{str_detect}}
#'
#' @examples
#' \dontrun{
#'   # Extract input elements with a suffix and matching a string pattern
#'   shiny_get_list_items(input_list = input, suffix = "numeric", string = "slider")
#' }
#'
#' @return A subset of the input list containing the matching elements.
#' @export
#' @import stringr
#'
#' @export
shiny_get_list_items <- function(input_list, suffix = NA, string, purrr = TRUE){
  if (purrr){
    input_list[names(input_list)[(stringr::str_detect(names(input_list), paste0("_", suffix)) + stringr::str_detect(names(input_list), string)) == 2]]
  } else {
    input_list[names(input_list)[stringr::str_detect(names(input_list), string)]]
  }
}


#' Create a standardized box for UI elements in Shiny applications
#'
#' This function generates a standard box for UI elements in Shiny applications.
#' It includes commonly used parameters, making it convenient to create consistent
#' box structures without repeatedly specifying the same inputs.
#'
#' @param title The title of the box (default is NA).
#' @param object_in_box The UI elements to be placed inside the box (default is NA).
#' @param collapsed Logical indicating whether the box is initially collapsed (default is FALSE).
#' @seealso
#' \code{\link[shinydashboard]{box}}
#'
#' @examples
#' \dontrun{
#'   # Create a standard box with a title and UI elements
#'   shiny_box_common(title = "Example Box", object_in_box = fluidRow(plotOutput("plot")))
#' }
#'
#' @return A standardized Shiny box with specified parameters.
#' @export
#' @import shinydashboard
#'
#' @export
shiny_box_common <- function(title = NA, object_in_box = NA, collapsed = FALSE){
  shinydashboard::box(title = title,
                      collapsed = collapsed,
                      collapsible = TRUE,
                      width = "100%",
                      solidHeader = TRUE,
                      status = "primary",
                      object_in_box)
}


#' Create a blank spacer row for spacing between UI elements
#'
#' This function generates a blank spacer row to create space between UI elements
#' in Shiny applications. It is a convenience function that inserts a fluid row
#' with an empty box having a specified height to create vertical space.
#'
#' @param size The height of the blank spacer row.
#' @seealso
#' \code{\link[shiny]{fluidRow}}, \code{\link[shinydashboard]{box}}
#'
#' @examples
#' \dontrun{
#'   # Create a spacer row with a specified height
#'   shiny_spacer_row(size = "20px")
#' }
#'
#' @return A blank spacer row with the specified height.
#' @export
#' @import shiny
#' @import shinydashboard
#'
#' @export
shiny_spacer_row <- function(size){
  shiny::fluidRow(
    shinydashboard::box(height = size)
  )
}


#' Create a convenient Shiny modal for pop-up messages
#'
#' This function wraps commonly used functions to create a Shiny modal for
#' pop-up messages. It allows you to link a triggering event with a message,
#' making it easier to display pop-up messages in Shiny applications with
#' reduced code repetition.
#'
#' @param trigger The triggering event that activates the modal.
#' @param msg The message content to be displayed in the modal.
#' @param size The size of the modal ("s" for small, "m" for medium, "l" for large).
#' @seealso
#' \code{\link[shiny]{observeEvent}}, \code{\link[shiny]{showModal}},
#' \code{\link[shiny]{modalDialog}}
#'
#' @examples
#' \dontrun{
#'   # Create a Shiny modal linked to a triggering event with a message
#'   shiny_modal(trigger = input$button_click, msg = "Process completed successfully!")
#' }
#'
#' @return NULL
#' @export
#' @import shiny
#'
#' @export
shiny_modal <- function(trigger, msg, size = "l"){
  shiny::observeEvent(trigger, {
    shiny::showModal(
      shiny::modalDialog(
        msg,
        size = size,
        easyClose = TRUE
      ))
  })
}

