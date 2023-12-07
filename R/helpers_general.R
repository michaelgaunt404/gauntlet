#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#helper to make pop up window to alert user
alert_me = function(){
  windows(bg = 'red', width = 100, height = 75);

  data.frame(x = 0, y = 0, text = "ALERT: SCRIPT HAS COMPLETED") %>%
    ggplot(aes(x, y, label = text)) + geom_label(size = 10) +
    labs(x = "", y = "") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "red"),
          panel.background = element_rect(fill = "red")
    )
}

#helper to make floor divides
# #generally used to make bins
# floor_divide = function(value, floor){
#   (value %/% floor)*floor
# }

#plusEqual operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))


#takes quick counts for columns and their items
quick_value_count = function(df, rows, column, filter = NA){
  if(is.na(filter)){
  df[rows,] %>%
    select(all_of(column)) %>%
    nrow()
  } else {
    df[rows,] %>%
      select(all_of(column)) %>%
      filter(!!as.symbol(column) == filter) %>%
      nrow()
  }
}



#function: formats numbers to pretty strings

#shiny specific=================================================================
list = list(closable = F,
            collapsed = F,
            collapsible = T,
            width = "100%",
            solidHeader = T,
            status = "primary")

quick_bs = function(id, title, text, trigger = "hover"){
  tagList(
    bsButton(inputId = id, label = "Info", icon = icon("question"), style = "info", size = "small"),
    bsPopover(id = id, title = title,
              content = text,
              placement = "right",
              trigger = trigger)
  )
}


#takes master shiny input list and extracts list elements by name match
#string can take "|" operator
get_list_items = function(input_list, suffix = NA, string, purrr = T){
  if (purrr){
    input_list[names(input_list)[(str_detect(names(input_list) , paste0("_", suffix))+str_detect(names(input_list), string))==2]]
  } else {
    input_list[names(input_list)[str_detect(names(input_list), string)]]
  }
}

#makes common box that works for most purposes
#objects need to be in a list
# boxPlus_common = function(title = NA, object_in_box = NA, collapsed = F){
#   boxPlus(title = title,
#           closable = F,
#           collapsed = collapsed,
#           collapsible = T,
#           width = "100%",
#           solidHeader = T,
#           status = "primary",
#           object_in_box)
# }

#makes common box that works for most purposes
#objects need to be in a list
box_common = function(title = NA, object_in_box = NA, collapsed = F){
  box(title = title,
      # closable = F,
      collapsed = collapsed,
      collapsible = T,
      width = "100%",
      solidHeader = T,
      status = "primary",
      object_in_box)
}

#creates an empty row of a given height
#for shiny usage
spacer_row = function(size){
  fluidRow(box(height = size))
}

#creates function for modals
modal = function(trigger, msg){
  observeEvent(trigger, {
    showModal(modalDialog(
      msg,
      size = "l",
      easyClose = TRUE
    ))
  })
}

# #automates aggregating counts and percents for different groupings of data
# count_percent_zscore = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
#                                 col = count, prefix = NULL,
#                                 rnd = NULL, cntr_scl = FALSE){
#   #summarizing column has to be 'count'
#
#   tmp = data %>%
#     group_by(across({{grp_c}})) %>%
#     summarise(count = sum({{col}})) %>%
#     ungroup() %>%
#     group_by(across({{grp_p}})) %>%
#     mutate(percent = ({{col}}/sum({{col}})) %>%
#              { if (!is.null(rnd)) round(., rnd) else .}
#     ) %>%
#     ungroup() %>%
#     { if (cntr_scl) (.) %>%
#         group_by(across({{grp_z}})) %>%
#         mutate(zscore = as.vector(scale({{col}})))
#       else .}
#
#   if (is.null(prefix)){
#     tmp
#   } else {
#     newname1 = str_glue("{prefix}_count")
#     newname2 = str_glue("{prefix}_percent")
#     rename(tmp, !!newname1 := count, !!newname2 := percent)
#   }
# }

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
#' @seealso \code{\link{readline}}, \code{\link{str_glue}}
#'
#' @importFrom stringr str_glue
#'
#' @keywords robust prompt, user input
#'
robust_prompt_used <- function(prompt) {
  message(str_glue("Do you want to {prompt}? Y/N"))
  response <- toupper(readline())

  if (response == "Y") {
    message(str_glue('You elected to {prompt}...'))
    return(response == "Y")
  } else if (response == "N") {
    message(str_glue('You did not elect to {prompt}...'))
    return(response == "Y")
  } else {
    message("Invalid response. Please enter Y or N.")
    robust_prompt_used(prompt = prompt)
  }
}

#' Flatten a named list
#'
#' This function takes a named list as input and returns a data frame with two
#' columns: the name of each list element (including parent element names), and
#' its corresponding value. If the input is not a list, the function returns a
#' data frame with a single row containing the value and its parent name (if
#' provided).
#'
#' @param lst A named list to be flattened into a data frame.
#' @param parent_name A character string indicating the name of the parent list
#' element. This argument is optional and should only be provided if the input
#' list is a child element of a larger parent list. By default, the parent name
#' is set to an empty string.
#'
#' @return A data frame with two columns: the name of each list element (including
#' parent element names), and its corresponding value.
#'
#' @examples
#' test_list <- list(a = 1, b = list(c = 2, d = 3))
#' flatten_named_list(test_list)
#'
#' @export
#'
#' @keywords named list, data frame, recursion
#'
flatten_named_list <- function(lst, parent_name = "") {
  # If input is not a list, return a data frame with the single value
  if (!is.list(lst)) {
    return(data.frame(name = parent_name, value = lst))
  }
  # If input is a list, recursively flatten its elements and combine into a data frame
  else {
    child_dfs <- lapply(names(lst), function(name) {
      child_name <- if (parent_name == "") name else paste0(parent_name, ".", name)
      flatten_named_list(lst[[name]], child_name)
    })
    do.call(rbind, child_dfs)
  }
}
