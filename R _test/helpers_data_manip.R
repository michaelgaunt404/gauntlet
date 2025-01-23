#' Floor divide a vector by some value.
#'
#' This function can be applied to a vector to bin values by a floor divide value.
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

#' Round to 0 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt0(rnorm(10))
dgt0 = function(x){
  round(x, 0)
}

#' Round to 1 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt1(rnorm(10))
dgt1 = function(x){
  round(x, 1)
}

#' Round to 2 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt2(rnorm(10))
dgt2 = function(x){
  round(x, 2)
}

#' Round to 3 digits
#'
#' Use this function to round a vector.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' dgt3(rnorm(10))
dgt3 = function(x){
  round(x, 3)
}

#' Rescale a vector with first entry.
#'
#' Use this function to rescale a vector using the first entry
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' @examples
#'
#' \dontrun{
#'data.frame(group = c(rep("A", 4), rep("B", 4))
#'           ,order = c(1:4, 1:4)) %>%
#'  mutate(value = 2*order+rnorm(8, 5)) %>%
#'  group_by(group) %>%
#'  mutate(value_corrected = crrct0(value))
#'  }
crrct0 = function(x){
  x-x[1]
}

#evaluate string functions
#eg works if var is "input$something > num" in shiny
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))
}


#cleans df using common operations
quick_clean = function(df, na_marker){
  df %>%
    na_if(na_marker) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("cols", "rows"))
}

#' Limit to Non-Negative Values
#'
#' This function takes a numeric vector and replaces any negative values with zero.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with non-negative values.
#'
#' @examples
#' lmt0(c(5, 6, 2, 0, -1))
#'
#' @export
lmt0 <- function(x) {
  ifelse(x < 0, 0, x)
}


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











#end
