


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
#'data.frame(group = c(rep("A", 4), rep("B", 4))
#'           ,order = c(1:4, 1:4)) %>%
#'  mutate(value = 2*order+rnorm(8, 5)) %>%
#'  group_by(group) %>%
#'  mutate(value_corrected = crrct0(value))
crrct0 = function(x){
  x-x[1]
}

#plusEqual operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#evaluate string functions
#eg works if var is "input$something > num" in shiny
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))
}

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

#cleans df using common operations
quick_clean = function(df, na_marker){
  df %>%
    na_if(na_marker) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("cols", "rows"))
}

pretty_char = function(col){
  col %>%
    stringr::str_replace_all(., "_", " ") %>%
    stringr::str_to_title(.)
}


#corrects column to start with zero
# crrct0 = function(x){
#   x-x[1]
# }

#function: changes negative to zero
lmt0 = function(x){
  ifelse(x<0, 0, x)
}

#function: formats numbers to pretty strings
pretty_num = function(vector, rnd = 0){
  digit = vector %>%
    dgt0()

  case_when(str_length(abs(digit))>9~paste0(round((digit/1e6), rnd), "B")
            ,str_length(abs(digit))>6~paste0(round((digit/1e6), rnd), "M")
            ,str_length(abs(digit))>3~paste0(round((digit/1e3), rnd), "k")
            ,str_length(abs(digit))>0~paste(digit)
            ,T~"Undefined")
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

#automates aggregating counts and percents for different groupings of data
#' Group by count, sum, percent and zscore.
#'
#' This function can be used to perform a number of mathematical operation on a dataframe.
#' Count or sum numerical values by group, get percent breakdown of counts or sums by group, and get optional zscore of values.
#'
#' @param data a dataframe
#' @param grp_c a vector of columns to group counting operation with - do not quote columns
#' @param grp_p a vector of columns to group percent calculation operation with - do not quote columns
#' @param grp_z a vector of columns to group zscore calculation operation with - do not quote columns
#' @param col a column to count or sum - do not quote column
#' @param prefix a string used to prefix calculated columns with - leave empty if you do not want a prefix
#' @param rnd integer indcating how many digits you want calculated columns to be rounded to - leave empty if you do not want rounding
#' @param cntr_scl (`TRUE`/`FALSE`) boolean to indicate if zscore should be calculated - default is `FALSE`
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'temp_data = data.frame(group = c(rep("A", 4), rep("B", 4))
#'                       ,order = c(1:4, 1:4)) %>%
#'  mutate(value = 2*order+rnorm(8, 5)
#'         ,count = 1)
#'
#'count_percent_zscore(temp_data, grp_c = c(group), grp_p = c(), col = count)
#'
#'count_percent_zscore(temp_data, grp_c = c(order), grp_p = c(), col = value)
count_percent_zscore = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                col , prefix = NULL, rnd = NULL, cntr_scl = FALSE){
  tmp = data %>%
    group_by(across({{grp_c}})) %>%
    summarise(count = sum({{col}})) %>%
    ungroup() %>%
    group_by(across({{grp_p}})) %>%
    mutate(percent = (count/sum(count)) %>%
             { if (!is.null(rnd)) round(., rnd) else .}
    ) %>%
    ungroup() %>%
    { if (cntr_scl) (.) %>%
        group_by(across({{grp_z}})) %>%
        mutate(zscore = as.vector(scale(count)))
      else .}

  if (is.null(prefix)){
    tmp
  } else {
    newname1 = str_glue("{prefix}_count")
    newname2 = str_glue("{prefix}_percent")
    rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
}



#' Calculate quantiles for groups of pre-aggregated metrics.
#'
#' Use this function to calculate qunatiles for data that has already been aggregated and collapsed into counts. Avoids the need to expand data then compute quantiles.
#'
#' @param df a dataframe of data
#' @param value string of column that qunatiles will be calculated for
#' @param weight string of column that will be used to calculate quantiles with
#' @param quantiles vector of quantiles that should be returned - default is `c(0, .25, .5, .75, 1)` quantiles
#'
#' @return a dataframe with qunatiles - in wide format
#' @export
#'
#' @examples
#'
#' df = data.frame(group = c(rep(1, 4)
#'                           ,rep(2, 3))
#'                 ,days = c(round(runif(4, 0, 20), 0)
#'                           ,round(runif(3, 0, 20), 0))
#'                 ,count = c(round(runif(4, 0, 5), 0)
#'                            ,round(runif(3, 0, 5), 0)))
#'
#' df %>%
#'   group_by(group) %>%
#'   nest() %>%
#'   mutate(qauntiles = map(data, ~group_wtd_quantiles(.x, value = "days", weight = "count"))) %>%
#'   unnest(cols = c(qauntiles)) %>%
#'   select(!data)
#'
#' #alternative calucaltion by expansion
#' rep(df$days[1:4], df$count[1:4]) %>%  summary()
group_wtd_quantiles = function(df, value, weight = "count", quantiles = c(0, .25, .5, .75, 1)){
  map(quantiles, ~DescTools::Quantile(df[[value]], df[[weight]], .x, na.rm = T)) %>%
    reduce(bind_cols) %>%
    set_names(map_chr(quantiles, ~paste0(value, "_", .x*100, "%")))
}





#end
