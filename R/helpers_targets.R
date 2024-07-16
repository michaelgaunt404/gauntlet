#' Scan File for Target String
#'
#' This function scans a file for lines containing a specific target string and performs optional manipulations on the lines.
#'
#' @param file A character string specifying the path to the file to be scanned.
#' @param print_tar Logical; indicating whether to print the lines in a format suitable for tar loading. Default is \code{FALSE}.
#' @param rm_tar_load Logical; indicating whether to remove remove lines with tar_load{tar_objects}. Users may load objects but they are not used in the report, removing these items will give a list of tar objects that are actually used in the analysis. Default is \code{TRUE}.
#'
#' @return A character vector containing lines from the file matching the criteria specified.
#'
#' @examples
#' \dontrun{
#' # Scan a file for lines containing "plt_mdl_" and "plt_agg" strings, remove lines containing "tar_read",
#' # remove trailing spaces, and print lines in tar loading format.
#' tar_scan_file("example.txt", "plt_mdl_|plt_agg"), rm_tar = TRUE, rm_spce = TRUE, print_tar = TRUE)
#' }
#'
#' @export
#'
#' @importFrom stringr str_detect str_glue
#'
tar_scan_file = function(file, print_tar = FALSE, rm_tar_load = T){

  #note: this creates an index of all objects that are made in tar_file
  #TODO: should remove any target in a line that is commented out
  #----- not terribly important to do but could do it
  index_tar_ref = readLines("_targets.r") %>%
    .[stringr::str_detect(., ".*tar_target\\(")] %>%
    gsub(".*tar_target\\(", "\\1", .) %>%
    gsub(",.*", "\\1", .) %>%
    .[!(. == "")] %>%
    paste0(collapse = "|")

  #note: using above index - it reads a file and grabs all valid tar obects
  #TODO: this still captures objects that are commented out
  #----- should add functionality here that removes objects that are in commented out line or HTML commented out line
  lines = readLines(file) %>%
    paste0(collapse = " ") %>%
    stringr::str_split(" ")

  lines[[1]] %>%
    .[stringr::str_detect(., index_tar_ref)] %>%
    {if(rm_tar_load) .[!stringr::str_detect(., "tar_load")] else .} %>%
    gsub(str_glue(".*({index_tar_ref}).*"), "\\1", .) %>%
    unique() %>%
    sort() %>%
    paste0("tar_load(", ., ")", collapse = "\n") %>%
    cat()
  # {if(print_tar) stringr::str_glue("tar_load({.})") else .}

  return(lines)
}



