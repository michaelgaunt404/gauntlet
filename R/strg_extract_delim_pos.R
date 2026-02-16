#' Extract a position from a delimiter-separated string
#'
#' Splits a string by a specified delimiter and returns the element at a given
#' position counted from the **end** of the string. This is useful for reliably
#' extracting filenames, directory names, or other path components from
#' consistently delimited strings.
#'
#' @param string A character string to split.
#' @param pos Integer position to extract, counted from the end of the string.
#'   For example, \code{pos = 1} returns the last element, \code{pos = 2} the
#'   second-to-last, etc.
#' @param delim Delimiter used to split the string. Defaults to \code{"/"}.
#'
#' @return A character string corresponding to the requested position.
#'
#' @details
#' This function assumes the input string is regularly delimited and does not
#' perform bounds checking. If \code{pos} exceeds the number of available
#' elements, \code{NA} will be returned.
#'
#' @examples
#' \dontRun{
#' path <- "C:/Users/GauntM/Documents/010_projects/replicaToolkitR_lab2/projects/replica_md_quick_builds/data/data_thru_bbox_20260106_200951068395_2024_Q2_test"
#'
#' # Get the last path component
#' strg_extract_delim_pos(path, pos = 1, delim = "/")
#'
#' # Get the parent directory name
#' strg_extract_delim_pos(path, pos = 2, delim = "/")
#' }
#'
#' @importFrom stringr str_split
#'
#' @export
strg_extract_delim_pos <- function(string, pos, delim = "/") {
  parts <- stringr::str_split(string, delim)[[1]]
  rev(parts)[pos]
}
