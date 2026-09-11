#' Delete source folders, typically after a verified copy
#'
#' Removes each directory in `dirs` recursively via [fs::dir_delete()].
#' Intended to be called on the `src_dir` column of the log returned by
#' [dirOrg_copy_selected_dirs()] -- e.g. after filtering to rows where
#' `copy_status == "copied_verified"` -- rather than on a raw selection,
#' so nothing is deleted before it's confirmed copied.
#'
#' @param dirs Character vector of directory paths to delete.
#'
#' @return Invisibly, the input `dirs` (unchanged). Called for its side
#'   effect of deleting each folder.
#'
#' @export
#'
#' @importFrom fs dir_delete
#'
#' @examples
#' \dontrun{
#' verified <- copy_log$src_dir[copy_log$copy_status == "copied_verified"]
#' dirOrg_delete_dirs(verified)
#' }
dirOrg_delete_dirs <- function(dirs) {
  for (d in dirs) {
    dirOrg_say("Deleting ", d, level = "warn")
    fs::dir_delete(d)
  }
  invisible(dirs)
}
