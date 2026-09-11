#' Run the full scan -> select -> copy (-> delete) reimage workflow
#'
#' Convenience wrapper that chains [dirOrg_get_dir_sizes_at_level()],
#' [dirOrg_select_dirs_popup()], and [dirOrg_copy_selected_dirs()], with an
#' optional final step that deletes verified source folders via
#' [dirOrg_delete_dirs()].
#'
#' @param root Character path to the folder to scan.
#' @param dest_root Character path to the destination root folder.
#'   Defaults to `"E:/gaunt_project_reimage"`.
#' @param level Integer, depth below `root` to scan. Defaults to `1`.
#' @param space_buffer_gb Numeric, minimum free space (in GB) to leave on
#'   the destination drive. Defaults to `25`.
#' @param overwrite Logical, whether to overwrite existing destination
#'   folders. Defaults to `FALSE`.
#' @param delete_after_copy Logical, whether to delete source folders
#'   whose copy was verified (`copy_status == "copied_verified"`) once
#'   copying finishes. Defaults to `FALSE`.
#'
#' @return The copy log `data.table` from [dirOrg_copy_selected_dirs()],
#'   or `NULL` (invisibly) if the popup selection is cancelled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_log <- dirOrg_run_reimage_workflow(
#'   root = "C:/Users/GauntM/Documents/030_projects",
#'   dest_root = "E:/gaunt_project_reimage",
#'   level = 1,
#'   space_buffer_gb = 25,
#'   delete_after_copy = TRUE
#' )
#' }
dirOrg_run_reimage_workflow <- function(root,
                                        dest_root = "E:/gaunt_project_reimage",
                                        level = 1,
                                        space_buffer_gb = 25,
                                        overwrite = FALSE,
                                        delete_after_copy = FALSE) {

  sizes <- dirOrg_get_dir_sizes_at_level(root, level = level)
  subset <- dirOrg_select_dirs_popup(sizes, dest_root = dest_root)
  if (is.null(subset)) {
    dirOrg_say("Selection cancelled -- nothing copied.", level = "warn")
    return(invisible(NULL))
  }

  copy_log <- dirOrg_copy_selected_dirs(
    subset,
    dest_root = dest_root,
    space_buffer_gb = space_buffer_gb,
    overwrite = overwrite
  )

  if (isTRUE(delete_after_copy)) {
    verified <- copy_log$src_dir[copy_log$copy_status == "copied_verified"]
    if (length(verified) > 0) {
      dirOrg_delete_dirs(verified)
    } else {
      dirOrg_say("No verified copies to delete.", level = "warn")
    }
  }

  copy_log
}
