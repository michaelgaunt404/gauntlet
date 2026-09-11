#' Scan folder sizes at a fixed depth below a root
#'
#' Walks the directory tree under `root`, finds every directory sitting
#' exactly `level` levels below it, and sums the size of all files inside
#' each one (recursively). The scan root is stashed as a `"scan_root"`
#' attribute on the result so [dirOrg_copy_selected_dirs()] can rebuild
#' relative paths later without it being passed again explicitly.
#'
#' @param root Character path to the folder to scan.
#' @param level Integer, depth (relative to `root`) at which to collect
#'   folders. `level = 1` collects `root`'s immediate subfolders.
#'   Defaults to `1`.
#'
#' @return A [data.table::data.table()] with columns `dir` (character path),
#'   `total_size` (numeric, bytes), and `total_size_human` (character,
#'   formatted via [fs::fs_bytes()]). The `scan_root` attribute holds the
#'   absolute path passed to `root`. Returns a zero-row table if no folders
#'   are found at `level`.
#'
#' @export
#'
#' @importFrom fs path_abs dir_ls path_split file_size fs_bytes
#' @importFrom data.table data.table setattr
#'
#' @examples
#' \dontrun{
#' sizes <- dirOrg_get_dir_sizes_at_level(
#'   "C:/Users/GauntM/Documents/030_projects",
#'   level = 1
#' )
#' }
dirOrg_get_dir_sizes_at_level <- function(root, level = 1) {

  ## 1.1 -- Resolve root, walk the tree ---------------------------------------
  root <- fs::path_abs(root)
  dirOrg_say("Scanning ", root, " for folders at level ", level, "...")
  all_dirs <- fs::dir_ls(root, recurse = TRUE, type = "directory")

  ## 1.2 -- Compute depth of every dir relative to root ------------------------
  root_depth <- length(fs::path_split(root)[[1]])
  depths <- lengths(fs::path_split(all_dirs)) - root_depth
  target_dirs <- all_dirs[depths == level]

  if (length(target_dirs) == 0) {
    dirOrg_say("No folders found at level ", level, " under ", root, level = "warn")
    return(data.table::data.table(
      dir = character(), total_size = numeric(), total_size_human = character()
    ))
  }
  dirOrg_say("Found ", length(target_dirs), " folder(s) at level ", level, ". Summing sizes...")

  ## 1.3 -- Recursive size sum per target folder --------------------------------
  sizes <- vapply(target_dirs, function(d) {
    files <- tryCatch(fs::dir_ls(d, recurse = TRUE, type = "file"), error = function(e) character())
    if (length(files) == 0) return(0)
    as.numeric(sum(fs::file_size(files)))
  }, numeric(1))

  ## 1.4 -- Assemble result + tag scan root for downstream path building -------
  result <- data.table::data.table(
    dir = target_dirs,
    total_size = sizes,
    total_size_human = format(fs::fs_bytes(sizes))
  )
  data.table::setattr(result, "scan_root", root)
  dirOrg_say("Done. Total across all folders: ", format(fs::fs_bytes(sum(sizes))), level = "success")
  result
}
