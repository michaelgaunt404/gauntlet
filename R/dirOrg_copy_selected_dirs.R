
#' Copy selected folders to a new drive, preserving structure, with checks
#'
#' Copies each folder in `selected_dt` into `dest_root`, rebuilding the
#' path relative to the parent of `scan_root` so the original folder
#' structure is preserved. Before copying anything, checks that the total
#' selected size will fit on the destination drive with a configurable
#' buffer; re-checks free space before each individual folder as well.
#' After each copy, verifies file count and total bytes match between
#' source and destination.
#'
#' @param selected_dt A `data.table` of folders to copy, as returned by
#'   [dirOrg_select_dirs_popup()] (or a subset of
#'   [dirOrg_get_dir_sizes_at_level()]'s output), with columns `dir` and
#'   `total_size`.
#' @param dest_root Character path to the destination root folder.
#'   Defaults to `"E:/gaunt_project_reimage"`.
#' @param scan_root Character path to the original scan root. If `NULL`
#'   (the default), read from the `"scan_root"` attribute of
#'   `selected_dt`.
#' @param space_buffer_gb Numeric, minimum free space (in GB) to always
#'   leave on the destination drive. Copying stops before breaching this
#'   buffer. Defaults to `25`.
#' @param overwrite Logical, whether to overwrite a destination folder
#'   that already exists. Defaults to `FALSE` (existing folders are
#'   skipped).
#'
#' @return A `data.table` log with one row per folder, containing
#'   `src_dir`, `dest_dir`, `src_bytes`, `dest_bytes`, `src_files`,
#'   `dest_files`, `bytes_match`, `files_match`, `copy_status`
#'   (one of `"copied_verified"`, `"copied_mismatch"`, `"copy_failed"`,
#'   `"skipped_exists"`, or `"skipped_low_space"`), and `notes`.
#'
#' @export
#'
#' @importFrom fs path_abs path_dir dir_create path_rel path dir_exists
#'   dir_copy dir_ls file_size fs_bytes
#' @importFrom data.table data.table rbindlist
#'
#' @examples
#' \dontrun{
#' copy_log <- dirOrg_copy_selected_dirs(
#'   subset,
#'   dest_root = "E:/gaunt_project_reimage",
#'   space_buffer_gb = 25
#' )
#' }
dirOrg_copy_selected_dirs <- function(selected_dt,
                                      dest_root = "E:/gaunt_project_reimage",
                                      scan_root = NULL,
                                      space_buffer_gb = 25,
                                      overwrite = FALSE) {

  ## 3.1 -- Resolve scan_root + validate ----------------------------------------
  if (is.null(scan_root)) scan_root <- attr(selected_dt, "scan_root")
  if (is.null(scan_root)) {
    stop(
      "No scan_root available -- pass it explicitly (the root folder originally given to dirOrg_get_dir_sizes_at_level()).",
      call. = FALSE
    )
  }

  scan_root <- fs::path_abs(scan_root)
  anchor <- fs::path_dir(scan_root)  # parent of scan_root -- keeps scan_root's own folder name in dest path
  dest_root <- fs::path_abs(dest_root)
  fs::dir_create(dest_root, recurse = TRUE)

  buffer_bytes <- space_buffer_gb * 1024^3
  n <- nrow(selected_dt)
  dirOrg_say(
    "Preparing to copy ", n, " folder(s) into ", dest_root,
    " (min free-space buffer: ", space_buffer_gb, " GB)"
  )

  ## 3.2 -- Pre-flight: total selected size vs. current free space --------------
  total_needed <- sum(selected_dt$total_size)
  free_before <- tryCatch(dirOrg_get_free_space_bytes(dest_root), error = function(e) NA_real_)
  if (!is.na(free_before)) {
    dirOrg_say(
      "Total selected size: ", format(fs::fs_bytes(total_needed)),
      " | Free on destination drive: ", format(fs::fs_bytes(free_before))
    )
    if (free_before - total_needed < buffer_bytes) {
      stop(sprintf(
        "Not enough space: copying everything selected would leave only %s free (buffer requires %s). Aborting before starting.",
        format(fs::fs_bytes(free_before - total_needed)), format(fs::fs_bytes(buffer_bytes))
      ), call. = FALSE)
    }
  } else {
    dirOrg_say(
      "Could not verify free space up front -- proceeding, but per-folder checks below will still catch problems.",
      level = "warn"
    )
  }

  ## 3.3 -- Per-folder copy loop with checks -------------------------------------
  log_rows <- vector("list", n)

  for (i in seq_len(n)) {
    src <- fs::path_abs(selected_dt$dir[i])
    rel <- fs::path_rel(src, start = anchor)
    dst <- fs::path(dest_root, rel)
    dirOrg_say(sprintf("[%d/%d] %s -> %s", i, n, src, dst))

    ## 3.3a -- space check before *this* folder (re-measured, not assumed) -----
    free_now <- tryCatch(dirOrg_get_free_space_bytes(dest_root), error = function(e) NA_real_)
    src_bytes <- selected_dt$total_size[i]
    if (!is.na(free_now) && (free_now - src_bytes) < buffer_bytes) {
      dirOrg_say(
        "Stopping: copying this folder would breach the ", space_buffer_gb, " GB buffer on the destination drive.",
        level = "error"
      )
      log_rows[[i]] <- data.table::data.table(
        src_dir = src, dest_dir = dst, src_bytes = src_bytes, dest_bytes = NA_real_,
        src_files = NA_integer_, dest_files = NA_integer_,
        bytes_match = FALSE, files_match = FALSE, copy_status = "skipped_low_space",
        notes = "aborted: space buffer"
      )
      break
    }

    ## 3.3b -- skip / overwrite check --------------------------------------------
    if (fs::dir_exists(dst) && !overwrite) {
      dirOrg_say("Destination already exists and overwrite = FALSE -- skipping.", level = "warn")
      log_rows[[i]] <- data.table::data.table(
        src_dir = src, dest_dir = dst, src_bytes = src_bytes, dest_bytes = NA_real_,
        src_files = NA_integer_, dest_files = NA_integer_,
        bytes_match = NA, files_match = NA, copy_status = "skipped_exists",
        notes = "destination already present"
      )
      next
    }

    ## 3.3c -- the actual copy ----------------------------------------------------
    copy_ok <- TRUE
    err_msg <- NA_character_
    tryCatch({
      fs::dir_create(fs::path_dir(dst), recurse = TRUE)
      fs::dir_copy(src, dst, overwrite = overwrite)
    }, error = function(e) {
      copy_ok <<- FALSE
      err_msg <<- conditionMessage(e)
    })

    ## 3.3d -- verify: file count + total bytes match between src and dst --------
    if (copy_ok) {
      src_files <- tryCatch(fs::dir_ls(src, recurse = TRUE, type = "file"), error = function(e) character())
      dst_files <- tryCatch(fs::dir_ls(dst, recurse = TRUE, type = "file"), error = function(e) character())
      dst_bytes <- if (length(dst_files) > 0) as.numeric(sum(fs::file_size(dst_files))) else 0
      files_match <- length(src_files) == length(dst_files)
      bytes_match <- isTRUE(all.equal(src_bytes, dst_bytes))
      status <- if (files_match && bytes_match) "copied_verified" else "copied_mismatch"
      lvl <- if (status == "copied_verified") "success" else "error"
      dirOrg_say(sprintf(
        "  files: %d/%d | bytes: %s/%s", length(dst_files), length(src_files),
        format(fs::fs_bytes(dst_bytes)), format(fs::fs_bytes(src_bytes))
      ), level = lvl)

      log_rows[[i]] <- data.table::data.table(
        src_dir = src, dest_dir = dst, src_bytes = src_bytes, dest_bytes = dst_bytes,
        src_files = length(src_files), dest_files = length(dst_files),
        bytes_match = bytes_match, files_match = files_match, copy_status = status, notes = NA_character_
      )
    } else {
      dirOrg_say("Copy failed: ", err_msg, level = "error")
      log_rows[[i]] <- data.table::data.table(
        src_dir = src, dest_dir = dst, src_bytes = src_bytes, dest_bytes = NA_real_,
        src_files = NA_integer_, dest_files = NA_integer_,
        bytes_match = FALSE, files_match = FALSE, copy_status = "copy_failed", notes = err_msg
      )
    }
  }

  ## 3.4 -- Assemble + print summary ----------------------------------------------
  log_dt <- data.table::rbindlist(Filter(Negate(is.null), log_rows))
  n_ok <- sum(log_dt$copy_status == "copied_verified")
  n_bad <- sum(log_dt$copy_status %in% c("copied_mismatch", "copy_failed", "skipped_low_space"))
  dirOrg_say(
    sprintf("Done: %d/%d verified, %d flagged (see copy_status/notes columns).", n_ok, n, n_bad),
    level = if (n_bad == 0) "success" else "warn"
  )

  log_dt
}
