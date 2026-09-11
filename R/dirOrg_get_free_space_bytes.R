#' Get free space on a Windows drive, in bytes
#'
#' Shells out to PowerShell's `Get-Volume` cmdlet to read the remaining
#' free space on the drive that contains `path`. Windows-only.
#'
#' @param path Character path (file or directory) on the drive to check.
#'   Only the drive letter is used.
#'
#' @return Numeric scalar, free space in bytes.
#'
#' @keywords internal
#'
#' @importFrom fs path_abs
#'
#' @examples
#' \dontrun{
#' dirOrg_get_free_space_bytes("E:/gaunt_project_reimage")
#' }
dirOrg_get_free_space_bytes <- function(path) {
  drive_letter <- toupper(substr(fs::path_abs(path), 1, 1))
  cmd <- sprintf("(Get-Volume -DriveLetter %s).SizeRemaining", drive_letter)
  out <- suppressWarnings(
    system2("powershell", args = c("-NoProfile", "-Command", cmd),
            stdout = TRUE, stderr = FALSE)
  )
  val <- suppressWarnings(as.numeric(trimws(out[length(out)])))
  if (is.na(val)) {
    stop("Could not read free space for drive ", drive_letter, ":", call. = FALSE)
  }
  val
}
