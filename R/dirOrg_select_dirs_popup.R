
#' Pick a subset of scanned folders in an interactive popup
#'
#' Opens a `miniUI`/`shiny` gadget showing the folders returned by
#' [dirOrg_get_dir_sizes_at_level()] in a sortable table, with running
#' totals for the current selection and a live free-space readout for the
#' destination drive.
#'
#' @param size_dt A [data.table::data.table()] as returned by
#'   [dirOrg_get_dir_sizes_at_level()], with columns `dir`, `total_size`,
#'   and `total_size_human`.
#' @param dest_root Character path to the destination drive/folder, used
#'   only to report free space in the popup. Defaults to
#'   `"E:/gaunt_project_reimage"`.
#'
#' @return A `data.table` containing just the selected rows of `size_dt`
#'   (with the `scan_root` attribute preserved), or `NULL` if the user
#'   cancels.
#'
#' @export
#'
#' @importFrom data.table setattr
#' @importFrom fs fs_bytes
#' @importFrom shiny fluidRow column actionButton hr uiOutput fluidPage
#'   observeEvent renderUI tagList strong br span showNotification stopApp
#'   runGadget dialogViewer
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom DT DTOutput renderDT datatable dataTableProxy selectRows
#'
#' @examples
#' \dontrun{
#' sizes  <- dirOrg_get_dir_sizes_at_level("C:/Users/GauntM/Documents/030_projects")
#' subset <- dirOrg_select_dirs_popup(sizes, dest_root = "E:/gaunt_project_reimage")
#' }
dirOrg_select_dirs_popup <- function(size_dt, dest_root = "E:/gaunt_project_reimage") {

  ## 2.1 -- Validate input ------------------------------------------------------
  if (nrow(size_dt) == 0) stop("size_dt has no rows -- nothing to select from.", call. = FALSE)
  scan_root <- attr(size_dt, "scan_root")
  if (is.null(scan_root)) {
    dirOrg_say(
      "size_dt has no scan_root attribute -- you'll need to pass scan_root explicitly to dirOrg_copy_selected_dirs().",
      level = "warn"
    )
  }

  ## 2.2 -- UI: table of folders + select-all/clear + live selection summary ---
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select folders to copy"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton("select_all", "Select all")),
        shiny::column(6, shiny::actionButton("clear_all", "Clear all"))
      ),
      shiny::hr(),
      shiny::uiOutput("selection_summary"),
      shiny::hr(),
      DT::DTOutput("dir_table")
    )
  )

  ## 2.3 -- Server: selection tracking + live totals + free-space readout -------
  server <- function(input, output, session) {

    output$dir_table <- DT::renderDT({
      DT::datatable(
        size_dt[, .(dir, total_size_human, total_size)],
        selection = "multiple",
        rownames = FALSE,
        colnames = c("Folder", "Size", "bytes"),
        options = list(
          pageLength = 25,
          order = list(list(2, "desc")),
          columnDefs = list(list(visible = FALSE, targets = 2))
        )
      )
    })

    dt_proxy <- DT::dataTableProxy("dir_table")
    shiny::observeEvent(input$select_all, DT::selectRows(dt_proxy, seq_len(nrow(size_dt))))
    shiny::observeEvent(input$clear_all, DT::selectRows(dt_proxy, NULL))

    output$selection_summary <- shiny::renderUI({
      sel <- input$dir_table_rows_selected
      n_sel <- length(sel)
      bytes_sel <- if (n_sel > 0) sum(size_dt$total_size[sel]) else 0
      free_bytes <- tryCatch(dirOrg_get_free_space_bytes(dest_root), error = function(e) NA_real_)
      shiny::tagList(
        shiny::strong(sprintf("Selected: %d folder(s) -- %s", n_sel, format(fs::fs_bytes(bytes_sel)))),
        shiny::br(),
        if (!is.na(free_bytes)) {
          shiny::span(sprintf("Free space on %s: %s", dest_root, format(fs::fs_bytes(free_bytes))))
        } else {
          shiny::span("Could not read free space on destination drive.")
        }
      )
    })

    ## 2.4 -- Return subset on Done, NULL on Cancel --------------------------
    shiny::observeEvent(input$done, {
      sel <- input$dir_table_rows_selected
      if (length(sel) == 0) {
        shiny::showNotification("Select at least one folder first.", type = "warning")
        return(invisible(NULL))
      }
      out <- size_dt[sel]
      data.table::setattr(out, "scan_root", scan_root)
      shiny::stopApp(out)
    })
    shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Select folders", width = 900, height = 700))
}
