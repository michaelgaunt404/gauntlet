#' Interactive converter for `_targets` objects to CSV/GPKG
#'
#' @description
#' Launches a small Shiny UI that lists files in a `_targets/objects` folder by
#' their **casename** (i.e., filename). You can multi-select casenames, and the
#' app will read each object via `targets::tar_read_raw()` and convert it based
#' on class:
#'
#' - **`sf`** objects → written as **GeoPackage** (`.gpkg`) via `sf::st_write()`
#' - **`data.frame`** (incl. `tibble`, `data.table`) → written as **CSV**
#'   (`.csv`) via `readr::write_csv()`
#'
#' A conversion log is shown in the UI after processing.
#'
#' @details
#' - The function assumes that each file’s **base name** corresponds to a valid
#'   target name readable with `targets::tar_read_raw()`.
#' - The output directory is created (recursively) if it does not exist.
#' - Unknown object classes are skipped with an explanatory message.
#'
#' @param object_folder Character scalar. Path to the `_targets/objects`
#'   directory that contains the stored binary objects. Defaults to
#'   `"_targets/objects"`.
#' @param output_folder Character scalar. Directory where converted files will
#'   be written. Defaults to `file.path(tempdir(), "converted")`.
#'
#' @return
#' A Shiny application object (invisibly). Side effects: writes `.csv` and/or
#' `.gpkg` files to `output_folder`.
#'
#' @section Conversion rules:
#' \itemize{
#'   \item If \code{inherits(obj, "sf")} → write \code{.gpkg} via \code{st_write()}.
#'   \item Else if \code{inherits(obj, "data.frame")} → write \code{.csv} via \code{write_csv()}.
#'   \item Else → skip with message.
#' }
#'
#' @examples
#' \dontrun{
#' # Run the GUI against your targets store and write outputs to a folder:
#' targets_convert_via_gui(
#'   object_folder = "_targets/objects",
#'   output_folder = file.path(tempdir(), "converted_targets")
#' )
#' }
#'
#' @export
#'
#' @importFrom fs dir_ls
#' @importFrom tools file_path_sans_ext
#' @importFrom targets tar_read_raw
#' @importFrom sf st_write
#' @importFrom readr write_csv
#' @importFrom DT DTOutput renderDT datatable dataTableProxy selectRows
#' @importFrom shiny shinyApp fluidPage titlePanel checkboxInput actionButton
#' @importFrom shiny h4 verbatimTextOutput observeEvent renderText showNotification
targets_convert_via_gui <- function(
    object_folder = "_targets/objects",
    output_folder = "./data/converted") {
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # List files in the targets object folder
  files <- fs::dir_ls(object_folder, type = "file", recurse = FALSE)
  file_df <- data.frame(
    casename = basename(files),
    stringsAsFactors = FALSE
    # note: we list by filename; we assume base name == target name
  )

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Convert targets objects"),
      shiny::checkboxInput("select_all", "Select all casenames", FALSE),
      DT::DTOutput("case_table"),
      shiny::actionButton("convert_btn", "Convert Selected"),
      shiny::h4("Conversion Log"),
      shiny::verbatimTextOutput("log")
    ),

    server = function(input, output, session) {
      # Table of available casenames
      output$case_table <- DT::renderDT({
        DT::datatable(file_df, selection = "multiple", rownames = FALSE)
      })

      # Toggle select all
      shiny::observeEvent(input$select_all, {
        proxy <- DT::dataTableProxy("case_table")
        if (isTRUE(input$select_all)) {
          DT::selectRows(proxy, 1:nrow(file_df))
        } else {
          DT::selectRows(proxy, NULL)
        }
      })

      # Convert on click
      shiny::observeEvent(input$convert_btn, {
        selected <- input$case_table_rows_selected
        if (length(selected) == 0) {
          shiny::showNotification("No casenames selected.", type = "error")
          return(invisible(NULL))
        }

        log <- character()

        for (i in selected) {
          case <- file_df$casename[i]
          case_base <- tools::file_path_sans_ext(case)

          obj <- tryCatch(
            targets::tar_read_raw(case_base),
            error = function(e) e
          )

          if (inherits(obj, "error")) {
            log <- c(log, paste("✗ Failed to read:", case, "-", obj$message))
            next
          }

          if (inherits(obj, "sf")) {
            out_path <- file.path(output_folder, paste0(case_base, ".gpkg"))
            tryCatch({
              sf::st_write(obj, out_path, delete_dsn = TRUE, quiet = TRUE)
              log <- c(log, paste("✓ Converted to GPKG:", case_base))
            }, error = function(e) {
              log <- c(log, paste("✗ Failed to write GPKG:", case_base, "-", e$message))
            })

          } else if (inherits(obj, "data.frame")) {
            out_path <- file.path(output_folder, paste0(case_base, ".csv"))
            tryCatch({
              readr::write_csv(obj, out_path)
              log <- c(log, paste("✓ Converted to CSV:", case_base))
            }, error = function(e) {
              log <- c(log, paste("✗ Failed to write CSV:", case_base, "-", e$message))
            })

          } else {
            log <- c(log, paste("✗ Unknown object type in:", case_base))
          }
        }

        output$log <- shiny::renderText(paste(log, collapse = "\n"))
      })
    }
  )
}
