select_columns_from_pivoted_table <- function(data) {
  if (ncol(data) < 1) {
    stop("Data must have at least one column.")
  }

  # Reactive value to store selected items
  selected_values <- reactiveVal(NULL)

  ui <- fluidPage(
    titlePanel("Select Column Names from Pivoted Table"),

    reactableOutput("table_parent"),

    verbatimTextOutput("selected_columns"),

    actionButton("save_btn", "Save & Close", class = "btn-primary")
  )

  server <- function(input, output, session) {

    # Render reactable with selectable rows
    output$table_parent <- renderReactable({
      reactable(
        data,
        selection = "multiple",
        # id = "table_parent",
        striped = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        resizable = TRUE
      )
    })

    # Get selected rows
    selected <- reactive({
      selected_rows <- reactable::getReactableState("table_parent", "selected")
      if (is.null(selected_rows)) return(character(0))
      data[[1]][selected_rows]  # Extracts first column values
    })

    # Display selected values as a vector
    output$selected_columns <- renderPrint({
      selected()
    })

    # Save selection and close app when button is clicked
    observeEvent(input$save_btn, {
      selected_values(selected())  # Store selected values
      stopApp(selected_values())   # Close app and return vector
    })
  }

  runApp(shinyApp(ui, server))
}
