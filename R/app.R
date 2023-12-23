# Main application -------------------------------------------------------------

#' Run the main shiny application
#'
#' @param ...
#'
#' @export
#'
#' @examples
#' \dontrun{run_twfe_app()}
run_twfe_app <- function(...) {
  ui <- fluidPage(
    withMathJaxLocal(),
    titlePanel("Two-Way Fixed Effect Estimator"),
    tabsetPanel(id = "parameters_tabset",
      tabPanel("Basic", basic_UI("basic")),
      tabPanel("Two Events", twoEvents_UI("two_events")),
      tabPanel("Multi Events", multiEvents_UI("multi_events")),
      tabPanel("Debug: Data Generated",
        column(6,
          h3("Data event group"),
          tableOutput("data_event_table")
        ),
        column(6,
          h3("Data individual"),
          tableOutput("data_ind_table")
        )
      )
    ),
    h2("Model"),
    model_UI("model")
  )

  server <- function(input, output, session) {
    data_events <- list(
      basic_Server("basic"),
      twoEvents_Server("two_events"),
      multiEvents_Server("multi_events")
    )

    data_event <- reactiveVal()
    observe({
      if (input$parameters_tabset == "Basic") {
        data_event(data_events[[1]]())
      } else if (input$parameters_tabset == "Two Events") {
        data_event(data_events[[2]]())
      } else if (input$parameters_tabset == "Multi Events") {
        data_event(data_events[[3]]())
      }
    })

    data_ind <- reactive({
      data_event() |>
        tidyr::uncount(group_size, .id = "num") |>
        mutate(ind = paste(group, num, sep = "_"))
    })

    output$data_event_table <- renderTable(data_event())
    output$data_ind_table <- renderTable(data_ind())

    model_Server("model", data_ind)
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, ...)
}

withMathJaxLocal <- function(...)  {
  addResourcePath("MathJax", system.file("MathJax", package = "twfeBenchmark"))
  path <- "MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
          ..., tags$script(HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}
