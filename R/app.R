# Main application -------------------------------------------------------------

#' Run the main shiny application
#'
#' @inheritDotParams shiny::shinyApp -ui -server
#'
#' @export
#'
#' @examples
#' \dontrun{run_twfe_app()}
run_twfe_app <- function(...) {
  ui <- page_navbar(
    id = "parameters_tabset",
    title = "Two-Way Fixed Effect Estimator",
    sidebar = sidebar(
      width = 275,
      title = "Parameters",
      multiParameters_UI("parameters")
    ),
    theme = bs_theme(
      bootswatch = "zephyr"
    ),
    header = tagList(
      shinyjs::useShinyjs(),
      withMathJaxLocal()
    ),
    nav_panel("Purpose", overview_UI("presentation")),
    nav_panel("Presets", presets_UI("presets")),
    nav_panel("Analysis",
      value = "outcome_plot",
      accordion(
        accordion_panel("Outcome Plot",
        plotOutput("event_plot")
      ),
      navset_card_underline(id = "model_tabset",
        selected = "model",
        nav_panel("Theory", theory_UI("theory"), value = "theory"),
        nav_panel("Regression", model_UI("model"), value = "model")
      )
    )),
    nav_panel("Debug: Data Generated",
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Data event group"),
          tableOutput("data_event_table")
        ),
        card(
          full_screen = TRUE,
          card_header("Data individual"),
          tableOutput("data_ind_table")
        )
      ),
    )
  )

  server <- function(input, output, session) {
    session$userData$timeline <- seq(0L, 10L)
    params_group <- multiParameters_Server("parameters", max_treated = 10L)
    params_group_flat <- reactive({
      treated <- params_group()$treated
      append(
        list(control = params_group()$control),
        purrr::set_names(treated, paste0("treated_", seq_along(treated)))
      ) |>
        insert_name()
    })

    presets_Server("presets", session)

    data_event <- reactive({
      generate_data_event(
        control_group = params_group()$control,
        treated_groups = params_group()$treated,
        timeline = session$userData$timeline
      )
    })

    data_ind <- reactive({
      data_event() |>
        tidyr::uncount(size, .id = "num") |>
        mutate(ind = paste(group, num, sep = "_"))
    })

    output$data_event_table <- renderTable(data_event())
    output$data_ind_table <- renderTable(data_ind())

    treated_events <- reactive({
      purrr::map(
        params_group()$treated,
        \(treated_group) treated_group$event
      ) |> purrr::flatten_int()
    })

    output$event_plot <- renderPlot({
      plot_data(
        pp_table(data_event()),
        treated_events(),
        session$userData$timeline
      )
    })

    theory_Server("theory", params_group_flat, data_event)
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
