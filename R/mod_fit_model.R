model_UI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel(
      "Difference in Difference",
      sidebarLayout(
        sidebarPanel(
          "Formula: $$y_{it} = \\alpha_i + \\gamma_t + \\beta^{DD}D_{it} + \\varepsilon_{it}$$",
          actionButton(ns("fit_model"), "Fit model")
        ),
        mainPanel(gt::gt_output(ns("model_formula")))
      )
    ),
    tabPanel(
      "Event Study",
      sidebarLayout(
        sidebarPanel(
          "Formula: ",
          div(
            "$$y_{it} = \\alpha_i + \\gamma_t + \\sum_{g} \\mu_{g}\\mathbf{1}(t - E_i \\in g) + \\varepsilon_{it}$$",
            style = "overflow: auto;"
          ),
          eventStudyParameters_UI(ns("params_model_event")),
          actionButton(ns("fit_model_event"), "Fit model")
        ),
        mainPanel(gt::gt_output(ns("model_formula_event")))
      )
    ),
    tabPanel(
      "Interaction-Weighted",
      sidebarLayout(
        sidebarPanel(
          "Formula: ",
          p(
            "$$y_{it} = \\alpha_i + \\gamma_t + \\sum_{e \\notin \\{+\\infty\\}} \\sum_{l} \\beta_{e,l}\\mathbf{1}(E_i = e)D_{it}^l + \\varepsilon_{it}$$", # nolint
            style = "overflow: auto;"
          ),
          actionButton(ns("fit_model_iw"), "Fit model")
        ),
        mainPanel(gt::gt_output(ns("model_formula_iw")))
      )
    )
  )
}

model_Server <- function(id, data_ind) {
  moduleServer(id, function(input, output, session) {
    params_model_event <- eventStudyParameters_Server("params_model_event")

    model_fitted <- reactive({
      twfe_did_model(data_ind())
    })

    output$model_formula <- gt::render_gt({
      model_fitted() |>
        gtsummary::tbl_regression() |>
        gtsummary::as_gt()
    }) |>
      bindEvent(input$fit_model)

    model_fitted_event <- reactive({
      event_study_model(data_ind(), params_model_event())
    })

    output$model_formula_event <- gt::render_gt({
      model_fitted_event() |>
        gtsummary::tbl_regression() |>
        gtsummary::as_gt()
    }) |>
      bindEvent(input$fit_model_event)

    model_fitted_iw <- reactive({
      iw_model(data_ind())
    })

    output$model_formula_iw <- gt::render_gt({
      model_fitted_iw() |>
        gtsummary::tbl_regression() |>
        gtsummary::as_gt()
    }) |>
      bindEvent(input$fit_model_iw)

  })
}

