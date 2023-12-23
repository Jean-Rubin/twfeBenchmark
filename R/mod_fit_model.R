model_UI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel(
      "Difference in Difference",
      sidebarLayout(
        sidebarPanel(
          "Formula: $$y_{it} = \\alpha_i + \\alpha_t + \\beta^{DD}D_{it} + \\varepsilon_{it}$$",
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
            "$$y_{it} = \\alpha_i + \\alpha_t + \\sum_{l = -9}^9 \\beta_{l}D_{it}^l + \\varepsilon_{it}$$",
            style = "overflow: auto;"
          ),
          actionButton(ns("fit_model_event"), "Fit model")
        ),
        mainPanel(gt::gt_output(ns("model_formula_event")))
      )
    ),
    tabPanel(
      "Interaction-Weighted",
      sidebarPanel(
        "Formula: ",
          p(
            "$$y_{it} = \\alpha_i + \\alpha_t + \\sum_{e \\notin \\{+\\infty\\}} \\sum_{l = -9}^9 \\beta_{e,l}\\mathbf{1}(E_i = e)D_{it}^l + \\varepsilon_{it}$$",
            style = "overflow: auto;"
          ),
          actionButton(ns("fit_model_iw"), "Fit model")
        ),
        mainPanel(gt::gt_output(ns("model_formula_iw")))
    )
  )
}

model_Server <- function(id, data_ind) {
  moduleServer(id, function(input, output, session) {
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
      event_study_model(data_ind())
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

