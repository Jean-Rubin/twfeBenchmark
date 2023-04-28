basicDID_slidersParams_UI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    sliderInput(
      ns("base_gap"),
      "Base gap",
      min = -5,
      max = 5,
      value = 3
    ),
    sliderInput(
      ns("slope_trend"),
      "Slope trend",
      min = 0,
      max = 1,
      value = 0.5,
      step = 0.25
    ),
    sliderInput(
      ns("event"),
      "Event time",
      min = 0,
      max = 10,
      value = 5
    ),
    sliderInput(
      ns("effect"),
      "Instant Effect",
      min = -5,
      max = 5,
      value = 2,
      step = 1
    ),
    sliderInput(
      ns("slope_effect"),
      "Slope effect",
      min = -3,
      max = 3,
      value = 0,
      step = 0.5
    )
  )
}

basicDID_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    sidebarLayout(
      basicDID_slidersParams_UI(id),
      mainPanel(plotOutput(ns("event_plot")))
    ),
    sidebarLayout(
      sidebarPanel(actionButton(ns("fit_model"), "Fit model")),
      mainPanel(
        "$$y_{it} = \\alpha_i + \\alpha_t + \\beta^{DD}D_{it} + \\varepsilon_{it}$$",
        gt::gt_output(ns("twfe_formula"))
      )
    )
  )
}

basicDID_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeline <- seq(0, 10)
    common_trend <- reactive(input$slope_trend * timeline)

    data_event <- reactive(
      bind_rows(
        tibble(
          ind = "Never treated",
          t = timeline,
          y = common_trend(),
          treated = 0
        ),
        tibble(
          ind = "Treated",
          t = timeline,
          y = common_trend() + input$base_gap +
            input$slope_effect * pmax.int(t - input$event, 0) +
            input$effect * (t >= input$event),
          treated = as.numeric(t >= input$event)
        )
      )
    )

    data_counterfactual <- reactive({
      data_event() |>
        filter(ind == "Never treated") |>
        mutate(
          ind = "Counterfactual",
          y = common_trend() + input$base_gap
        )
    })

    output$event_plot <- renderPlot({
      ggplot(data_event(), aes(t, y, colour = ind)) +
        geom_line() +
        geom_point() +
        geom_vline(xintercept = input$event, linetype = "dotted") +
        geom_hline(yintercept = 0) +
        geom_line(data = data_counterfactual(), linetype = "dashed") +
        scale_x_continuous(breaks = timeline) +
        coord_cartesian(xlim = c(0, 10), ylim = c(-10, 30)) +
        labs(x = "Time", y = "Outcome", colour = "Individual") +
        theme_bw() +
        theme(
          legend.position = "bottom",
          text = element_text(size = 14)
        )
    })

    twfe_model <- reactive({
      data_event() |>
        mutate(
          "Individual FE" = ind,
          "Temporal FE" = factor(t),
          "Treatment Effect" = treated
        ) |>
        lm(
          y ~ `Treatment Effect` + `Individual FE` + `Temporal FE`,
          data = _
        )
    })


    output$twfe_formula <- gt::render_gt({
      twfe_model() |>
        gtsummary::tbl_regression() |>
        gtsummary::as_gt()
    }) |>
      bindEvent(input$fit_model)
  })
}
