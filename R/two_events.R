twoEventsDID_slidersParams_UI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    h3("Trend"),
    sliderInput(
      ns("slope_trend"),
      "Slope trend",
      min = 0,
      max = 1,
      value = 0.5,
      step = 0.25
    ),
    h3("First group"),
    sliderInput(
      ns("base_gap_1"),
      "Base gap 1",
      min = -5,
      max = 10,
      value = 7
    ),
    sliderInput(
      ns("event_1"),
      "Event time 1",
      min = 0,
      max = 10,
      value = 3
    ),
    sliderInput(
      ns("effect_1"),
      "Instant Effect 1",
      min = -5,
      max = 10,
      value = 8,
      step = 1
    ),
    sliderInput(
      ns("slope_effect_1"),
      "Slope effect 1",
      min = -3,
      max = 3,
      value = 0,
      step = 0.5
    ),
    h3("Second group"),
    sliderInput(
      ns("base_gap_2"),
      "Base gap 2",
      min = -5,
      max = 10,
      value = 3
    ),
    sliderInput(
      ns("event_2"),
      "Event time 2",
      min = 0,
      max = 10,
      value = 7
    ),
    sliderInput(
      ns("effect_2"),
      "Instant Effect 2",
      min = -5,
      max = 10,
      value = 5,
      step = 1
    ),
    sliderInput(
      ns("slope_effect_2"),
      "Slope effect 2",
      min = -3,
      max = 3,
      value = 0,
      step = 0.5
    )
  )
}

twoEventsDID_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    sidebarLayout(
      twoEventsDID_slidersParams_UI(id),
      mainPanel(
        plotOutput(ns("event_plot")),
        h3("Goodman-Bacon Decomposition"),
        uiOutput(ns("analytic"))
      )
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

twoEventsDID_Server <- function(id) {
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
          ind = "Treated 1",
          t = timeline,
          y = common_trend() + input$base_gap_1 +
            input$slope_effect_1 * pmax.int(t - input$event_1, 0) +
            input$effect_1 * (t >= input$event_1),
          treated = as.numeric(t >= input$event_1)
        ),
        tibble(
          ind = "Treated 2",
          t = timeline,
          y = common_trend() + input$base_gap_2 +
            input$slope_effect_2 * pmax.int(t - input$event_2, 0) +
            input$effect_2 * (t >= input$event_2),
          treated = as.numeric(t >= input$event_2)
        )
      )
    )

    d_1 <- reactive((10 - input$event_1 + 1) / 11)
    d_2 <- reactive((10 - input$event_2 + 1) / 11)
    s_1u <- reactive((2 / 3)^2 * 1 / 4 * d_1() * (1 - d_1()))
    s_2u <- reactive((2 / 3)^2 * 1 / 4 * d_2() * (1 - d_2()))
    s_12_1 <- reactive({
      v_12_1 <- 1 / 4 * (d_1() - d_2()) * (1 - d_1()) / (1 - d_2())^2
      (2 / 3 * (1 - d_2()))^2 * v_12_1
    })
    s_12_2 <- reactive({
      v_12_2 <- 1 / 4 * d_2() * (d_1() - d_2()) / d_1()^2
      (2 / 3 * d_1())^2 * v_12_2
    })
    s_tot <- reactive(s_1u() + s_2u() + s_12_1() + s_12_2())
    s_1u_n <- reactive(s_1u() / s_tot())
    s_2u_n <- reactive(s_2u() / s_tot())
    s_12_1_n <- reactive(s_12_1() / s_tot())
    s_12_2_n <- reactive(s_12_2() / s_tot())

    pp <- function(x, ...) formatC(x, digits = 2, ...)
    output$analytic <- renderUI({
      withMathJax(HTML(
        paste0("$$\\overline{D}_1 = ", pp(d_1()), "\\quad ",
               "\\overline{D}_2 = ", pp(d_2()), "$$"),
        paste0("$$s_{1U} = ", pp(s_1u_n()), "\\quad ",
               "s_{2U} = ", pp(s_2u_n()), "$$"),
        paste0("$$s_{12}^1 = ", pp(s_12_1_n()), "\\quad ",
               "s_{12}^2 = ", pp(s_12_2_n()), "$$"),
        "$$s_{1U}\\beta_{1U} + s_{2U}\\beta_{2U} + s_{12}^1\\beta_{12}^1 + s_{12}^2\\beta_{12}^2 = \\beta$$",
        paste0(
          "$$", pp(s_1u_n()), "\\times", input$effect_1,
          "+", pp(s_2u_n()), "\\times", input$effect_2,
          "+", pp(s_12_1_n()), "\\times", input$effect_1,
          "+", pp(s_12_2_n()), "\\times", input$effect_2,
          "=", (s_1u_n() + s_12_1_n()) * input$effect_1 +
            (s_2u_n() + s_12_2_n()) * input$effect_2,
          "$$"
        )
      ))
    })

    output$event_plot <- renderPlot({
      ggplot(data_event(), aes(t, y, colour = ind)) +
        geom_line() +
        geom_point() +
        geom_vline(xintercept = input$event_1, linetype = "dotted") +
        geom_vline(xintercept = input$event_2, linetype = "dotted") +
        geom_hline(yintercept = 0) +
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
