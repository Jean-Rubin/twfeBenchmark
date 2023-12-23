twoEvents_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Parameters", style = "margin-top: 0;"),
      tabsetPanel(
        tabPanel("Control",
          controlParameters_UI(ns("control_parameters"))
        ),
        tabPanel("Treated 1",
          dataParameters_UI(ns("treated_parameters_1"),
            base_gap_default = 5,
            event_default = 3,
            permanent_effect_default = 8
          )
        ),
        tabPanel("Treated 2",
          dataParameters_UI(ns("treated_parameters_2"),
            event_default = 6
          )
        )
      )
    ),
    mainPanel(
      plotOutput(ns("event_plot")),
      h3("Goodman-Bacon Decomposition"),
      uiOutput(ns("analytic_decomposition"))
    )
  )
}

twoEvents_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeline <- seq(0, 10)
    control_group <- controlParameters_Server("control_parameters", timeline)
    treated_groups <- list(
      dataParameters_Server("treated_parameters_1"),
      dataParameters_Server("treated_parameters_2")
    )

    data_event <- reactive(
      bind_rows(
        generate_group(
          name = "control",
          timeline = timeline,
          event = +Inf,
          size = control_group$size(),
          common_trend = control_group$common_trend(),
          base_gap = 0,
          permanent_effect = 0,
          ponctual_effect = 0,
          slope_effect = 0
        ),
        generate_group(
          name = "treated_1",
          timeline = timeline,
          event = treated_groups[[1]]$event(),
          size = treated_groups[[1]]$size(),
          common_trend = control_group$common_trend(),
          base_gap = treated_groups[[1]]$base_gap(),
          permanent_effect = treated_groups[[1]]$permanent_effect(),
          ponctual_effect = treated_groups[[1]]$ponctual_effect(),
          slope_effect = treated_groups[[1]]$slope_effect()
        ),
        generate_group(
          name = "treated_2",
          timeline = timeline,
          event = treated_groups[[2]]$event(),
          size = treated_groups[[2]]$size(),
          common_trend = control_group$common_trend(),
          base_gap = treated_groups[[2]]$base_gap(),
          permanent_effect = treated_groups[[2]]$permanent_effect(),
          ponctual_effect = treated_groups[[2]]$ponctual_effect(),
          slope_effect = treated_groups[[2]]$slope_effect()
        )
      )
    )

    nb_times <- length(timeline)
    n_tot <- reactive(
      treated_groups[[1]]$size() +
      treated_groups[[2]]$size() +
      control_group$size()
    )
    n_u <- reactive(control_group$size() / n_tot())
    n_1 <- reactive(treated_groups[[1]]$size() / n_tot())
    n_2 <- reactive(treated_groups[[2]]$size() / n_tot())
    n_1u <- reactive(n_1() / (n_1() + n_u()))
    n_2u <- reactive(n_2() / (n_2() + n_u()))
    n_12 <- reactive(n_1() / (n_1() + n_2()))
    d_1 <- reactive((nb_times - treated_groups[[1]]$event()) / nb_times)
    d_2 <- reactive((nb_times - treated_groups[[2]]$event()) / nb_times)
    v_1u <- reactive(n_1u() * (1 - n_1u()) * d_1() * (1 - d_1()))
    s_1u <- reactive((n_1() + n_u())^2 * v_1u())
    v_2u <- reactive(n_2u() * (1 - n_2u()) * d_2() * (1 - d_2()))
    s_2u <- reactive((n_2() + n_u())^2 * v_2u())
    v_12_1 <- reactive(
      n_12() * (1 - n_12()) * (d_1() - d_2()) * (1 - d_1()) / (1 - d_2())^2
    )
    s_12_1 <- reactive(((n_1() + n_2()) * (1 - d_2()))^2 * v_12_1())
    v_12_2 <- reactive(
      n_12() * (1 - n_12()) * d_2() * (d_1() - d_2()) / d_1()^2
    )
    s_12_2 <- reactive(
      ((n_1() + n_2()) * d_1())^2 * v_12_2()
    )
    s_tot <- reactive(s_1u() + s_2u() + s_12_1() + s_12_2())
    s_1u_n <- reactive(s_1u() / s_tot())
    s_2u_n <- reactive(s_2u() / s_tot())
    s_12_1_n <- reactive(s_12_1() / s_tot())
    s_12_2_n <- reactive(s_12_2() / s_tot())

    did_estimate <- function(data, groups, event) {
      y_post_t <- data[data$group == groups$treated & data$t >= event, ][["y"]]
      y_post_u <- data[data$group == groups$control & data$t >= event, ][["y"]]
      y_pre_t <- data[data$group == groups$treated & data$t < event, ][["y"]]
      y_pre_u <- data[data$group == groups$control & data$t < event, ][["y"]]

      diff_post <- mean(y_post_t - y_post_u)
      diff_pre <- mean(y_pre_t - y_pre_u)

      ifelse(is.na(diff_post), 0, diff_post) - ifelse(is.na(diff_pre), 0, diff_pre)
    }

    beta_1u <- reactive({
      did_estimate(
        data_event(),
        list(treated = "treated_1", control = "control"),
        treated_groups[[1]]$event()
      )
    })

    beta_2u <- reactive({
      did_estimate(
        data_event(),
        list(treated = "treated_2", control = "control"),
        treated_groups[[2]]$event()
      )
    })

    beta_12_1 <- reactive({
      did_estimate(
        data_event() |> filter(t < treated_groups[[2]]$event()),
        list(treated = "treated_1", control = "treated_2"),
        treated_groups[[1]]$event()
      )
    })

    beta_12_2 <- reactive({
      did_estimate(
        data_event() |> filter(t >= treated_groups[[1]]$event()),
        list(treated = "treated_2", control = "treated_1"),
        treated_groups[[2]]$event()
      )
    })

    pp <- function(x, ...) formatC(x, digits = 2, ...)
    output$analytic_decomposition <- renderUI({
      withMathJax(HTML(
        paste0("$$\\overline{D}_1 = ", pp(d_1()), "\\quad ",
               "\\overline{D}_2 = ", pp(d_2()), "$$"),
        paste0("$$s_{1U} = ", pp(s_1u_n()), "\\quad ",
               "s_{2U} = ", pp(s_2u_n()), "$$"),
        paste0("$$s_{12}^1 = ", pp(s_12_1_n()), "\\quad ",
               "s_{12}^2 = ", pp(s_12_2_n()), "$$"),
        "$$s_{1U}\\beta_{1U} + s_{2U}\\beta_{2U} + s_{12}^1\\beta_{12}^1 + s_{12}^2\\beta_{12}^2 = \\beta$$",
        paste0(
          "$$", pp(s_1u_n()), "\\times ", pp(beta_1u()),
          " + ", pp(s_2u_n()), "\\times ", pp(beta_2u()),
          " + ", pp(s_12_1_n()), "\\times ", pp(beta_12_1()),
          " + ", pp(s_12_2_n()), "\\times ", pp(beta_12_2()),
          " = ", pp(
            s_1u_n() * beta_1u() +
              s_12_1_n() * beta_12_1() +
              s_2u_n() * beta_2u() +
              s_12_2_n() * beta_12_2()
          ),
          "$$"
        )
      ))
    })

    output$event_plot <- renderPlot({
      plot_data(
        pp_table(data_event()),
        c(treated_groups[[1]]$event(), treated_groups[[2]]$event()),
        timeline
      )
    })

    data_event
  })
}
