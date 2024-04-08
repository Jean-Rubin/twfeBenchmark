goodmanBaconDecomp_UI <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Pairwise Comparison",
      sidebarLayout(
        sidebarPanel(
          h3("Groups", style = "margin-top: 0;"),
          selectInput(ns("select_group_1"),
            "Select first group",
            choices = pp_col("control")
          ),
          selectInput(ns("select_group_2"),
            "Select second group",
            choices = pp_col("control")
          )
        ),
        mainPanel(
          column(6,
            plotOutput(ns("pairwise_plot_1")),
            uiOutput(ns("analytic_decomposition_1"))
          ),
          column(6,
            plotOutput(ns("pairwise_plot_2")),
            uiOutput(ns("analytic_decomposition_2"))
          )
        )
      )
    ),
    tabPanel("Global Overview")
  )
}

goodmanBaconDecomp_Server <- function(id, params_group_flat, data_event) {
  moduleServer(id, function(input, output, session) {
    timeline <- session$userData$timeline
    groups_name <- reactive({
      data_event() |>
        pull(group) |>
        unique() |>
        pp_col()
    })

    second_groups_name <- reactive(setdiff(groups_name(), input$select_group_1))

    observe({
      updateSelectInput(session, "select_group_1", choices = groups_name())
    })

    observe({
      updateSelectInput(session, "select_group_2", choices = second_groups_name())
    })

    group_1 <- reactive(unpp_col(input$select_group_1))
    group_2 <- reactive(unpp_col(input$select_group_2))
    params_group_1 <- reactive(params_group_flat()[[group_1()]])
    params_group_2 <- reactive(params_group_flat()[[group_2()]])


    goodman_bacon_coefs <- reactive({
      if (is.null(input$select_group_1) || is.null(input$select_group_2)) {
        return()
      }

      goodman_bacon_coef(
        length(timeline),
        params_group_1(),
        params_group_2()
      )
    })

    goodman_bacon_did_estimates <- reactive({
      if (is.null(input$select_group_1) || is.null(input$select_group_2)) {
        return()
      }

      did_estimates(
        data_event(),
        append(params_group_1(), list(name = group_1())),
        append(params_group_2(), list(name = group_2()))
      )
    })

    comparison <- reactive({
      if (params_group_1()$event < params_group_2()$event) `<` else `>=`
    })

    output$pairwise_plot_1 <- renderPlot({
      data_event() |>
        filter(
          group %in% c(group_1(), group_2()),
          comparison()(t, params_group_2()$event)
        ) |>
        pp_table() |>
        plot_data(
          c(params_group_1()$event, params_group_2()$event),
          timeline
        )
    })

    output$pairwise_plot_2 <- renderPlot({
      data_event() |>
        filter(
          group %in% c(group_1(), group_2()),
          !comparison()(t, params_group_1()$event)
        ) |>
        pp_table() |>
        plot_data(
          c(params_group_1()$event, params_group_2()$event),
          timeline
        )
    })

    pp <- function(x, ...) formatC(x, digits = 2, ...)

    output$analytic_decomposition_1 <- renderUI({
      withMathJax(HTML(paste0(
        "$$",
        "s_{12}^1 = ", pp(goodman_bacon_coefs()$s_12_1), "\\quad",
        "\\beta_{12}^1 = ", pp(goodman_bacon_did_estimates()$beta_12_1),
        "$$"
      )))
    })

    output$analytic_decomposition_2 <- renderUI({
      withMathJax(HTML(paste0(
        "$$",
        "s_{12}^2 = ", pp(goodman_bacon_coefs()$s_12_2), "\\quad",
        "\\beta_{12}^2 = ", pp(goodman_bacon_did_estimates()$beta_12_2),
        "$$"
      )))
    })

  })
}
