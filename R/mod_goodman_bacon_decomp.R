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
            uiOutput(ns("pairwise_analytic_decomposition_1"))
          ),
          column(6,
            plotOutput(ns("pairwise_plot_2")),
            uiOutput(ns("pairwise_analytic_decomposition_2"))
          )
        )
      )
    ),
    tabPanel("Global Overview",
      sidebarLayout(
        sidebarPanel(
          h3("Estimation", style = "margin-top: 0;"),
          uiOutput(ns("global_analytic_decomposition")),
          tableOutput(ns("global_decomposition"))
        ),
        mainPanel(
          plotOutput(ns("global_plot"))
        )
      )
    )
  )
}

goodmanBaconDecomp_Server <- function(id, params_group_flat, data_event) {
  moduleServer(id, function(input, output, session) {
    timeline <- session$userData$timeline
    groups_name <- reactive({
      pp_col(names(params_group_flat()))
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

      did_estimates(data_event(), params_group_1(), params_group_2())
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

    output$pairwise_analytic_decomposition_1 <- renderUI({
      withMathJaxLocal(HTML(paste0(
        "$$",
        "s_{12}^1 = ", pp(goodman_bacon_coefs()$s_12_1), "\\quad",
        "\\beta_{12}^1 = ", pp(goodman_bacon_did_estimates()$beta_12_1),
        "$$"
      )))
    })

    output$pairwise_analytic_decomposition_2 <- renderUI({
      withMathJaxLocal(HTML(paste0(
        "$$",
        "s_{12}^2 = ", pp(goodman_bacon_coefs()$s_12_2), "\\quad",
        "\\beta_{12}^2 = ", pp(goodman_bacon_did_estimates()$beta_12_2),
        "$$"
      )))
    })

    goodman_bacon_df <- reactive({
      tidyr::crossing(
        group_1 = params_group_flat(),
        group_2 = params_group_flat()
      ) |>
        mutate(
          group_1_name = purrr::map_chr(group_1, \(x) x$name),
          group_2_name = purrr::map_chr(group_2, \(x) x$name),
          group_1_event = purrr::map_dbl(group_1, \(x) x$event),
          group_2_event = purrr::map_dbl(group_2, \(x) x$event),
        ) |>
        filter(
          group_2_name != "control",
          group_1_name != group_2_name
        ) |>
        mutate(
          did = purrr::map2_dbl(
            group_1, group_2,
            \(x, y) did_estimates(data_event(), x, y)$beta_12_2
          ),
          did_weight = purrr::map2_dbl(
            group_1, group_2,
            \(x, y) goodman_bacon_coef(length(timeline), x, y)$s_12_2
          )
        ) |>
        mutate(
          did_weight = did_weight / sum(did_weight),
          did_type = case_when(
            group_1_name == "control" ~ "Normal",
            group_1_event < group_2_event ~ "Early as control",
            .default = "Late as control"
          )
        ) |>
        select(group_1_name, group_2_name, did_type, did, did_weight)
    })

    output$global_analytic_decomposition <- renderUI({
      withMathJaxLocal(HTML(paste0(
        "$$",
        "\\beta = \\sum_{k, l} s_{k,l}^k \\beta_{k,l}^{k} = ",
        goodman_bacon_df() |>
          summarize(result = sum(did * did_weight)) |>
          pull(result),
        "$$"
      )))
    })

    output$global_decomposition <- renderTable({
      goodman_bacon_df() |>
        mutate(
          across(c(group_1_name, group_2_name), pp_col)
        ) |>
        rename(
          Control = group_1_name,
          Treated = group_2_name,
          Type = did_type,
          DiD = did,
          Weight = did_weight
        )
    })


    output$global_plot <- renderPlot({
      goodman_bacon_df() |>
      ggplot(aes(x = did_weight, y = did, colour = did_type)) +
        geom_point(size = 6) +
        labs(
          x = "Weight",
          y = "DiD estimate",
          colour = "Comparison type"
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(-15, 15)) +
        theme_common()
    })

  })
}
