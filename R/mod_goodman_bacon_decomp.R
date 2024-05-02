goodmanBaconDecomp_UI <- function(id) {
  ns <- NS(id)

  navset_tab(
    nav_panel("Global Overview",
      layout_columns(
        card(
          card_header("Estimation"),
          uiOutput(ns("global_analytic_decomposition")),
          tableOutput(ns("global_decomposition"))
        ),
        plotOutput(ns("global_plot"))
      )
    ),
    nav_panel("Pairwise Comparison",
      layout_sidebar(
        sidebar = sidebar(
          title = "Groups",
          selectInput(ns("select_group_1"),
            "Select first group",
            choices = pp_col("control")
          ),
          selectInput(ns("select_group_2"),
            "Select second group",
            choices = pp_col("control")
          )
        ),
        layout_columns(
          card(
            full_screen = TRUE,
            card_header("Effect of first group using second group as control"),
            plotOutput(ns("pairwise_plot_1")),
            card_footer(uiOutput(ns("pairwise_analytic_decomposition_1")))
          ),
          card(
            full_screen = TRUE,
            card_header("Effect of second group using first group as control"),
            plotOutput(ns("pairwise_plot_2")),
            card_footer(uiOutput(ns("pairwise_analytic_decomposition_2")))
          )
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

    group_1 <- reactive({
      unpp_col(input$select_group_1)
    })
    group_2 <- reactive({
      unpp_col(input$select_group_2)
    })

    params_group_1 <- reactive({
      params_group_1 <- params_group_flat()[[group_1()]]
      req(!is.null(params_group_1$size))
      params_group_1
    })
    params_group_2 <- reactive({
      params_group_2 <- params_group_flat()[[group_2()]]
      req(!is.null(params_group_2$size))
      params_group_2
    })

    missing_selection <- reactive({
      is.null(input$select_group_1) || is.null(input$select_group_2)
    })

    goodman_bacon_coefs <- reactive({
      goodman_bacon_decomp_params(
        length(timeline),
        params_group_1(),
        params_group_2()
      )
    })

    goodman_bacon_did_estimates <- reactive({
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
        "$$\\overline{T}_1 = ", pp(1 - goodman_bacon_coefs()$d_2), "$$",
        "$$",
        "\\begin{align*}",
        "\\beta_{12}^1 &= ", pp(goodman_bacon_did_estimates()$beta_12_1), "\\\\",
        "s_{12}^1 &= ", pp(goodman_bacon_coefs()$s_12_1), "\\\\",
        "\\hline",
        "\\big((n_1 + n_2) \\cdot \\overline{T}_1\\big)^2 &= ", pp(goodman_bacon_coefs()$n_12_1), "\\\\",
        "n_{12} \\cdot (1 - n_{12}) &= ", pp(goodman_bacon_coefs()$v_n_12), "\\\\",
        "\\frac{\\overline{D}_1^1}{\\overline{T}_1} \\cdot \\frac{\\overline{D}_2^1}{\\overline{T}_1} &= ", pp(goodman_bacon_coefs()$v_d_12_1), "\\\\",
        "\\end{align*}",
        "$$"
      )))
    })

    output$pairwise_analytic_decomposition_2 <- renderUI({
      withMathJaxLocal(HTML(paste0(
        "$$\\overline{T}_2 = ", pp(goodman_bacon_coefs()$d_1), "$$",
        "$$",
        "\\begin{align*}",
        "\\beta_{12}^2 &= ", pp(goodman_bacon_did_estimates()$beta_12_2), "\\\\",
        "s_{12}^2 &= ", pp(goodman_bacon_coefs()$s_12_2), "\\\\",
        "\\hline",
        "\\big((n_1 + n_2) \\cdot \\overline{T}_2\\big)^2 &= ", pp(goodman_bacon_coefs()$n_12_2), "\\\\",
        "n_{12} \\cdot (1 - n_{12}) &= ", pp(goodman_bacon_coefs()$v_n_12), "\\\\",
        "\\frac{\\overline{D}_1^2}{\\overline{T}_2} \\cdot \\frac{\\overline{D}_2^2}{\\overline{T}_2} &= ", pp(goodman_bacon_coefs()$v_d_12_2), "\\\\",
        "\\end{align*}",
        "$$"
      )))
    })

    goodman_bacon_df <- reactive({
      req(length(groups_name()) > 1) # Need at least a treated group
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
          goodman_bacon_coefs = purrr::map2(
            group_1, group_2,
            \(x, y) {
              res <- goodman_bacon_decomp_params(length(timeline), x, y)
              list(
                pure_size_coef = res$n_12_2,
                variance_size_coef = res$v_n_12,
                variance_treatment_coef = res$v_d_12_2,
                did_coef = res$s_12_2
              )
            }
          )
        ) |>
        tidyr::unnest_wider(goodman_bacon_coefs) |>
        mutate(
          did_weight = did_coef / sum(did_coef),
          did_type = case_when(
            group_1_name == "control" ~ "Normal",
            group_1_event < group_2_event ~ "Early as control",
            .default = "Late as control"
          )
        ) |>
        select(
          group_1_name, group_2_name,
          did_type, did, did_weight
          # did_coef, pure_size_coef, variance_size_coef, variance_treatment_coef
        )
    })

    output$global_analytic_decomposition <- renderUI({
      withMathJaxLocal(HTML(paste0(
        "$$",
        "\\beta = \\frac{\\sum_{k, l} s_{k,l}^k \\beta_{k,l}^{k}}{\\sum_{k,l} s_{k,l}^k} = ",
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
