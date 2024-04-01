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
          plotOutput(ns("goodman_bacon_pairwise_plot")),
          uiOutput(ns("analytic_decomposition"))
        )
      )
    ),
    tabPanel("Global Overview")
  )
}

goodmanBaconDecomp_Server <- function(id, params_group_flat, data_event) {
  moduleServer(id, function(input, output, session) {
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


    goodman_bacon_coefs <- reactive({
      if (is.null(input$select_group_1) || is.null(input$select_group_2)) {
        return()
      }

      goodman_bacon_coef(
        11,
        params_group_flat()[[unpp_col(input$select_group_1)]],
        params_group_flat()[[unpp_col(input$select_group_2)]]
      )
    })

    pp <- function(x, ...) formatC(x, digits = 2, ...)
    output$analytic_decomposition <- renderUI({
      withMathJax(HTML(goodman_bacon_coefs()[[1]]

        # paste0("$$\\overline{D}_1 = ", pp(d_1()), "\\quad ",
        #        "\\overline{D}_2 = ", pp(d_2()), "$$"),
        # paste0("$$s_{1U} = ", pp(s_1u_n()), "\\quad ",
        #        "s_{2U} = ", pp(s_2u_n()), "$$"),
        # paste0("$$s_{12}^1 = ", pp(s_12_1_n()), "\\quad ",
        #        "s_{12}^2 = ", pp(s_12_2_n()), "$$"),
        # "$$s_{1U}\\beta_{1U} + s_{2U}\\beta_{2U} + s_{12}^1\\beta_{12}^1 + s_{12}^2\\beta_{12}^2 = \\beta$$",
        # paste0(
        #   "$$", pp(s_1u_n()), "\\times ", pp(beta_1u()),
        #   " + ", pp(s_2u_n()), "\\times ", pp(beta_2u()),
        #   " + ", pp(s_12_1_n()), "\\times ", pp(beta_12_1()),
        #   " + ", pp(s_12_2_n()), "\\times ", pp(beta_12_2()),
        #   " = ", pp(
        #     s_1u_n() * beta_1u() +
        #       s_12_1_n() * beta_12_1() +
        #       s_2u_n() * beta_2u() +
        #       s_12_2_n() * beta_12_2()
        #   ),
        #   "$$"
        # )
      ))
    })
  })
}
