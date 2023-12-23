basic_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Parameters", style = "margin-top: 0;"),
      tabsetPanel(
        tabPanel("Control",
          controlParameters_UI(ns("control_parameters"))
        ),
        tabPanel("Treated",
          dataParameters_UI(ns("treated_parameters"))
        )
      )
    ),
    mainPanel(plotOutput(ns("event_plot")))
  )
}

basic_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeline <- seq(0, 10)
    control_group <- controlParameters_Server("control_parameters", timeline)
    treated_group <- dataParameters_Server("treated_parameters")

    data_event <- reactive(
      bind_rows(
        generate_group(
          name = "control",
          timeline = timeline,
          event = +Inf,
          group_size = control_group$size(),
          common_trend = control_group$common_trend(),
          base_gap = 0,
          permanent_effect = 0,
          ponctual_effect = 0,
          slope_effect = 0
        ),
        generate_group(
          name = "treated",
          timeline = timeline,
          event = treated_group$event(),
          size = treated_group$size(),
          common_trend = control_group$common_trend(),
          base_gap = treated_group$base_gap(),
          permanent_effect = treated_group$permanent_effect(),
          ponctual_effect = treated_group$ponctual_effect(),
          slope_effect = treated_group$slope_effect()
        )
      )
    )

    data_counterfactual <- reactive({
      data_event() |>
        filter(group == "control") |>
        mutate(
          group = "counterfactual",
          y = control_group$common_trend() + treated_group$base_gap(),
          size = treated_group$size()
        )
    })

    output$event_plot <- renderPlot({
      plot_data(pp_table(data_event()), treated_group$event(), timeline) +
        geom_line(data = pp_table(data_counterfactual()), linetype = "dashed")
    })

    data_event
  })
}
