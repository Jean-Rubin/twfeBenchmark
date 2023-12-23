multiEvents_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h3("Parameters", style = "magin-top: 0;"),
        multiParameters_UI(ns("parameters")),
      ),
      mainPanel(
        plotOutput(ns("event_plot"))
      )
    )
  )
}

multiEvents_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeline <- seq(0L, 10L)
    params_groups <- multiParameters_Server("parameters")
    control_group <- params_groups$control
    treated_groups <- reactive(params_groups$treated())

    data_event <- reactive({
      bind_rows(
        generate_group(
          name = "Never treated",
          timeline = timeline,
          event = +Inf,
          group_size = control_group$control_size(),
          common_trend = control_group$common_trend(),
          base_gap = 0,
          permanent_effect = 0,
          ponctual_effect = 0,
          slope_effect = 0
        ),
        purrr::imap(treated_groups(), function(treated_group, i) {
          generate_group(
            name = paste("Treated", i),
            timeline = timeline,
            event = treated_group$event(),
            group_size = treated_group$group_size(),
            common_trend = control_group$common_trend(),
            base_gap = treated_group$base_gap(),
            permanent_effect = treated_group$permanent_effect(),
            ponctual_effect = treated_group$ponctual_effect(),
            slope_effect = treated_group$slope_effect()
          )
        })
      )
    })

    treated_events <- reactive({
      purrr::map(treated_groups(), function(treated_group) {
        treated_group$event()
      }) |> purrr::flatten_int()
    })

    output$event_plot <- renderPlot({
      plot_data(
        data_event(),
        treated_events(),
        timeline
      )
    })

    data_event
  })
}
