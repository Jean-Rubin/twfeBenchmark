multiEvents_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Parameters", style = "margin-top: 0;"),
      multiParameters_UI(ns("parameters"))
    ),
    mainPanel(
      plotOutput(ns("event_plot"))
    )
  )
}

multiEvents_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeline <- session$userData$timeline
    params_groups <- multiParameters_Server("parameters")
    control_group <- params_groups$control
    treated_groups <- reactive(params_groups$treated())

    data_event <- reactive({
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
        purrr::imap(treated_groups(), function(treated_group, i) {
          generate_group(
            name = paste("treated", i, sep = "_"),
            timeline = timeline,
            event = treated_group$event(),
            size = treated_group$size(),
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
        pp_table(data_event()),
        treated_events(),
        timeline
      )
    })

    data_event
  })
}
