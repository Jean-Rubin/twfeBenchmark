controlParameters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(
      ns("size"),
      "Control size",
      min = 1,
      max = 5,
      value = 1,
      step = 1
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
      ns("quadratic_trend"),
      "Quadratic trend",
      min = -0.1,
      max = 0.1,
      value = 0,
      step = 0.05
    ),
    sliderInput(
      ns("oscillation_amplitude_trend"),
      "Oscillation amplitude trend",
      min = 0,
      max = 10,
      value = 0,
      step = 1
    ),
    sliderInput(
      ns("oscillation_frequency_trend"),
      "Oscillation frequency trend",
      min = 0,
      max = 5,
      value = 0,
      step = 0.5
    )
  )
}

controlParameters_Server <- function(id, timeline) {
  moduleServer(id, function(input, output, session) {
    normalized_timeline <- 2 * pi * timeline / max(timeline)
    reactive(list(
      size = input$size,
      slope_trend = input$slope_trend,
      quadratic_trend = input$quadratic_trend,
      oscillation_amplitude_trend = input$oscillation_amplitude_trend,
      oscillation_frequency_trend = input$oscillation_frequency_trend,
      common_trend = input$slope_trend * timeline +
        input$quadratic_trend * timeline^2 +
        input$oscillation_amplitude_trend *
        sin(input$oscillation_frequency_trend * normalized_timeline)
    ))
  })
}
