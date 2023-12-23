dataParameters_UI <- function(
    id,
    size_default = 1,
    base_gap_default = 3,
    event_default = 5,
    permanent_effect_default = 5,
    ponctual_effect_default = 0,
    slope_effect_default = 0
) {
  ns <- NS(id)
  tagList(
    sliderInput(
      ns("size"),
      "Group size",
      min = 1,
      max = 5,
      value = size_default
    ),
    sliderInput(
      ns("base_gap"),
      "Base gap",
      min = -5,
      max = 5,
      value = base_gap_default
    ),
    sliderInput(
      ns("event"),
      "Event time",
      min = 0,
      max = 10,
      value = event_default
    ),
    sliderInput(
      ns("permanent_effect"),
      "Permanent effect",
      min = -10,
      max = 10,
      value = permanent_effect_default,
      step = 1
    ),
    sliderInput(
      ns("ponctual_effect"),
      "Ponctual effect",
      min = -10,
      max = 10,
      value = ponctual_effect_default,
      step = 1
    ),
    sliderInput(
      ns("slope_effect"),
      "Slope effect",
      min = -3,
      max = 3,
      value = slope_effect_default,
      step = 0.5
    )
  )
}

dataParameters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(
      size = input$size,
      event = input$event,
      base_gap = input$base_gap,
      permanent_effect = input$permanent_effect,
      ponctual_effect = input$ponctual_effect,
      slope_effect = input$slope_effect
    ))
  })
}
