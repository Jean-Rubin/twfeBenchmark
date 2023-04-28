# library(shiny)
# library(dplyr)
# library(ggplot2)

run_twfe_app <- function(...) {
  shinyOptions(shiny.mathjax.url = "MathJax.js")
  ui <- fluidPage(
    withMathJax(),
    titlePanel("Two-Way Fixed Effect Estimator"),
    tabsetPanel(
      tabPanel("Basic DID", basicDID_UI("basic_did")),
      tabPanel("Two Events", twoEventsDID_UI("two_events_did"))
    )
  )

  server <- function(input, output, session) {
    basicDID_Server("basic_did")
    twoEventsDID_Server("two_events_did")
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, ...)
}
