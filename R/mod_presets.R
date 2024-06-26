presets_UI <- function(id) {
  ns <- NS(id)
  navset_tab(
    nav_panel(
      "One group",
      sidebarLayout(
        sidebarPanel(
          h3("One treatment group", style = "margin-top: 0;"),
          p("In this preset, there is one treated group and one control group."),
          actionButton(ns("preset_1"), "Select Preset")
        ),
        mainPanel(
          h3("What you can check"),
          tags$ul(
            tags$li(paste(
              "Basic two-way fixed effect model correctly estimate",
              "a difference in difference."
            ))
          )
        )
      )
    ),
    nav_panel(
      "Two groups",
      sidebarLayout(
        sidebarPanel(
          h3("Two treated groups", style = "margin-top: 0;"),
          p("In this preset, there are two treated groups and one control group."),
          p(paste(
            "The treatments aren't dynamic,",
            "ie. each group has a constant permanent effect from their respective treatment date."
          )),
          actionButton(ns("preset_2"), "Select Preset")
        ),
        mainPanel(
          h3("What you can check"),
          h4("Goodman-bacon decomposition"),
          tags$ul(
            tags$li(paste(
              "Basic TWFE estimate can be seen as a weighted sum of difference in difference."
            )),
            tags$li(paste(
              "However these difference in difference can use treated groups as control."
            )),
            tags$li(paste(
              "The weights are proportional to a form of balance in the comparison.",
              "More precisely, DiD with equal amount of treated individuals and control individuals",
              "and equal amount of treated times and untreated times",
              "will have a higher weight.",
              "Of course, there is also a pure size effect,",
              "ie. DiD with more individuals will have a higher weight."
            ))
          ),
          h4("de Chaisemartin, D'Haultfoeuille decomposition"),
          tags$ul(
            tags$li(paste(
              "Basic TWFE estimate",
              "can be seen as a weighted sum of the treatment effects of each group at each date."
            )),
            tags$li(paste(
              "Although the weights sum to one,",
              "some of them may be negative.",
              "In such a situation,",
              "the estimate can therefore violate the 'no sign-reversal' property:",
              "every treatment effect can be positive and the estimate can still be negative."
            )),
            tags$li(paste(
              "In a staggered design,",
              "the weights are decreasing with time.",
              "The potentially problematic weights are thus likely to be at the end."
            ))
          )
        )
      )
    ),
    nav_panel(
      "Dynamic Treatment",
      sidebarLayout(
        sidebarPanel(
          h3("Two treated groups with dynamic treatment effects", style = "margin-top: 0;"),
          p("In this preset, there are two treated groups and one control group."),
          p(paste(
            "The treatments are dynamic,",
            "ie. each group has a treatment effect that evolve through time."
          )),
          p(paste(
            "However, the treatments are cohort-homogenous,",
            "ie. each group has the same profile of treatment effect relative to the treatment date."
          )),
          actionButton(ns("preset_3"), "Select Preset")
        ),
        mainPanel(
          h3("What you can check"),
          h4("Goodman-bacon decomposition"),
          tags$ul(
            tags$li(paste(
              "Basic TWFE estimate can be seen as a weighted sum of difference in difference."
            )),
            tags$li(paste(
              "Although the weights are positive and sum to one,",
              "some DiD may have a different sign than the global treatment effect."
            ))
          ),
          h4("de Chaisemartin, D'Haultfoeuille decomposition"),
          tags$ul(
            tags$li(paste(
              "Basic TWFE estimate",
              "can be seen as a weighted sum of the treatment effects of each group at each date."
            )),
            tags$li(paste(
              "Although the weights sum to one,",
              "some of them may be negative.",
              "In such a situation,",
              "the estimate can therefore violate the 'no sign-reversal' property:",
              "every treatment effect can be positive and the estimate can still be negative."
            )),
            tags$li(paste(
              "In a staggered design,",
              "the weights are decreasing with time.",
              "The potentially problematic weights are thus likely to be at the end."
            ))
          ),
          h4("Sun, Abraham decomposition"),
          tags$ul(
            tags$li(paste(
              "In an event study,",
              "treatment may be dynamic but shall be homogenous across groups."
            ))
          )
        )
      )
    )
  )
}

presets_Server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    # Setting preset 1
    update_preset_1 <- reactiveVal(0)
    observe({
      updateTabsetPanel(
        session = parent_session,
        "parameters_tabset",
        selected = "outcome_plot"
      )
      shinyjs::delay(50, shinyjs::click("parameters-reset_treated", asis = TRUE))
      shinyjs::delay(2000, shinyjs::click("parameters-add_treated", asis = TRUE))
      shinyjs::delay(3000, update_preset_1(isolate(1 - update_preset_1())))
    }) |>
      bindEvent(input$preset_1)

    # Setting preset 2
    update_preset_2 <- reactiveVal(0)
    observe({
      updateTabsetPanel(
        session = parent_session,
        "parameters_tabset",
        selected = "outcome_plot"
      )
      shinyjs::delay(50, shinyjs::click("parameters-reset_treated", asis = TRUE))
      shinyjs::delay(2000, shinyjs::click("parameters-add_treated", asis = TRUE))
      shinyjs::delay(3000, shinyjs::click("parameters-add_treated", asis = TRUE))
      shinyjs::delay(4000, update_preset_2(isolate(1 - update_preset_2())))
    }) |>
      bindEvent(input$preset_2)

    observe({
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-base_gap",
        value = 5
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-event",
        value = 3
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-permanent_effect",
        value = 8
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_2-event",
        value = 6
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_2-permanent_effect",
        value = 3
      )
    }) |>
    bindEvent(update_preset_2())

    # Setting preset 3
    update_preset_3 <- reactiveVal(0)
    observe({
      updateTabsetPanel(
        session = parent_session,
        "parameters_tabset",
        selected = "outcome_plot"
      )
      shinyjs::delay(50, shinyjs::click("parameters-reset_treated", asis = TRUE))
      shinyjs::delay(2000, shinyjs::click("parameters-add_treated", asis = TRUE))
      shinyjs::delay(3000, shinyjs::click("parameters-add_treated", asis = TRUE))
      shinyjs::delay(4000, update_preset_3(isolate(1 - update_preset_3())))
    }) |>
      bindEvent(input$preset_3)

    observe({
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-base_gap",
        value = 5
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-event",
        value = 2
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-permanent_effect",
        value = 2
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_1-slope_effect",
        value = 2
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_2-event",
        value = 6
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_2-permanent_effect",
        value = 2
      )
      updateSliderInput(
        session = parent_session,
        "parameters-params_treated_2-slope_effect",
        value = 2
      )
    }) |>
      bindEvent(update_preset_3())

  })
}
