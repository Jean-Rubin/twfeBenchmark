multiParameters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_treated"),
      "Add Treated", class = "btn-success", icon = icon("plus")
    ),
    actionButton(ns("reset_treated"),
      "Remove All Treated", class = "btn-danger", icon = icon("refresh")
    ),
    tabsetPanel(id = ns("parameters"),
      tabPanel("Control", value = "tab_control",
        controlParameters_UI(ns("control_parameters"))
      )
    )
  )
}

multiParameters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    timeline <- seq(0L, 10L)
    max_treated <- 10L
    treated_id_pool <- reactiveVal(seq_len(max_treated))
    treated_ids <- reactive(setdiff(seq_len(max_treated), treated_id_pool()))
    ns_treated <- function(id, n) paste0(id, "_treated_", n)

    # Add treatment group
    observe({
      if (length(treated_id_pool()) == 0L) return()
      n <- min(treated_id_pool())
      insertTab(
        "parameters",
        tabPanel(paste("Treated", n), value = ns_treated("tab", n),
          dataParameters_UI(ns(ns_treated("params", n))),
          actionButton(ns(ns_treated("remove", n)),
            "Remove Treated", class = "btn-danger", icon = icon("minus")
          )
        )
      )
      updateTabsetPanel(session, "parameters", selected = ns_treated("tab", n))
      treated_id_pool(setdiff(treated_id_pool(), n))
    }) |>
      bindEvent(input$add_treated)

    # Remove treatment group
    purrr::walk(seq_len(max_treated), function(n) {
      observe({
        removeTab("parameters", ns_treated("tab", n))
        treated_id_pool(c(treated_id_pool(), n))
      }) |>
        bindEvent(input[[ns_treated("remove", n)]])
    })

    # Reset treatment groups
    observe({
      purrr::walk(seq_len(max_treated), function(n) {
        removeTab("parameters", ns_treated("tab", n))
      })
      treated_id_pool(seq_len(max_treated))
    }) |>
      bindEvent(input$reset_treated)


    control_group <- controlParameters_Server("control_parameters", timeline)
    treated_groups <- reactive({
      purrr::map(treated_ids(), function(n) {
        dataParameters_Server(ns_treated("params", n))
      })
    })


    list(
      control = control_group,
      treated = treated_groups
    )
  })
}
