theory_UI <- function(id) {
  ns <- NS(id)
  tabsetPanel(id = ns("model_theory_tabset"),
    tabPanel("Goodman-Bacon Decomposition",
      goodmanBaconDecomp_UI(ns("goodman_bacon"))
    ),
    tabPanel("de Chaisemartin, D'Haultfoeuille Decomposition",
      dCdHDecomp_UI(ns("dcdh"))
    )
  )
}

theory_Server <- function(id, params_group_flat, data_event) {
  moduleServer(id, function(input, output, session) {
    goodmanBaconDecomp_Server("goodman_bacon", params_group_flat, data_event)
    dCdHDecomp_Server("dcdh", data_event)
  })
}
