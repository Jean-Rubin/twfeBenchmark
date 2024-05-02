theory_UI <- function(id) {
  ns <- NS(id)
  navset_tab(id = ns("model_theory_tabset"),
    nav_panel("Goodman-Bacon Decomposition",
      goodmanBaconDecomp_UI(ns("goodman_bacon"))
    ),
    nav_panel("de Chaisemartin, D'Haultfoeuille Decomposition",
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
