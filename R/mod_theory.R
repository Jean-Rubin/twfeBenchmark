theory_UI <- function(id) {
  ns <- NS(id)
  tabsetPanel(id = ns("model_theory_tabset"),
    tabPanel("Goodman-Bacon Decomposition",
      goodmanBaconDecomp_UI(ns("goodman_bacon"))
    ),
    tabPanel("de Chaisemartin, D'Haultfoeuille Decomposition",
      DT::DTOutput(ns("w_table"))
    )
  )
}

theory_Server <- function(id, params_group_flat, data_event) {
  moduleServer(id, function(input, output, session) {
    goodmanBaconDecomp_Server("goodman_bacon", params_group_flat, data_event)

    ## DC DH -----
    compute_w <- function(data) {
      # n_treated <- data |>
      #   filter(treated) |>
      #   group_by(group) |>
      #   slice_head(n = 1) |>
      #   summarize(n = sum(size)) |>
      #   pull(n)

      data |>
        group_by(group) |>
        mutate(treated_group = mean(treated)) |>
        ungroup() |>
        group_by(t) |>
        mutate(treated_t = mean(treated)) |>
        ungroup() |>
        mutate(treated_mean = mean(treated)) |>
        mutate(
          eps = treated - treated_group - treated_t + treated_mean,
          w = size * eps
        ) |>
        filter(treated == 1) |>
        mutate(w = w / sum(w)) |>
        select(group, t, treated, treated_group, treated_t, treated_mean, eps, w)
    }

    output$w_table <- DT::renderDT({
      compute_w(data_event()) |>
        DT::datatable() |>
        DT::formatRound(c(
          "treated_group",
          "treated_t",
          "treated_mean",
          "eps",
          "w"
        ), digits = 3)
    })

  })
}
