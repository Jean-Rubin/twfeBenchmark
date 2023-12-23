twfe_did_model <- function(data_ind) {
  data_renamed <- data_ind |>
    mutate(
      "Individual FE" = ind,
      "Temporal FE" = factor(t),
      "Treatment Effect" = treated
    )

  lm(
    y ~ `Treatment Effect` + `Individual FE` + `Temporal FE`,
    data = data_renamed
  )
}
