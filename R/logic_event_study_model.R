event_study_model <- function(data_ind, specification) {
  data_renamed <- data_ind |>
    mutate(
      "Individual FE" = ind,
      "Temporal FE" = factor(t)
    )

  relative_time <- as.numeric(specification$t_rel)
  for (l in relative_time) {
    data_renamed <- data_renamed |>
      mutate(
        "Relative Time {{l}}" := as.numeric((t - event) == l),
      )
  }
  has_group <- length(specification$group) > 0
  if (has_group) {
    data_renamed <- data_renamed |>
      mutate(
        "Relative Time Group" = as.numeric((t - event) %in% specification$group)
      )
  }

  relative_time_vars <- paste0("`Relative Time ", relative_time, "`")

  formula_char <- paste0(
    "y ~ ",
    paste(relative_time_vars, collapse = " + "),
    if (has_group) " + `Relative Time Group`" else "",
    " + `Individual FE` + `Temporal FE`"
  )

  lm(formula_char, data = data_renamed)
}
