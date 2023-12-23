iw_model <- function(data_ind) {
  data_renamed <- data_ind |>
    mutate(
      "Individual FE" = ind,
      "Temporal FE" = factor(t),
      "Relative Time_-9" = as.numeric((t - event) == -9),
      "Relative Time_-8" = as.numeric((t - event) == -8),
      "Relative Time_-7" = as.numeric((t - event) == -7),
      "Relative Time_-6" = as.numeric((t - event) == -6),
      "Relative Time_-5" = as.numeric((t - event) == -5),
      "Relative Time_-4" = as.numeric((t - event) == -4),
      "Relative Time_-3" = as.numeric((t - event) == -3),
      "Relative Time_-2" = as.numeric((t - event) == -2),
      "Relative Time_-1" = as.numeric((t - event) == -1),
      "Relative Time_0" =  as.numeric((t - event) == 0),
      "Relative Time_1" =  as.numeric((t - event) == 1),
      "Relative Time_2" =  as.numeric((t - event) == 2),
      "Relative Time_3" =  as.numeric((t - event) == 3),
      "Relative Time_4" =  as.numeric((t - event) == 4),
      "Relative Time_5" = as.numeric((t - event) == 5),
      "Relative Time_6" = as.numeric((t - event) == 6),
      "Relative Time_7" = as.numeric((t - event) == 7),
      "Relative Time_8" = as.numeric((t - event) == 8),
      "Relative Time_9" = as.numeric((t - event) == 9),
      "Treated_1" = as.numeric(group == "Treated 1"),
      "Treated_2" = as.numeric(group == "Treated 2")
    )

  relative_time <- -9:9
  relative_time_vars <- paste0("`Relative Time_", relative_time, "`")
  treated_vars <- c("Treated_1", "Treated_2")
  interaction_vars <- tidyr::crossing(treated_vars, relative_time_vars) |>
    mutate(interaction_vars = paste0("I(", treated_vars, " * ", relative_time_vars, ")")) |>
    pull(interaction_vars)

  formula_char <- paste0(
    "y ~ `Individual FE` + `Temporal FE` + ",
    paste(interaction_vars, collapse = " + ")
  )

  lm(formula_char, data = data_renamed)
}
