get_leads_lags_names <- function(specification) {
  leads_lags_names <- sort(as.numeric(c(specification$t_rel, specification$ref)))
  if (length(specification$group > 0)) {
    leads_lags_names <- c("group_t_rel", leads_lags_names)
  }

  leads_lags_names
}

event_study_model <- function(data_ind, specification) {
  data_renamed <- data_ind |>
    mutate(
      t_rel = t - event
    )

  relative_time_fit <- as.numeric(specification$t_rel)
  relative_time_ref <- as.numeric(specification$ref)
  relative_time <- c(relative_time_fit, relative_time_ref)

  # Currently keep control group
  if (specification$balance) {
    data_renamed <- data_renamed |>
      filter(t_rel %in% c(relative_time, -Inf)) |>
      group_by(ind) |>
      filter(all(relative_time %in% t_rel) || all(is.infinite(t_rel))) |>
      ungroup()
  }

  has_group <- length(specification$group) > 0
  has_ref <- length(specification$ref) > 0
  if (has_group) {
    data_renamed <- data_renamed |>
      mutate(
        "group_t_rel" = as.numeric((t - event) %in% specification$group)
      )
  }

  relative_time_char <- capture.output(dput(relative_time_fit))
  time_ref <- capture.output(dput(specification$ref))
  formula_model <- as.formula(glue::glue(
    "y ~ ",
    "i(t_rel, keep = {relative_time_char}",
    if (has_ref) ", ref = {time_ref}" else "", ")",
    if (has_group) " + group_t_rel" else "",
    " | ind + t"
  ))

  fixest::feols(
    formula_model,
    data = data_renamed
    # cluster = "ind"
  )
}
