generate_group <- function(
  name,
  timeline,
  event,
  common_trend,
  size = 1,
  base_gap = 0,
  permanent_effect = 0,
  ponctual_effect = 0,
  slope_effect = 0
) {
  if (is.null(event)) return(tibble())

  tibble(
    group = name,
    event = event,
    t = timeline,
    y = common_trend + base_gap +
      slope_effect * pmax.int(t - event, 0) +
      permanent_effect * (t >= event) +
      ponctual_effect * (t == event),
    treated = as.numeric(t >= event),
    size = size
  )
}

generate_data_event <- function(
  control_group,
  treated_groups,
  timeline
) {
  bind_rows(
    generate_group(
      name = "control",
      timeline = timeline,
      event = +Inf,
      size = control_group$size,
      common_trend = control_group$common_trend,
      base_gap = 0,
      permanent_effect = 0,
      ponctual_effect = 0,
      slope_effect = 0
    ),
    purrr::imap(treated_groups, function(treated_group, i) {
      generate_group(
        name = paste("treated", i, sep = "_"),
        timeline = timeline,
        event = treated_group$event,
        size = treated_group$size,
        common_trend = control_group$common_trend,
        base_gap = treated_group$base_gap,
        permanent_effect = treated_group$permanent_effect,
        ponctual_effect = treated_group$ponctual_effect,
        slope_effect = treated_group$slope_effect
      )
    })
  )
}
