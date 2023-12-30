generate_group <- function(
  name,
  size,
  event,
  timeline,
  common_trend,
  y
) {
  if (is.null(event)) return(tibble())

  tibble(
    group = name,
    size = size,
    event = event,
    t = timeline,
    treated = as.numeric(t >= event),
    y = common_trend + y(t, event = event)
  )
}

generate_data_event <- function(
  control_group,
  treated_groups,
  timeline
) {
  common_trend <- control_group$y(timeline)

  bind_rows(
    generate_group(
      name = "control",
      size = control_group$size,
      event = control_group$event,
      timeline = timeline,
      common_trend = 0,
      y = control_group$y
    ),
    purrr::imap(treated_groups, function(treated_group, i) {
      generate_group(
        name = paste("treated", i, sep = "_"),
        size = treated_group$size,
        event = treated_group$event,
        timeline = timeline,
        common_trend = common_trend,
        y = treated_group$y
      )
    })
  )
}
