generate_group <- function(
  name,
  timeline,
  event,
  common_trend,
  group_size = 1,
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
    group_size = group_size
  )
}
