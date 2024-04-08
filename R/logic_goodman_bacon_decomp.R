did_estimate <- function(data, groups, event) {
  y_post_t <- data[data$group == groups$treated & data$t >= event, ][["y"]]
  y_post_u <- data[data$group == groups$control & data$t >= event, ][["y"]]
  y_pre_t <- data[data$group == groups$treated & data$t < event, ][["y"]]
  y_pre_u <- data[data$group == groups$control & data$t < event, ][["y"]]

  diff_post <- mean(y_post_t - y_post_u)
  diff_pre <- mean(y_pre_t - y_pre_u)

  if (is.na(diff_post) || is.na(diff_pre)) return(0)

  diff_post - diff_pre
}

goodman_bacon_coef <- function(nb_times, group_1, group_2) {
  n_1 <- group_1$size
  n_2 <- group_2$size
  n_12 <- n_1 / (n_1 + n_2)
  d_1 <- min_max((nb_times - group_1$event) / nb_times, 0, 1)
  d_2 <- min_max((nb_times - group_2$event) / nb_times, 0, 1)

  # Switch to make d_1 > d_2
  if (d_1 < d_2) {
    d_1 <- 1 - d_1
    d_2 <- 1 - d_2
  }
  v_12_1 <- n_12 * (1 - n_12) * (d_1 - d_2) * (1 - d_1) / (1 - d_2)^2
  v_12_2 <- n_12 * (1 - n_12) * d_2 * (d_1 - d_2) / d_1^2

  s_12_1 <- ((n_1 + n_2) * (1 - d_2))^2 * v_12_1
  s_12_2 <-  ((n_1 + n_2) * d_1)^2 * v_12_2

  list(
    s_12_1 = s_12_1,
    s_12_2 = s_12_2
  )
}

did_estimates <- function(data_event, group_1, group_2) {
  # Formula assumes that group_1$event < group_2$event
  comparison <- if (group_1$event < group_2$event) `<` else `>=`

  beta_12_1 <- did_estimate(
    data_event |> filter(comparison(t, group_2$event)),
    list(treated = group_1$name, control = group_2$name),
    group_1$event
  )

  beta_12_2 <- did_estimate(
    data_event |> filter(!comparison(t, group_1$event)),
    list(treated = group_2$name, control = group_1$name),
    group_2$event
  )

  list(
    beta_12_1 = beta_12_1,
    beta_12_2 = beta_12_2
  )
}
