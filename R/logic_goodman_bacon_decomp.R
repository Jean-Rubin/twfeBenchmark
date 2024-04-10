#' Basic Difference in Difference estimation around an event
#'
#' @param data Table containing:
#'  - `group`: name of the group.
#'  - `t`: date.
#'  - `y`: outcome.
#' @param group_control Name of the `control` group.
#' @param group_treated Name of the `treated` group.
#' @param event Date of the treatment event.
#'
#' @return The Difference in Difference estimation of the treatment effect.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   t = c(1:10, 1:10),
#'   group = rep(c("group_1", "group_2"), each = 10),
#'   y = c(1:10, c(1:4, 6:11))
#' )
#' did_estimate(data, "group_1", "group_2", 5)
did_estimate <- function(data, group_control, group_treated, event) {
  y_post_t <- data[data$group == group_treated & data$t >= event, ][["y"]]
  y_post_u <- data[data$group == group_control & data$t >= event, ][["y"]]
  y_pre_t <- data[data$group == group_treated & data$t < event, ][["y"]]
  y_pre_u <- data[data$group == group_control & data$t < event, ][["y"]]

  diff_post <- mean(y_post_t - y_post_u)
  diff_pre <- mean(y_pre_t - y_pre_u)

  if (is.na(diff_post) || is.na(diff_pre)) return(0)

  diff_post - diff_pre
}

#' Goodman-Bacon Difference in Difference weights
#'
#' @param nb_times Number of time observations.
#' @param group_1 List corresponding to the first group and containing:
#'  - `size`: size of the group.
#'  - `event`: time of the treatment event (eventually infinite for a control).
#' @param group_2 List corresponding to the second group and containing:
#'  - `size`: size of the group.
#'  - `event`: time of the treatment event (eventually infinite for a control).
#'
#' @return A list containing the non-normalized DiD weights in the Goodman-Bacon
#'   decomposition for the DiD estimators:
#'  - `s_12_1`: using `group_1` as the treated and `group_2` as the control.
#'  - `s_12_2`: using `group_2` as the treated and `group_1` as the control.
#' @export
#'
#' @examples
#' goodman_bacon_coef(11, list(size = 5, event = 6), list(size = 1, event = 3))
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
    group_control = group_2$name,
    group_treated = group_1$name,
    group_1$event
  )

  beta_12_2 <- did_estimate(
    data_event |> filter(!comparison(t, group_1$event)),
    group_control = group_1$name,
    group_treated = group_2$name,
    group_2$event
  )

  list(
    beta_12_1 = beta_12_1,
    beta_12_2 = beta_12_2
  )
}
