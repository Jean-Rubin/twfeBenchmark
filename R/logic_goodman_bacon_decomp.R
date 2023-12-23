did_estimate <- function(data, groups, event) {
  y_post_t <- data[data$group == groups$treated & data$t >= event, ][["y"]]
  y_post_u <- data[data$group == groups$control & data$t >= event, ][["y"]]
  y_pre_t <- data[data$group == groups$treated & data$t < event, ][["y"]]
  y_pre_u <- data[data$group == groups$control & data$t < event, ][["y"]]

  diff_post <- mean(y_post_t - y_post_u)
  diff_pre <- mean(y_pre_t - y_pre_u)

  ifelse(is.na(diff_post), 0, diff_post) - ifelse(is.na(diff_pre), 0, diff_pre)
}

goodman_bacon_decomp <- function(timeline, treated_groups, control_group) {
  nb_times <- length(timeline)
  n_tot <- treated_groups[[1]]$size +
    treated_groups[[2]]$size +
    control_group$size

  n_u <- control_group$size / n_tot
  n_1 <- treated_groups[[1]]$size / n_tot
  n_2 <- treated_groups[[2]]$size / n_tot
  n_1u <- n_1 / (n_1 + n_u)
  n_2u <- n_2 / (n_2 + n_u)
  n_12 <- n_1 / (n_1 + n_2)
  d_1 <- min_max((nb_times - treated_groups[[1]]$event) / nb_times, 0, 1)
  d_2 <- min_max((nb_times - treated_groups[[2]]$event) / nb_times, 0, 1)
  v_1u <- n_1u * (1 - n_1u) * d_1 * (1 - d_1)
  s_1u <- (n_1 + n_u)^2 * v_1u
  v_2u <- n_2u * (1 - n_2u) * d_2 * (1 - d_2)
  s_2u <- (n_2 + n_u)^2 * v_2u
  v_12_1 <- n_12 * (1 - n_12) * (d_1 - d_2) * (1 - d_1) / (1 - d_2)^2
  s_12_1 <- ((n_1 + n_2) * (1 - d_2))^2 * v_12_1
  v_12_2 <- n_12 * (1 - n_12) * d_2 * (d_1 - d_2) / d_1^2
  s_12_2 <-  ((n_1 + n_2) * d_1)^2 * v_12_2
  s_tot <- s_1u + s_2u + s_12_1 + s_12_2
  s_1u_n <- s_1u / s_tot
  s_2u_n <- s_2u / s_tot
  s_12_1_n <- s_12_1 / s_tot
  s_12_2_n <- s_12_2 / s_tot

  # n_u -> n_2
  # n_1u -> n_12
  # 0 -> d_2
  # v_1u -> v_12_1
  # s_1u -> s_12_1
  # d_2 -> 1 - d_1, d_1 -> 1 - d_2
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


remainder <- function() {
beta_1u <- reactive({
  did_estimate(
    data_event(),
    list(treated = "Treated 1", control = "Never treated"),
    treated_groups[[1]]$event()
  )
})

beta_2u <- reactive({
  did_estimate(
    data_event(),
    list(treated = "Treated 2", control = "Never treated"),
    treated_groups[[2]]$event()
  )
})

beta_12_1 <- reactive({
  did_estimate(
    data_event() |> filter(t < treated_groups[[2]]$event()),
    list(treated = "Treated 1", control = "Treated 2"),
    treated_groups[[1]]$event()
  )
})

beta_12_2 <- reactive({
  did_estimate(
    data_event() |> filter(t >= treated_groups[[1]]$event()),
    list(treated = "Treated 2", control = "Treated 1"),
    treated_groups[[2]]$event()
  )
})

pp <- function(x, ...) formatC(x, digits = 2, ...)
output$analytic_decomposition <- renderUI({
  withMathJax(HTML(
    paste0("$$\\overline{D}_1 = ", pp(d_1()), "\\quad ",
      "\\overline{D}_2 = ", pp(d_2()), "$$"),
    paste0("$$s_{1U} = ", pp(s_1u_n()), "\\quad ",
      "s_{2U} = ", pp(s_2u_n()), "$$"),
    paste0("$$s_{12}^1 = ", pp(s_12_1_n()), "\\quad ",
      "s_{12}^2 = ", pp(s_12_2_n()), "$$"),
    "$$s_{1U}\\beta_{1U} + s_{2U}\\beta_{2U} + s_{12}^1\\beta_{12}^1 + s_{12}^2\\beta_{12}^2 = \\beta$$",
    paste0(
      "$$", pp(s_1u_n()), "\\times ", pp(beta_1u()),
      " + ", pp(s_2u_n()), "\\times ", pp(beta_2u()),
      " + ", pp(s_12_1_n()), "\\times ", pp(beta_12_1()),
      " + ", pp(s_12_2_n()), "\\times ", pp(beta_12_2()),
      " = ", pp(
        s_1u_n() * beta_1u() +
        s_12_1_n() * beta_12_1() +
        s_2u_n() * beta_2u() +
        s_12_2_n() * beta_12_2()
      ),
      "$$"
    )
  ))
})
}
