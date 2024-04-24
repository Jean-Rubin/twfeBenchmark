#' Produce a table with de Chaisemartin and D'Haultfoeuille weights
#'
#' @param data A table with:
#'  - `group`: id of the group (Primary key 1).
#'  - `t`: time (Primary key 2).
#'  - `size`: size of the group.
#'  - `treated`: treatment value.
#'
#' @return A table with the same rows as `data` and with:
#'  - `group`: (Primary key 1).
#'  - `t`: (Primary key 2).
#'  - `treated_group`: mean treatment of group.
#'  - `treated_t`: mean treatment at time `t`.
#'  - `treated_mean`: global mean treatment.
#'  - `eps`: residual of double demeaning.
#'  - `w`: weight in the de Chaisemartin and D'Haultfoeuille decomposition.
#'
#' @examples
#' data_ex <- data.frame(
#'   group = rep(c("control", "treated_1"), each = 10),
#'   t = c(1:10, 1:10),
#'   size = rep(c(1, 1), each = 10),
#'   treated = c(rep(0, 10), c(rep(0, 5), rep(1, 5)))
#' )
#' compute_dcdh_weights(data_ex)
compute_dcdh_weights <- function(data) {
  data |>
    group_by(group) |>
    mutate(treated_group = mean(treated)) |>
    ungroup() |>
    group_by(t) |>
    mutate(treated_t = mean(treated)) |>
    ungroup() |>
    mutate(treated_mean = mean(treated)) |>
    mutate(
      eps = treated - treated_group - treated_t + treated_mean,
      w = size * eps
    ) |>
    mutate(w = ifelse(treated == 1, w / sum(w * treated), NA)) |>
    select(group, t, treated_group, treated_t, treated_mean, eps, w)
}
