plot_data <- function(data_event, treated_events, timeline) {
  ggplot(
    data_event,
    aes(
      t,
      y,
      colour = group,
      linewidth = compute_linewidth(size),
      size = compute_size(size)
    )
  ) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = treated_events, linetype = "dotted") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = timeline) +
    scale_continuous_identity(aesthetics = "linewidth") +
    scale_size_identity() +
    coord_cartesian(xlim = c(0, 10), ylim = c(-10, 30)) +
    labs(x = "Time", y = "Outcome", colour = "Group") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 16)
    )
}

compute_linewidth <- function(size) {
  0.5 + (size - 1) / 4
}

compute_size <- function(size) {
  2 + 2 * (size - 1) / 4
}
