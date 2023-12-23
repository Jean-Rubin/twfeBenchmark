#' Clamping values between two bounds
#'
#' @param x A vector to clamp.
#' @param mini Lower bound.
#' @param maxi Upper bound.
#'
#' @return A vector whose values are between the two bounds.
#' @examples
#' min_max(c(-5, 1, 2, 10), 0, 5)
min_max <- function(x, mini = 0, maxi = 1) {
  pmin.int(0, pmax.int(1, x))
}
