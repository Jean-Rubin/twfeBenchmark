#' Insert the name of each sublist of a list inside it
#'
#' @param x A list of list.
#'
#' @return A list of list, where each sublist contains a `name` attribute
#'   corresponding to its name.
#' @export
#'
#' @examples
#' insert_name(list(a = list(aa = 1, ab = 2), b = list()))
insert_name <- function(x) {
  purrr::imap(x, \(xx, xx_name) append(list(name = xx_name), xx))
}
