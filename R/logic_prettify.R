dictionnary_logic_to_pretty <- c(
  "control" = "Never Treated",
  "treated" = "Treated",
  "counterfactual" = "Counterfactual"
)

dictionnary_pretty_to_logic <- names(dictionnary_logic_to_pretty)
names(dictionnary_pretty_to_logic) <- dictionnary_logic_to_pretty

pp_table <- function(df) {
  df |>
    mutate(
      group = pp_col(group)
    )
}

#' Pretty relabeling of some values
#'
#' @param x A vector with names to prettify.
#'
#' @return A vector with prettified names.
#' @examples
#' pp_col(c("control_1", "treated_2_12"))
pp_col <- function(x) {
  # Separate id part with the sequence of digits
  split_x <- stringr::str_match(x, "^(?<id>.*?)(?:_)?(?<num>\\d.*)?$")
  pretty_id <- dictionnary_logic_to_pretty[split_x[, "id"]]
  pretty_num <- stringr::str_replace_all(split_x[, "num"], stringr::fixed("_"), " ")
  pretty_x <- paste0(
    stringr::str_replace_na(pretty_id, ""),
    ifelse(is.na(pretty_id) | is.na(pretty_num), "", " "),
    stringr::str_replace_na(pretty_num, "")
  )

  pretty_x
}

#' Recover the logic values associated to a pretty name
#'
#' @param x A vector with pretty names.
#'
#' @return A vector with unprettified names.
#' @examples
#' unpp_col(c("Never Treated 1", "Treated 2 12"))
unpp_col <- function(x) {
  split_x <- stringr::str_match(x, "^(?<ppid>.*?)(?: )?(?<num>\\d.*)?$")
  unpretty_id <- dictionnary_pretty_to_logic[split_x[, "ppid"]]
  unpretty_num <- stringr::str_replace_all(split_x[, "num"], stringr::fixed(" "), "_")
  unpretty_x <- paste0(
    stringr::str_replace_na(unpretty_id, ""),
    ifelse(is.na(unpretty_id) | is.na(unpretty_num), "", "_"),
    stringr::str_replace_na(unpretty_num, "")
  )

  unpretty_x
}

