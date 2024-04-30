eventStudyParameters_UI <- function(id) {
  ns <- NS(id)
  rank_list_options <- sortable::sortable_options(multiDrag = TRUE)
  tagList(
    tags$style(
      ".default-sortable .rank-list {
        display: flex;
      }"
    ),
    checkboxInput(ns("balance"), "Balance group in relative time"),
    sortable::bucket_list(
      header = "Model specification:",
      group_name = ns("bucket_list_event_study"),
      orientation = "horizontal",
      sortable::add_rank_list(
        "References relative periods (or dropped if no collinearity)",
        labels = NULL,
        input_id = ns("rank_list_ref"),
        options = rank_list_options
      ),
      sortable::add_rank_list(
        "Leads and lags used in the model as is",
        labels = as.list(-4:4),
        input_id = ns("rank_list_t_rel"),
        options = rank_list_options
      ),
      sortable::add_rank_list(
        "Leads and lags trimmed from the model",
        labels = as.list(c(-9:-5, 5:9)),
        input_id = ns("rank_list_trim"),
        options = rank_list_options
      ),
      sortable::add_rank_list(
        "Group of leads and lags",
        labels = NULL,
        input_id = ns("rank_list_group"),
        options = rank_list_options
      )
    )
  )
}

eventStudyParameters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(
      t_rel = input$rank_list_t_rel,
      ref = input$rank_list_ref,
      trim = input$rank_list_trim,
      group = input$rank_list_group,
      balance = input$balance
    ))
  })
}
