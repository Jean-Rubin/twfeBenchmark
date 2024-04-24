eventStudyParameters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      ".default-sortable .rank-list {
        display: flex;
      }"
    ),
    sortable::bucket_list(
      header = "Model specification:",
      group_name = ns("bucket_list_event_study"),
      orientation = "horizontal",
      sortable::add_rank_list(
        "Leads and lags trimmed from the model",
        labels = as.list(c(-9:-5, 5:9)),
        input_id = ns("rank_list_trim"),
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        "Leads and lags used in the model as is",
        labels = as.list(-4:4),
        input_id = ns("rank_list_t_rel"),
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        "Group of leads and lags",
        labels = NULL,
        input_id = ns("rank_list_group"),
        options = sortable::sortable_options(multiDrag = TRUE)
      )
    )
  )
}

eventStudyParameters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(
      trim = input$rank_list_trim,
      t_rel = input$rank_list_t_rel,
      group = input$rank_list_group
    ))
  })
}
