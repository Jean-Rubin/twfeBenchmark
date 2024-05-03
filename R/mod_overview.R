overview_UI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Purpose of this app"),
    p(paste(
      "The goal of this app is to provide a Shiny application",
      "as an exploration tool for understanding two-way fixed effect estimations.",
      "By letting people experiment with small designs,",
      "we hope that they will have a better understanding of the recent results in the literature.",
      "Indeed, although these types of models were widely used in the literature as a causal analysis tool,",
      "the basic ones have multiple limitations that could yield to misleading results."
    )),
    p(paste(
      "You can create a basic dataset with a control group and multiple treated groups",
      "using the `Parameters` sidebar to manually tweak the size of the group, the time of treatment",
      "and the treatment shape."
    )),
    p(paste(
      "Presets are also proposed to directly build simple datasets that can expose particular phenomenons."
    ))
  )
}
