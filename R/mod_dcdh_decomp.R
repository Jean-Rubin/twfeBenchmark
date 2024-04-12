dCdHDecomp_UI <- function(id) {
  ns <- NS(id)

  echarts4r::echarts4rOutput(ns("w_plot"))
}

dCdHDecomp_Server <- function(id, data_event) {
  moduleServer(id, function(input, output, session) {
    dcdh_w_df <- reactive({
      compute_dcdh_weights(data_event())
    })

    output$w_plot <- echarts4r::renderEcharts4r({
      dcdh_w_df() |>
        mutate(
          group = pp_col(group),
          w = round(100 * w) / 100,
          t = factor(t, levels = session$userData$timeline),
          metaData = paste(treated_group, treated_t, treated_mean, eps, sep = ",")
        ) |>
        echarts4r::e_charts(t) |>
        echarts4r::e_heatmap(
          group, w,
          bind = metaData,
          itemStyle = list(emphasis = list(shadowBlur = 10)),
          label = list(show = TRUE)
        ) |>
        echarts4r::e_visual_map(w, precision = 2, left = "right", bottom = "50%") |>
        echarts4r::e_tooltip(
          formatter = htmlwidgets::JS(
            "function(params) {
              let formatNum = (value) => Number.parseFloat(value).toFixed(2);
              let vals = params.name.split(',');
              return('<strong>' + params.value[1] + '</strong>' +
                     ', <strong>t</strong> = ' + params.value[0] +
                     '<br/>' + params.marker + '<strong>Weight</strong> = ' + formatNum(params.value[2]) +
                     '<br/>' +
                     '<br/>+<strong>Global average treatment</strong> = ' + formatNum(vals[2]) +
                     '<br/>-<strong>Average Treatment of group</strong> = ' + formatNum(-vals[0]) +
                     '<br/>-<strong>Average Treatment of time</strong> = ' + formatNum(-vals[1]) +
                     '<br/><strong>Residual</strong> = ' + formatNum(vals[3])
                    )
            }"
          )
        ) |>
        echarts4r::e_title(
          "Weights of the Treatment Effects",
          "in de Chaisemartin, d'Haultfoeuille decomposition"
        ) |>
        echarts4r::e_x_axis(splitArea = list(show = TRUE)) |>
        echarts4r::e_y_axis(splitArea = list(show = TRUE)) |>
        echarts4r::e_axis_labels(x = "Time") |>
        echarts4r::e_show_loading()
    })
  })
}
