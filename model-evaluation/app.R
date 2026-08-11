# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(highcharter)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("credit-evaluation.rds")) "." else "model-evaluation"
evaluation_data <- readRDS(file.path(app_dir, "credit-evaluation.rds"))
model_labels <- evaluation_data$metadata$model_labels

xy_points <- function(x, y) list_parse2(data.frame(x, y))

line_chart <- function(x_title, y_title) {
  highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(title = list(text = x_title), min = 0) |>
    hc_yAxis(title = list(text = y_title), min = 0) |>
    hc_tooltip(shared = TRUE, valueDecimals = 3) |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), lineWidth = 2,
      marker = list(enabled = FALSE)
    ))
}

# ui ---------------------------------------------------------------------
apptheme <- bs_theme()
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0))

options(
  highcharter.theme = hc_theme(
    chart = list(style = list(fontFamily = "system-ui")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    colors = unname(bs_get_variables(
      apptheme,
      c("primary", "danger", "warning", "success", "info", "secondary")
    )),
    tooltip = list(valueDecimals = 3, shared = TRUE,
      style = list(fontWeight = "normal")),
    xAxis = list(
      gridLineWidth = 1,
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    ),
    yAxis = list(
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    )
  )
)

ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "Model Evaluation",
      radioButtons(
        "model", tags$small("Model"),
        choices = stats::setNames(names(model_labels), model_labels),
        selected = "logistic"
      ),
      tags$small("Test metrics", class = "text-muted"),
      uiOutput("model_metrics"),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = c(6, 6, 6, 6), row_heights = c(1, 1), gap = "0.75rem",
      card(card_header("ROC curve"),
        highchartOutput("roc_plot", height = "100%")),
      card(card_header("Kolmogorov–Smirnov (KS)"),
        highchartOutput("ks_plot", height = "100%")),
      card(card_header("Cumulative gains"),
        highchartOutput("gains_plot", height = "100%")),
      card(card_header("Cumulative lift"),
        highchartOutput("lift_plot", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  chart_data <- reactive({
    threshold_all <- evaluation_data$threshold_curve
    threshold_all <- threshold_all[threshold_all$model == input$model, ]
    threshold <- threshold_all[is.finite(threshold_all$threshold), ]
    gains <- evaluation_data$gains_curve
    gains <- gains[gains$model == input$model, ]
    summary <- evaluation_data$evaluation_summary
    summary <- summary[summary$model == input$model, ][1L, ]

    list(
      roc = xy_points(
        threshold_all$false_positive_rate,
        threshold_all$true_positive_rate
      ),
      ks_tpr = xy_points(threshold$threshold, threshold$true_positive_rate),
      ks_fpr = xy_points(threshold$threshold, threshold$false_positive_rate),
      ks_gap = xy_points(threshold$threshold, threshold$ks_gap),
      ks_threshold = summary$ks_threshold,
      gains = xy_points(gains$population_fraction, gains$positive_fraction),
      lift = xy_points(
        gains$population_fraction[is.finite(gains$lift)],
        gains$lift[is.finite(gains$lift)]
      ),
      summary = summary
    )
  })

  output$model_metrics <- renderUI({
    metrics <- chart_data()$summary
    labels <- c("Log-loss", "AUC", "Gini", "KS")
    values <- sprintf("%.3f", unlist(metrics[c("log_loss", "auc", "gini", "ks")]))

    div(
      class = "border rounded p-2",
      div(
        class = "row g-2",
        lapply(seq_along(labels), function(i) {
          div(
            class = "col-6",
            tags$small(labels[[i]], class = "text-muted"),
            div(values[[i]])
          )
        })
      )
    )
  })

  output$roc_plot <- renderHighchart({
    data <- isolate(chart_data())
    line_chart("False-positive rate", "True-positive rate") |>
      hc_xAxis(max = 1) |>
      hc_yAxis(max = 1) |>
      hc_add_series(
        id = "roc", name = "Model", data = data$roc,
        lineWidth = 3
      ) |>
      hc_add_series(
        id = "roc_reference", name = "Random", data = xy_points(c(0, 1), c(0, 1)),
        color = "#adb5bd", dashStyle = "ShortDash", enableMouseTracking = FALSE
      )
  })

  output$ks_plot <- renderHighchart({
    data <- isolate(chart_data())
    line_chart("Score threshold", "Rate") |>
      hc_xAxis(
        max = 1,
        plotLines = list(list(
          value = data$ks_threshold, color = "#adb5bd", width = 1,
          dashStyle = "ShortDash", zIndex = 3
        ))
      ) |>
      hc_yAxis(max = 1) |>
      hc_add_series(
        id = "ks_tpr", name = "TPR", data = data$ks_tpr
      ) |>
      hc_add_series(
        id = "ks_fpr", name = "FPR", data = data$ks_fpr
      ) |>
      hc_add_series(
        id = "ks_gap", name = "KS gap", data = data$ks_gap,
        dashStyle = "ShortDot"
      )
  })

  output$gains_plot <- renderHighchart({
    data <- isolate(chart_data())
    line_chart("Population targeted", "Defaults captured") |>
      hc_xAxis(max = 1) |>
      hc_yAxis(max = 1) |>
      hc_add_series(
        id = "gains", name = "Model", data = data$gains,
        lineWidth = 3
      ) |>
      hc_add_series(
        id = "gains_reference", name = "Random",
        data = xy_points(c(0, 1), c(0, 1)), color = "#adb5bd",
        dashStyle = "ShortDash", enableMouseTracking = FALSE
      )
  })

  output$lift_plot <- renderHighchart({
    data <- isolate(chart_data())
    line_chart("Population targeted", "Lift") |>
      hc_yAxis(plotLines = list(list(
        value = 1, color = "#adb5bd", width = 1,
        dashStyle = "ShortDash", zIndex = 3
      ))) |>
      hc_add_series(
        id = "lift", name = "Model", data = data$lift,
        lineWidth = 3
      )
  })

  observeEvent(input$model, {
    data <- chart_data()

    highchartProxy("roc_plot") |>
      hcpxy_update_series(id = "roc", data = data$roc)
    highchartProxy("ks_plot") |>
      hcpxy_update(xAxis = list(plotLines = list(list(
        value = data$ks_threshold, color = "#adb5bd", width = 1,
        dashStyle = "ShortDash", zIndex = 3
      )))) |>
      hcpxy_update_series(id = "ks_tpr", data = data$ks_tpr) |>
      hcpxy_update_series(id = "ks_fpr", data = data$ks_fpr) |>
      hcpxy_update_series(id = "ks_gap", data = data$ks_gap)
    highchartProxy("gains_plot") |>
      hcpxy_update_series(id = "gains", data = data$gains)
    highchartProxy("lift_plot") |>
      hcpxy_update_series(id = "lift", data = data$lift)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
