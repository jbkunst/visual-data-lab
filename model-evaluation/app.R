# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(highcharter)
library(vdltheme)

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

metric_header <- function(title, metric, value) {
  tags$div(
    class = "d-flex justify-content-between align-items-center",
    tags$span(title),
    tags$small(sprintf("Test %s · %.3f", metric, value), class = "text-muted")
  )
}

log_loss_chart <- function(values) {
  pd_grid <- seq(0.005, 0.995, length.out = 200L)
  probability <- pmin(pmax(values$score, 1e-15), 1 - 1e-15)
  penalty <- -(
    values$status_bad * log(probability) +
      (1 - values$status_bad) * log(1 - probability)
  )

  highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(title = list(text = "Predicted PD"), min = 0, max = 1) |>
    hc_yAxis(title = list(text = "Individual log-loss"), min = 0) |>
    hc_tooltip(
      headerFormat = "<b>{series.name}</b><br/>",
      pointFormat = "Predicted PD: {point.x:.3f}<br/>Penalty: {point.y:.3f}"
    ) |>
    hc_plotOptions(
      line = list(animation = list(duration = 300), marker = list(enabled = FALSE)),
      scatter = list(
        animation = list(duration = 300),
        marker = list(radius = 2.5, symbol = "circle"), opacity = 0.35
      )
    ) |>
    hc_add_series(
      name = "Observed default: -log(PD)",
      data = xy_points(pd_grid, -log(pd_grid)),
      color = danger_color, lineWidth = 2, zIndex = 2
    ) |>
    hc_add_series(
      name = "Observed non-default: -log(1 - PD)",
      data = xy_points(pd_grid, -log(1 - pd_grid)),
      color = primary_color, lineWidth = 2, zIndex = 2
    ) |>
    hc_add_series(
      id = "log_loss_bad", name = "Default observations", type = "scatter",
      data = xy_points(
        values$score[values$status_bad == 1L],
        penalty[values$status_bad == 1L]
      ),
      color = danger_color, opacity = 0.35, zIndex = 3,
      showInLegend = FALSE
    ) |>
    hc_add_series(
      id = "log_loss_good", name = "Non-default observations", type = "scatter",
      data = xy_points(
        values$score[values$status_bad == 0L],
        penalty[values$status_bad == 0L]
      ),
      color = primary_color, opacity = 0.35, zIndex = 3,
      showInLegend = FALSE
    )
}

# ui ---------------------------------------------------------------------
apptheme <- theme_vdl()
primary_color <- unname(bs_get_variables(apptheme, "primary"))
danger_color <- unname(bs_get_variables(apptheme, "danger"))
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0))

options(highcharter.theme = highcharter_theme_vdl())

ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "Model Evaluation Curve",
      radioButtons(
        "model", tags$small("Model"),
        choices = stats::setNames(names(model_labels), model_labels),
        selected = "logistic"
      ),
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
      card(card_header(uiOutput("roc_title", class = "d-block w-100")),
        highchartOutput("roc_plot", height = "100%")),
      card(card_header(uiOutput("ks_title", class = "d-block w-100")),
        highchartOutput("ks_plot", height = "100%")),
      card(card_header(uiOutput("gains_title", class = "d-block w-100")),
        highchartOutput("gains_plot", height = "100%")),
      card(card_header(uiOutput("log_loss_title", class = "d-block w-100")),
        highchartOutput("log_loss_plot", height = "100%"))
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
    predictions <- evaluation_data$predictions
    predictions <- predictions[predictions$model == input$model, ]
    summary <- evaluation_data$evaluation_summary
    summary <- summary[summary$model == input$model, ][1L, ]
    default_rate <- sum(gains$positives) / sum(gains$observations)

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
      perfect_gains = xy_points(c(0, default_rate, 1), c(0, 1, 1)),
      predictions = predictions,
      summary = summary
    )
  })

  output$roc_title <- renderUI({
    metrics <- chart_data()$summary
    metric_header("ROC curve", "AUC", metrics$auc)
  })

  output$ks_title <- renderUI({
    metrics <- chart_data()$summary
    metric_header("Kolmogorov–Smirnov", "KS", metrics$ks)
  })

  output$gains_title <- renderUI({
    metrics <- chart_data()$summary
    metric_header("CAP / cumulative gains", "Gini", metrics$gini)
  })

  output$log_loss_title <- renderUI({
    metrics <- chart_data()$summary
    metric_header("Log-loss penalty", "log-loss", metrics$log_loss)
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
      ) |>
      hc_add_series(
        id = "gains_perfect", name = "Perfect", data = data$perfect_gains,
        color = "#dee2e6", dashStyle = "ShortDot",
        enableMouseTracking = FALSE
      )
  })

  output$log_loss_plot <- renderHighchart({
    data <- isolate(chart_data())
    log_loss_chart(data$predictions)
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
      hcpxy_update_series(id = "gains", data = data$gains) |>
      hcpxy_update_series(id = "gains_perfect", data = data$perfect_gains)

    probability <- pmin(pmax(data$predictions$score, 1e-15), 1 - 1e-15)
    penalty <- -(
      data$predictions$status_bad * log(probability) +
        (1 - data$predictions$status_bad) * log(1 - probability)
    )
    highchartProxy("log_loss_plot") |>
      hcpxy_update_series(
        id = "log_loss_bad",
        data = xy_points(
          data$predictions$score[data$predictions$status_bad == 1L],
          penalty[data$predictions$status_bad == 1L]
        )
      ) |>
      hcpxy_update_series(
        id = "log_loss_good",
        data = xy_points(
          data$predictions$score[data$predictions$status_bad == 0L],
          penalty[data$predictions$status_bad == 0L]
        )
      )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
